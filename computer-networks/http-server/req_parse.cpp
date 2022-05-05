#include "req_parse.h"
#include "server_response.h"
#include <regex>
#include "err.h"

std::unordered_map<std::string, std::string> parse_request_line(const std::string &request_line) {
    std::unordered_map<std::string, std::string> parsed_request;
    size_t left_bound = 0, right_bound = request_line.find(' ');
    if (right_bound == std::string::npos) {
        parsed_request["error"] = "400";
        return parsed_request;
    }

    try {
        std::string method = request_line.substr(left_bound, right_bound - left_bound);
        if (method != "GET" && method != "HEAD") {
            parsed_request["error"] = "501";
            return parsed_request;
        }

        if (request_line[right_bound + 1] == ' ') {
            parsed_request["error"] = "400";
            return parsed_request;
        }

        left_bound = right_bound + 1;
        right_bound = request_line.find(' ', left_bound);
        if (right_bound == std::string::npos) {
            parsed_request["error"] = "400";
            return parsed_request;
        }

        std::string target = request_line.substr(left_bound, right_bound - left_bound);
        if (!std::regex_match(target, std::regex("[a-zA-Z0-9.\\-/]+"))) {
            parsed_request["error"] = "404";
            return parsed_request;
        }

        if (request_line.substr(right_bound + 1) != "HTTP/1.1\r\n") {
            parsed_request["error"] = "400";
            return parsed_request;
        }

        parsed_request["method"] = method;
        parsed_request["target"] = target;
        return parsed_request;
    }
    catch (...) {
        parsed_request["error"] = "500";
        return parsed_request;
    }
}

std::string read_line_from_sock(FILE *sock_fstream) {
    const size_t MAX_RESOURCE_SIZE = 1 << 13; // max resource/header value size
    const size_t MAX_LINE_SIZE = MAX_RESOURCE_SIZE * 2; // multiplied by 2 for certainty

    size_t buffer_size = MAX_LINE_SIZE;
    char *buffer = (char*)malloc(buffer_size * sizeof(char));
    if (!buffer) {
        return "500";
    }

    size_t read_bytes_total = 0;
    ssize_t read_bytes;
    std::string res_line, read_str;
    memset(buffer, 0, buffer_size * sizeof(char));
    errno = 0;
    while ((read_bytes = getline(&buffer, &buffer_size, sock_fstream)) >= 0) {
        read_bytes_total += read_bytes;
        if (read_bytes_total > MAX_LINE_SIZE) {
            free(buffer);
            return "400";
        }

        try {
            read_str = std::string(buffer, read_bytes);
            res_line.append(read_str);
        }
        catch (...) {
            free(buffer);
            return "500";
        }

        if (read_str.find("\r\n") != std::string::npos) {
            break;
        }

        memset(buffer, 0, buffer_size * sizeof(char));
        errno = 0;
    }
    free(buffer);

    if (read_bytes < 0) {
        if (errno != 0) {
            return "500";
        }
        else {
            return "connection closed";
        }
    }

    return res_line;
}

size_t skip_spaces(const std::string &str, size_t i) {
    while (str[i] == ' ')
        i++;
    return i;
}

int parse_header_line(const std::string &header_line, std::unordered_map<std::string, std::string> &parsed_headers) {
    size_t left_bound = 0, right_bound = header_line.find(':');
    if (right_bound == std::string::npos) {
        parsed_headers["error"] = "400";
        return -1;
    }

    try {
        std::string field_name = header_line.substr(left_bound, right_bound - left_bound);
        if (field_name.empty() || field_name.find(' ') != std::string::npos) {
            parsed_headers["error"] = "400";
            return -1;
        }
        std::transform(field_name.begin(), field_name.end(), field_name.begin(),
                       [](unsigned char c){ return std::tolower(c); });

        left_bound = skip_spaces(header_line, right_bound + 1);

        std::string field_value;
        if (header_line[left_bound] != '\r' || header_line[left_bound + 1] != '\n') {
            right_bound = header_line.find(' ', left_bound);
            if (right_bound == std::string::npos) {
                right_bound = header_line.find("\r\n", left_bound);
            }
            field_value = header_line.substr(left_bound, right_bound - left_bound);
        }

        right_bound = skip_spaces(header_line, right_bound);

        if (header_line[right_bound] != '\r' || header_line[right_bound + 1] != '\n') {
            parsed_headers["error"] = "400";
            return -1;
        }

        if (field_name == "connection") {
            if (parsed_headers.find(field_name) != parsed_headers.end() || field_value.empty()) {
                parsed_headers["error"] = "400";
                return -1;
            }

            parsed_headers[field_name] = field_value;
        }
        else if (field_name == "content-length") {
            if (parsed_headers.find(field_name) != parsed_headers.end() || field_value != "0") {
                parsed_headers["error"] = "400";
                return -1;
            }

            parsed_headers[field_name] = field_value;
        }

        return 0;
    }
    catch (...) {
        parsed_headers["error"] = "500";
        return -1;
    }
}

std::unordered_map<std::string, std::string> read_and_parse_headers(FILE *sock_fd) {
    std::unordered_map<std::string, std::string> parsed_headers;
    bool reading_headers = true;
    std::string header_line;
    while (reading_headers) {
        header_line = read_line_from_sock(sock_fd);
        if (header_line == "400" || header_line == "500" || header_line == "connection closed") {
            parsed_headers["error"] = header_line;
            return parsed_headers;
        }

        if (header_line == "\r\n") {
            reading_headers = false;
        }
        else {
            if (parse_header_line(header_line, parsed_headers) == -1)
                return parsed_headers;
        }
    }

    return parsed_headers;
}

int parsing_error(std::unordered_map<std::string, std::string> &parsed_input, int sock, FILE *sock_fstream) {
    if (parsed_input.find("error") != parsed_input.end() && parsed_input["error"] != "404" &&
            parsed_input["error"] != "501") {
        std::string parsing_err = parsed_input["error"];
        if (parsing_err == "400") {
            bad_request_response(sock);
        }
        else if (parsing_err == "500") {
            internal_server_error_response(sock);
        }

        if (fclose(sock_fstream) == EOF)
            syserr("fclose");
        return 1;
    }

    return 0;
}

#include "server_util.h"
#include <regex>
#include <netinet/in.h>
#include <fstream>
#include <dirent.h>
#include "err.h"

int convert_port_num(char *port) {
    const int RESTRICTED_PORTS = 1023;

    int port_num = 0;
    std::string port_str(port);
    if (!std::regex_match(port_str, std::regex("[1-9][0-9]*")))
        fatal("wrong port syntax");

    try {
        port_num = std::stoi(port_str);
    }
    catch (...) {
        syserr("stoi");
    }

    if (port_num <= RESTRICTED_PORTS || port_num > UINT16_MAX)
        fatal("wrong port number");

    return port_num;
}

void bind_server_sock(int server_sock, int port_num) {
    struct sockaddr_in server_address = {};
    server_address.sin_family = AF_INET;
    server_address.sin_addr.s_addr = htonl(INADDR_ANY);
    server_address.sin_port = htons(port_num);

    if (bind(server_sock, (struct sockaddr *) &server_address, sizeof(server_address)) < 0)
        syserr("bind");
}

int start_server(int port_num) {
    const int QUEUE_LENGTH = 128;

    int server_sock = socket(PF_INET, SOCK_STREAM, 0);
    if (server_sock < 0)
        syserr("socket");

    bind_server_sock(server_sock, port_num);

    if (listen(server_sock, QUEUE_LENGTH) < 0)
        syserr("listen");

    return server_sock;
}

std::string find_resource_in_correlated_servers(std::string &resource, std::string &correlated_servers_path) {
    std::ifstream file(correlated_servers_path);
    std::string str;
    size_t left_bound, right_bound;
    try {
        while (std::getline(file, str)) {
            right_bound = str.find(9);
            std::string current_resource = str.substr(0, right_bound);
            if (resource == current_resource) {
                left_bound = right_bound + 1;
                right_bound = str.find(9, left_bound);
                std::string server = str.substr(left_bound, right_bound - left_bound);
                left_bound = right_bound + 1;
                std::string port = str.substr(left_bound);
                std::string res("http://");
                res = res.append(server);
                res += ":";
                res += port;
                res += resource;
                return res;
            }
        }

        return "404";
    }
    catch (...) {
        return "500";
    }
}

bool inside_dir(fs::path &dir, fs::path &file) {
    auto file_it = file.begin();
    for (const auto &dir_it : dir) {
        if (file_it == file.end() || dir_it != *file_it) {
            return false;
        }

        file_it++;
    }

    return true;
}

// -2 - 400, -1 - 500, 0 - szukaj dalej, 1 - gituwa, -3 - 404
std::string check_resource(std::string resource, fs::path &server_resources_dir) {
    if (resource[0] != '/') {
        return "400";
    }

    try {
        fs::path resource_canonical_path = fs::weakly_canonical(server_resources_dir.string().append(resource));
        if (!inside_dir(server_resources_dir, resource_canonical_path)) {
            return "404";
        }

        if (!fs::is_regular_file(resource_canonical_path)) {
            return "correlated";
        }
        std::ifstream resource_stream(resource_canonical_path.c_str());
        if (resource_stream.is_open()) {
            return "200";
        }
        else {
            return "correlated";
        }
    }
    catch (const fs::filesystem_error &e) {
        return "404";
    }
    catch (...) {
        return "500";
    }
}

fs::path get_resources_dir(char *resources_dir_str_path) {
    fs::path resources_dir;
    try {
        resources_dir = fs::canonical(fs::path(resources_dir_str_path));
    }
    catch (const std::bad_alloc &e) {
        syserr(e.what());
    }
    catch (...) {
        fatal("bad resource dir path");
    }

    DIR *dir_stream = opendir(resources_dir.c_str());
    if (!dir_stream) {
        fatal("bad resource dir path");
    }
    if (closedir(dir_stream) == -1)
        syserr("closedir");

    return resources_dir;
}

fs::path get_correlated_servers(char *correlated_servers_f_str_path) {
    fs::path correlated_servers;
    try {
        correlated_servers = fs::canonical(fs::path(correlated_servers_f_str_path));
    }
    catch (const std::bad_alloc &e) {
        syserr(e.what());
    }
    catch (...) {
        fatal("bad correlated servers file path");
    }

    errno = 0;
    FILE *f_stream = fopen(correlated_servers.c_str(), "r");
    if (!f_stream) {
        if (errno == EACCES) {
            fatal("bad correlated servers file path");
        }
        syserr("fopen");
    }

    fclose(f_stream);
    return correlated_servers;
}

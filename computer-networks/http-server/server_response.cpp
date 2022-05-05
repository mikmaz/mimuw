#include "server_response.h"
#include <unistd.h>
#include <fstream>
#include <cstring>

int write_msg_to_sock(const char *msg, int sock) {
    size_t msg_len = strlen(msg);
    size_t written_bytes = 0;
    ssize_t current_write;
    while (written_bytes != msg_len) {
        if ((current_write = write(sock, msg + written_bytes, msg_len - written_bytes)) == -1)
            return -1;
        written_bytes += current_write;
    }

    return 0;
}

int write_msg_to_sock_w_len(const char *msg, size_t msg_len, int sock) {
    size_t written_bytes = 0;
    ssize_t current_write;
    while (written_bytes != msg_len) {
        if ((current_write = write(sock, msg + written_bytes, msg_len - written_bytes)) == -1)
            return -1;
        written_bytes += current_write;
    }

    return 0;
}

int bad_request_response(int sock) {
    char msg[] = "HTTP/1.1 400 Bad Request\r\nConnection: close\r\nContent-Length: 0\r\n\r\n";
    return write_msg_to_sock(msg, sock);
}

int not_found_response(int sock, bool close_connection) {
    char msg[] = "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n";
    char msg_close_connection[] = "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
    return close_connection ? write_msg_to_sock(msg_close_connection, sock) : write_msg_to_sock(msg, sock);
}

int internal_server_error_response(int sock) {
    char msg[] = "HTTP/1.1 500 Internal Server Error\r\nConnection: close\r\nContent-Length: 0\r\n\r\n";
    return write_msg_to_sock(msg, sock);
}

int not_implemented_response(int sock, bool close_connection) {
    char msg[] = "HTTP/1.1 501 Not Implemented\r\nContent-Length: 0\r\n\r\n";
    char msg_close_connection[] = "HTTP/1.1 501 Not Implemented\r\nConnection: close\r\nContent-Length: 0\r\n\r\n";
    return close_connection ? write_msg_to_sock(msg_close_connection, sock) : write_msg_to_sock(msg, sock);
}

int ok_response(int sock, std::string &resource_path, std::string &method,  bool close_connection) {
    try {
        std::string msg;
        std::ifstream f_stream(resource_path);
        std::string file_content((std::istreambuf_iterator<char>(f_stream)), std::istreambuf_iterator<char>());
        msg = "HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\nContent-Length: ";
        msg += std::to_string(file_content.length());
        msg += "\r\n";
        if (close_connection)
            msg += "Connection: close\r\n";
        msg += "\r\n";
        if (method == "GET") {
            msg += file_content;
        }

        size_t msg_len = msg.length();
        const char *msg_c_str = msg.c_str();
        return write_msg_to_sock_w_len(msg_c_str, msg_len, sock);
    }
    catch (...) {
        return 1;
    }
}

int send_correlated_server(int sock, std::string &correlated_server_address, bool close_connection) {
    try {
        std::string msg = "HTTP/1.1 302 Found\r\nContent-Length: 0\r\nLocation: ";
        msg += correlated_server_address;
        if (close_connection)
            msg += "\r\nConnection: close";
        msg += "\r\n\r\n";
        size_t msg_len = msg.length();
        const char *msg_c_str = msg.c_str();
        return write_msg_to_sock_w_len(msg_c_str, msg_len, sock);
    }
    catch (...) {
        return 1;
    }
}

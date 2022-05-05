/**
 * Main server file.
 *
 * Author:   Mikołaj Mazurczyk
 * Index no: 426819
 */

#include <sys/socket.h>
#include <netinet/in.h>
#include <string>
#include <regex>
#include <cstdio>
#include <unistd.h>
#include <unordered_map>
#include <filesystem>
#include <csignal>
#include "req_parse.h"
#include "server_response.h"
#include "server_util.h"
#include "err.h"


void check_argc (int argc) {
    if (argc < 3 || argc > 4) {
        fatal("wrong number of arguments");
    }
}

int main(int argc, char *argv[]) {
    const int DEFAULT_PORT_NUM = 8080;
    signal(SIGPIPE, SIG_IGN);

    check_argc(argc);
    fs::path resources_dir = get_resources_dir(argv[1]);
    fs::path correlated_servers = get_correlated_servers(argv[2]);
    int port_num = argc == 4 ? convert_port_num(argv[3]) : DEFAULT_PORT_NUM;

    int server_sock = start_server(port_num);

    std::unordered_map<std::string, std::string> parsed_request, parsed_headers;
    fs::path resource_canonical_path;
    std::string request_line, resource_status, resource_http_address, correlated_servers_str_path, resource_str_path;
    FILE *connection_sock_fstream;
    struct sockaddr_in client_address = {};
    socklen_t client_address_len;
    int connection_sock, c;
    bool connected, connection_close;
    while (true) {
        client_address_len = sizeof(client_address);
        connection_sock = accept(server_sock, (struct sockaddr *) &client_address, &client_address_len);
        if (connection_sock < 0)
            syserr("accept");

        connected = true;
        if (!(connection_sock_fstream = fdopen(connection_sock, "r"))) {
            internal_server_error_response(connection_sock);
            close(connection_sock);
            connected = false;
        }
        while (connected) {
            request_line = read_line_from_sock(connection_sock_fstream);
            if (request_line == "400" || request_line == "500" || request_line == "connection closed") {
                parsed_request.clear();
                parsed_request["error"] = request_line;
            }
            else {
                parsed_request = parse_request_line(request_line);
            }

            if (parsing_error(parsed_request, connection_sock, connection_sock_fstream)) {
                connected = false;
            }
            else {
                parsed_headers = read_and_parse_headers(connection_sock_fstream);
                if (parsing_error(parsed_headers, connection_sock, connection_sock_fstream)) {
                    connected = false;
                }
            }

            if (parsed_request.find("error") != parsed_request.end() && (parsed_request["error"] == "404" ||
                    parsed_request["error"] == "501")) {
                connection_close = parsed_headers.find("connection") != parsed_headers.end() &&
                        parsed_headers["connection"] == "close";
                if ((parsed_request["error"] == "404" ?
                        not_found_response(connection_sock, connection_close) :
                        not_implemented_response(connection_sock, connection_close)) == -1 ||
                        connection_close) {
                    fclose(connection_sock_fstream);
                    connected = false;
                }
            }
            else if (connected) {
                resource_status = check_resource(parsed_request["target"], resources_dir);
                if (resource_status == "400") {
                    bad_request_response(connection_sock);
                    fclose(connection_sock_fstream);
                    connected = false;
                }
                else if (resource_status == "500") {
                    internal_server_error_response(connection_sock);
                    fclose(connection_sock_fstream);
                    connected = false;
                }
                else if (resource_status == "correlated") {
                    correlated_servers_str_path = correlated_servers.string();
                    resource_http_address = find_resource_in_correlated_servers(parsed_request["target"],
                                                                                correlated_servers_str_path);
                    if (resource_http_address == "500") {
                        internal_server_error_response(connection_sock);
                        fclose(connection_sock_fstream);
                        connected = false;
                    }
                    else if (resource_http_address == "404") {
                        connection_close = parsed_headers.find("connection") != parsed_headers.end() &&
                                           parsed_headers["connection"] == "close";
                        if (not_found_response(connection_sock, connection_close) == -1 || connection_close) {
                            fclose(connection_sock_fstream);
                            connected = false;
                        }
                    }
                    else {
                        connection_close = parsed_headers.find("connection") != parsed_headers.end() &&
                                parsed_headers["connection"] == "close";
                        c = send_correlated_server(connection_sock, resource_http_address, connection_close);
                        if (c == 1) {
                            internal_server_error_response(connection_sock);
                        }
                        if (connection_close || c == -1 || c == 1) {
                            fclose(connection_sock_fstream);
                            connected = false;
                        }
                    }
                }
                else if (resource_status == "404") {
                    connection_close = parsed_headers.find("connection") != parsed_headers.end() &&
                                       parsed_headers["connection"] == "close";
                    if (not_found_response(connection_sock, connection_close) == -1 || connection_close) {
                        fclose(connection_sock_fstream);
                        connected = false;
                    }
                }
                else {
                    try {
                        resource_canonical_path = fs::weakly_canonical(
                                resources_dir.string().append(parsed_request["target"]));
                        connection_close = parsed_headers.find("connection") != parsed_headers.end() &&
                                           parsed_headers["connection"] == "close";
                        resource_str_path = resource_canonical_path.string();
                        c = ok_response(connection_sock, resource_str_path,
                                        parsed_request["method"], connection_close);
                        if (c == 1) {
                            internal_server_error_response(connection_sock);
                        }
                        if (connection_close || c == -1 || c == 1) {
                            fclose(connection_sock_fstream);
                            connected = false;
                        }
                    }
                    catch (...) {
                        internal_server_error_response(connection_sock);
                        fclose(connection_sock_fstream);
                        connected = false;
                    }
                }
            }
        }
    }

    return 0;
}

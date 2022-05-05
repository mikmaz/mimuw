/**
 * Interface for server utilities.
 *
 * Author:   Mikołaj Mazurczyk
 * Index no: 426819
 */

#ifndef SERVER_UTIL_H
#define SERVER_UTIL_H

#include <filesystem>

namespace fs = std::filesystem;

/**
 * Converts port string to integer and returns it. In case of any error function exits program with EXIT_FAILURE.
 */
int convert_port_num(char *port);

/**
 * Creates server socket, binds it with @p port_num and starts listening on returned socket. In case of any error
 * function exits program with EXIT_FAILURE.
 */
int start_server(int port_num);

/**
 * Tries to find @p resource in file containing correlated resources.
 * @param  correlated_servers_path - absolute path to file containing information about correlated servers.
 * @return If resource has been successfully found, function returns HTTP address pointing to the resource. If resource
 *         hasn't been found, function returns "404", on internal error "500".
 */
std::string find_resource_in_correlated_servers(std::string &resource, std::string &correlated_servers_path);

/**
 * Checks if server has @p resource inside its @p server_resources_dir. Function returns:
 * - "200" if file was successfully found and can be read by program,
 * - "correlated" if file should be looked up in correlated servers,
 * - "404" if resource is outside directory or any error with it's path occurred,
 * - "400" if resource doesn't start with "/",
 * - "500" if any other system error occurred.
 */
std::string check_resource(std::string resource, fs::path &server_resources_dir);

/**
 * Checks if @p resources_dir_str_path points to directory that program has access to. If yes, function returns absolute
 * path to the directory. Otherwise function exits program with EXIT_FAILURE.
 */
fs::path get_resources_dir(char *resources_dir_str_path);

/**
 * Checks if @p correlated_servers_f_str_path points to a file that program has access to. If yes, function returns
 * absolute path to the file. Otherwise function exits program with EXIT_FAILURE.
 */
fs::path get_correlated_servers(char *correlated_servers_f_str_path);

#endif //SERVER_UTIL_H

/**
 * Interface for reading and parsing server requests.
 *
 * Author:   Mikołaj Mazurczyk
 * Index no: 426819
 */

#ifndef REQ_PARSE_H
#define REQ_PARSE_H

#include <unordered_map>

/**
 * Tries to parse http request line given in string. On success, function returns map containing request's method and
 * target under "method" and "target" keys. On failure, function sets returned map's "error" key to value:
 * - "400", if request line doesn't match HTTP/1.1 message format,
 * - "500", if internal error occurred,
 * - "501", if request's method is not equal to either HEAD or GET,
 * - "404", if target contains unsupported characters.
 */
std::unordered_map<std::string, std::string> parse_request_line(const std::string &request_line);

/**
 * Reads line (ended with CRLF) from server's TCP connection socket's file stream. On success, function returns read
 * line as string (containing CRLF), otherwise, function returns:
 * - "400", if read line's size is greater than configured max,
 * - "500", if internal error occurred,
 * - "connection closed", if connection with client terminated.
 */
std::string read_line_from_sock(FILE *sock_fstream);

/**
 * Tries to parse http request's header line given in string. On success, function returns 0 and adds key-value pair
 * {field-name, field-vale} to @p parsed_headers map. On failure, function sets returned map's "error" key to value:
 * - "400", if header line doesn't match HTTP/1.1 message format,
 * - "500", if internal error occurred.
 */
int parse_header_line(const std::string &header_line, std::unordered_map<std::string, std::string> &parsed_headers);

/**
 * Tries to read all request's header lines, parse and store them in returned map. On failure, function adds "error"
 * key-value pair to map and returns it.
 */
std::unordered_map<std::string, std::string> read_and_parse_headers(FILE *sock_fd);

/**
 * Checks if @p parsed_input map contains error message different than "404" and "501". If yes, function sends response
 * corresponding to "error" key's value, ends connection with client and returns 1. Otherwise, no changes to map are
 * made and function returns 0.
 */
int parsing_error(std::unordered_map<std::string, std::string> &parsed_input, int sock, FILE *sock_fstream);

#endif //REQ_PARSE_H

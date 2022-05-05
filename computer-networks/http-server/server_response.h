/**
 * Interface for sending server responses.
 *
 * Author:   Mikołaj Mazurczyk
 * Index no: 426819
 */

#ifndef SERVER_RESPONSE_H
#define SERVER_RESPONSE_H

#include <string>

/**
 * Sends "Bad Request" response with status code 400. On write error function returns -1, otherwise 0 is returned.
 */
int bad_request_response(int sock);

/**
 * Sends "Not Found" response with status code 404. Depending on @p close_connection, function sends "Connection: close"
 * header or not. On write error function returns -1, otherwise 0 is returned.
 */
int not_found_response(int sock, bool close_connection);

/**
 * Sends "Internal Server Error" response with status code 500. On write error function returns -1, otherwise 0 is
 * returned.
 */
int internal_server_error_response(int sock);

/**
 * Sends "Not Implemented" response with status code 501. Depending on @p close_connection, function sends
 * "Connection: close" header or not. On write error function returns -1, otherwise 0 is returned.
 */
int not_implemented_response(int sock, bool close_connection);

/**
 * Sends "OK" response with status code 200. Supported values of @p method are GET and HEAD. Depending on
 * @p close_connection, function sends "Connection: close" header or not. Function returns:
 * -  0 if there was no error during execution,
 * - -1 if there was write error,
 * -  1 if any other error occurred.
 */
int ok_response(int sock, std::string &resource_path, std::string &method, bool close_connection);

/**
 * Sends "Found" response with status code 302. Depending on @p close_connection, function sends "Connection: close"
 * header or not. Function returns:
 * -  0 if there was no error during execution,
 * - -1 if there was write error,
 * -  1 if any other error occurred.
 */
int send_correlated_server(int sock, std::string &correlated_server_address, bool close_connection);

#endif //SERVER_RESPONSE_H

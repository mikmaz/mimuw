/**
 * Class describing server's socket.
 *
 * Author:   Mikołaj Mazurczyk
 * Index no: 426819
 */

#ifndef SOCKET_H
#define SOCKET_H

#include <netinet/in.h>
#include <poll.h>

class Socket {
private:
    pollfd poll_sock{};
public:
    explicit Socket(uint16_t port);

    /**
     * Run's poll in order to check if there's new datagram to read.
     */
    bool run_poll(int timeout);

    [[nodiscard]] int get_fd() const {
        return poll_sock.fd;
    }
};


#endif //SOCKET_H

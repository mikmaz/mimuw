#include "Socket.h"
#include <stdexcept>

Socket::Socket(uint16_t port) {
    sockaddr_in6 ip6{};
    ip6.sin6_family = AF_INET6;
    ip6.sin6_addr = in6addr_any;
    ip6.sin6_port = htons(port);
    ip6.sin6_flowinfo = 0;
    ip6.sin6_scope_id = 0;
    if ((poll_sock.fd = socket(AF_INET6, SOCK_DGRAM, 0)) == -1) {
        throw std::runtime_error("socket");
    }
    poll_sock.events = POLLIN;
    poll_sock.revents = 0;
    int ipv6only_off = 0;
    if (setsockopt(poll_sock.fd, IPPROTO_IPV6, IPV6_V6ONLY, &ipv6only_off, sizeof(ipv6only_off)) == -1) {
        throw std::runtime_error("setsockopt");
    }

    if (bind(poll_sock.fd, (sockaddr *) &ip6, sizeof(ip6)) == -1) {
        throw std::runtime_error("bind");
    }
}

bool Socket::run_poll(int timeout) {
    poll_sock.revents = 0;
    int ret = poll(&poll_sock, 1, timeout);
    return ret > 0 && poll_sock.revents & POLLIN;
}

#include "Player.h"
#include <cstring>
#include <math.h>
#include <iostream>

Player::Player(std::string name, uint64_t session_id,in_port_t port, in6_addr *addr) {
    last_contact = current_msec_time();
    connected = true;
    in_game = false;
    ready = false;
    player_name = std::move(name);
    this->session_id = session_id;
    this->port = port;
    memcpy((this->addr).s6_addr, addr->s6_addr, sizeof(addr->s6_addr));
}

sockaddr_in6 Player::get_writing_ip6_addr() {
    sockaddr_in6 ip6{};
    ip6.sin6_addr = addr;
    ip6.sin6_port = port;
    ip6.sin6_scope_id = 0;
    ip6.sin6_flowinfo = 0;
    ip6.sin6_family = AF_INET6;

    return ip6;
}

bool Player::the_same_player(std::string &other_player_name, const in_port_t *other_port, in6_addr *other_addr) {
    for (size_t i = 0; i < sizeof(addr.s6_addr); ++i) {
        if (addr.s6_addr[i] != other_addr->s6_addr[i]) {
            return false;
        }
    }

    return player_name == other_player_name && port == *other_port;
}

int Player::compare_session_id(uint64_t other_session_id) const {
    if (session_id < other_session_id) {
        return -1;
    }
    if (session_id == other_session_id) {
        return 0;
    }

    return 1;
}

bool Player::operator < (const Player &other_player) const {
    if (player_name == other_player.player_name) {
        return session_id < other_player.session_id;
    }

    return player_name < other_player.player_name;
}

void Player::init_worm_starting_pos(std::pair<uint32_t, uint32_t> board_dims, RandomGenerator &rand,
                                    uint8_t player_number) {
    in_game = true;
    worm.x = ((long double) (rand.generate() % board_dims.first)) + 0.5;
    worm.y = ((long double) (rand.generate() % board_dims.second)) + 0.5;
    worm.angle = rand.generate() % 360;
    worm.worm_no = player_number;
    worm.alive = true;
}

bool Player::player_lost(std::pair<uint32_t, uint32_t> board_dims,
                         std::unordered_set<std::pair<uint32_t, uint32_t>, pair_hash> &taken_pixels) {
    uint32_t rounded_x = this->rounded_x();
    uint32_t rounded_y = this->rounded_y();
    if (worm.x < 0 || worm.y < 0 || rounded_x >= board_dims.first || rounded_y >= board_dims.second ||
        taken_pixels.find({rounded_x, rounded_y}) != taken_pixels.end()) {
        worm.alive = false;
        return true;
    }

    return false;
}

bool Player::update_pos(uint32_t turning_speed) {
    if (!worm.alive) {
        return false;
    }

    worm.angle += worm.turn_direction * 1.0 * ((long double) turning_speed);
    uint32_t previous_x = rounded_x();
    uint32_t previous_y = rounded_y();
    worm.x += cosl(worm.angle * M_PIl / 180.0);
    worm.y += sinl(worm.angle * M_PIl / 180.0);

    if (rounded_x() != previous_x || rounded_y() != previous_y) {
        return true;
    }

    return false;
}

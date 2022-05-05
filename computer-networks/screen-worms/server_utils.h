/**
 * Interface for server utilities.
 *
 * Author:   Mikołaj Mazurczyk
 * Index no: 426819
 */

#ifndef SERVER_UTILS_H
#define SERVER_UTILS_H

#include <cstdint>
#include <unordered_set>

/**
 * Returns current time from Epoch in milliseconds. Throws runtime error if 'gettimeofday' fails.
 */
uint64_t current_msec_time();

enum event_t : uint8_t {
    NEW_GAME = 0,
    PIXEL = 1,
    PLAYER_ELIMINATED = 2,
    GAME_OVER = 3
};

struct __attribute__((__packed__)) clients_datagram {
    uint64_t session_id;
    uint8_t turn_direction;
    uint32_t next_expected_event_no;
    uint8_t player_name[];
};


struct __attribute__((__packed__)) datagram_game_id {
    uint32_t game_id;
};

struct __attribute__((__packed__)) datagram_event_header {
    uint32_t len;
    uint32_t event_no;
    event_t event_type;
};

struct __attribute__((__packed__)) datagram_new_game_data {
    uint32_t maxx;
    uint32_t maxy;
    char player_names[];
};

struct __attribute__((__packed__)) datagram_pixel_data {
    uint8_t player_number;
    uint32_t x;
    uint32_t y;
};

struct __attribute__((__packed__)) datagram_player_eliminated_data {
    uint8_t player_number;
};

/**
 * Hash for pairs used for storing occupied pixels in 'unordered_set' game board.
 */
struct pair_hash {
    template <class T1, class T2>
    std::size_t operator () (std::pair<T1, T2> const &pair) const {
        std::size_t h1 = std::hash<T1>()(pair.first);
        std::size_t h2 = std::hash<T2>()(pair.second);

        return h1 ^ h2;
    }
};

class RandomGenerator {
private:
    uint32_t current_val;
    uint64_t mult_const = 279410273;
    uint64_t mod_const = 4294967291;
public:
    explicit RandomGenerator(uint32_t seed) {
        current_val = seed;
    }

    uint32_t generate();
};

#endif //SERVER_UTILS_H

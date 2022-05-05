/**
 * Class describing player (client).
 *
 * Author:   Mikołaj Mazurczyk
 * Index no: 426819
 */

#ifndef PLAYER_H
#define PLAYER_H

#include <unordered_set>
#include <netinet/in.h>

#include "server_utils.h"

/**
 * Auxiliary class for the Player describing his worm's current state on board during game.
 */
class Worm {
public:
    uint8_t worm_no;  // equivalent to 'player_number'
    bool alive;  // 'true' if worm is alive at current round
    long double x;  // current worm position on X axis
    long double y;  // current worm position on Y axis
    long double angle;  // current worm's turning angle
    int turn_direction;  // -1: left, 0: straight, 1: right
};

class Player {
private:
    uint64_t last_contact;  // player's client's last contact time in milliseconds
    uint32_t next_expected_event{};
    uint64_t session_id;
    bool connected;  // 'true' if player's client has active session
    bool in_game;  // 'true' if player is participating in active game
    bool ready;  // 'true' if player is ready to start a new game
    std::string player_name;
    Worm worm{};
    in6_addr addr{};
    in_port_t port;
public:
    Player(std::string name, uint64_t session_id, in_port_t port, in6_addr *addr);

    /**
     * Operator needed for sorting vector of players. In case 'player_name' fields are equal, function compares player's
     * 'session_id' fields.
     */
    bool operator < (const Player &other_player) const;

    void update_last_contact_time() {
        last_contact = current_msec_time();
    }

    void connect() {
        connected = true;
    }

    void update_readiness() {
        ready = ready || (worm.turn_direction != 0);
    }

    /**
     * Returns player's client's address in form that is ready to write to.
     */
    sockaddr_in6 get_writing_ip6_addr();

    /**
     * Checks whether two players belong to the same client (in terms of task specification).
     */
    bool the_same_player(std::string &other_player_name, const in_port_t *other_port, in6_addr *other_addr);

    /**
     * Returns:
     * : -1 if this session id < than other,
     * :  0 if they are equal,
     * :  1 otherwise.
     */
    [[nodiscard]] int compare_session_id(uint64_t other_session_id) const;

    /**
     * Updates player's worm's turning direction. If @p new_turn_dir equals 2 (which is left in task's specification),
     * function stores it as -1 for worm's angle's computation convenience.
     */
    void update_worm_turn_dir(uint8_t new_turn_dir) {
        worm.turn_direction = ((new_turn_dir == 2) ? -1 : new_turn_dir);
    }

    void update_next_expected_event(uint32_t new_nee) {
        next_expected_event = new_nee;
    }

    [[nodiscard]] bool has_events_to_receive(uint32_t events_no) const {
        return next_expected_event < events_no;
    }

    [[nodiscard]] uint32_t get_next_expected_event() const {
        return next_expected_event;
    }

    void increment_next_expected_event() {
        next_expected_event++;
    }

    /**
     * Sets player's worm's members for starting game.
     */
    void init_worm_starting_pos(std::pair<uint32_t, uint32_t> board_dims, RandomGenerator &rand, uint8_t player_number);

    /**
     * Checks if player lost and if yes, function kills his worm.
     */
    bool player_lost(std::pair<uint32_t, uint32_t> board_dims,
                     std::unordered_set<std::pair<uint32_t, uint32_t>, pair_hash> &taken_pixels);

    /**
     * Updates current in-game player's members. Returns true if position has changed, otherwise returns false.
     */
    bool update_pos(uint32_t turning_speed);

    void end_game() {
        in_game = false;
        worm.alive = false;
        ready = false;
    }

    [[nodiscard]] uint8_t player_number() const {
        return worm.worm_no;
    }

    [[nodiscard]] uint32_t rounded_x() const {
        return (uint32_t) worm.x;
    }

    [[nodiscard]] uint32_t rounded_y() const {
        return (uint32_t) worm.y;
    }

    [[nodiscard]] uint64_t get_last_contact() const {
        return last_contact;
    }

    void disconnect() {
        connected = false;
    }

    [[nodiscard]] bool is_in_game() const {
        return in_game;
    }

    [[nodiscard]] bool is_connected() const {
        return connected;
    }

    [[nodiscard]] bool is_ready() const {
        return ready;
    }

    void zero_next_expected_event() {
        next_expected_event = 0;
    }

    void exclude_from_game() {
        in_game = false;
    }

    std::string name() {
        return player_name;
    }
};


#endif //PLAYER_H

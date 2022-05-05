/**
 * Class describing game's engine.
 *
 * Author:   Mikołaj Mazurczyk
 * Index no: 426819
 */

#ifndef WORMSENGINE_H
#define WORMSENGINE_H

#include "Player.h"
#include "Event.h"
#include "ServerArgs.h"

class WormsEngine {
private:
    uint64_t player_inactivity_tolerance = 2000000; // 2 seconds
    uint64_t round_interval;
    uint64_t next_round_time;
    std::pair<uint32_t, uint32_t> board_dims;
    bool active_game;
    uint32_t worm_turning_speed;
    uint32_t game_id;
    std::vector<Player> *players;
    std::vector<Event> *events;
    RandomGenerator rand;
    std::unordered_set<std::pair<uint32_t, uint32_t>, pair_hash> taken_pixels = {};

    void disconnect_inactive_players();

    bool all_players_ready();
public:
    WormsEngine(ServerArgs &parsed_args, std::vector<Player> &players, std::vector<Event> &events);

    void update_next_round_time() {
        next_round_time += round_interval;
    }

    bool interval_passed() const {
        return current_msec_time() >= next_round_time;
    }

    uint32_t get_game_id() const {
        return game_id;
    }

    /**
     * Moves player to new position. Returns true if position has changed, otherwise returns false.
     */
    bool move_player(Player &player);

    /**
     * Returns names of players currently participating in game.
     */
    std::vector<std::string> get_playing_player_names();

    void start_new_game();

    /**
     * Returns number of players that haven't lost yet.
     */
    uint32_t get_players_in_game();

    /**
     * Clear player's member's values at the end of the game.
     */
    void clear_players();

    uint64_t time_to_next_round() const {
        return next_round_time - current_msec_time();
    }

    bool game_is_active() const {
        return active_game;
    }

    void run_round();
};

#endif //WORMSENGINE_H

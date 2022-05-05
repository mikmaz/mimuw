#include "WormsEngine.h"
#include <algorithm>
#include <iostream>

void WormsEngine::disconnect_inactive_players() {
    for (auto player_it = players->begin(); player_it != players->end(); ++player_it) {
        if (current_msec_time() - player_it->get_last_contact() > player_inactivity_tolerance) {
            player_it->disconnect();
        }
        if (!player_it->is_in_game() && !player_it->is_connected()) {
            players->erase(player_it--);
        }
    }
}

bool WormsEngine::all_players_ready() {
    unsigned int ready_players = 0;
    for (Player &player : *players) {
        if (!player.name().empty()) {
            if (!player.is_ready()) {
                return false;
            }
            ready_players++;
        }
    }

    return ready_players >= 2;
}

WormsEngine::WormsEngine(ServerArgs &parsed_args, std::vector<Player> &players, std::vector<Event> &events)
: rand(parsed_args.get_random_seed()) {
    next_round_time = current_msec_time();
    round_interval = 1000000 / parsed_args.get_rounds_per_sec();
    active_game = false;
    game_id = 0;
    board_dims.first = parsed_args.get_board_width();
    board_dims.second = parsed_args.get_board_height();
    worm_turning_speed = parsed_args.get_turning_speed();
    this->players = &players;
    this->events = &events;
}

bool WormsEngine::move_player(Player &player) {
    if (player.player_lost(board_dims, taken_pixels)) {
        events->push_back(Event((uint32_t) events->size(), player.player_number()));
        return false;
    }
    else {
        uint32_t rounded_x = player.rounded_x();
        uint32_t rounded_y = player.rounded_y();
        taken_pixels.insert({rounded_x, rounded_y});
        events->push_back(Event((uint32_t) events->size(), player.player_number(), rounded_x, rounded_y));
        return true;
    }
}

std::vector<std::string> WormsEngine::get_playing_player_names() {
    std::vector<std::string> player_names;
    for (Player &player : *players) {
        if (player.is_in_game()) {
            player_names.push_back(player.name());
        }
    }

    return player_names;
}

void WormsEngine::start_new_game() {
    events->clear();
    taken_pixels.clear();
    uint8_t current_player_number = 0;
    for (Player &player : *players) {
        player.zero_next_expected_event();
        if (!player.name().empty()) {
            player.init_worm_starting_pos(board_dims, rand, current_player_number);
            current_player_number++;
        }
        else {
            player.exclude_from_game();
        }
    }

    events->emplace_back(Event(0, board_dims.first, board_dims.second, get_playing_player_names()));
    for (Player &player : *players) {
        if (player.is_in_game()) {
            move_player(player);
        }
    }
}

uint32_t WormsEngine::get_players_in_game() {
    uint32_t players_in_game = 0;
    for (Player &player : *players) {
        if (player.is_in_game()) {
            players_in_game++;
        }
    }

    return players_in_game;
}

void WormsEngine::clear_players() {
    for (Player &player : *players) {
        player.end_game();
    }
}

void WormsEngine::run_round() {
    disconnect_inactive_players();
    if (active_game) {
        uint32_t players_in_game = get_players_in_game();
        bool pos_updated;
        for (Player &player : *players) {
            if (player.is_in_game()) {
                pos_updated = player.update_pos(worm_turning_speed);
                if (pos_updated) {
                    if (!move_player(player)) {
                        players_in_game--;
                    }
                }

                if (players_in_game < 2) {
                    clear_players();
                    events->push_back(Event(events->size()));
                    active_game = false;
                    break;
                }
            }
        }
    }
    else if (all_players_ready()) {
        std::sort(players->begin(), players->end());
        game_id = rand.generate();
        start_new_game();
        active_game = true;
        next_round_time = current_msec_time();
    }
}

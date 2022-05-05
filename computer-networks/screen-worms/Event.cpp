#include "Event.h"

#include <utility>

Event::Event(uint32_t event_no) {
    type = GAME_OVER;
    this->event_no = event_no;
}

Event::Event(uint32_t event_no, uint32_t maxx, uint32_t maxy, std::vector<std::string> player_names) {
    type = NEW_GAME;
    this->event_no = event_no;
    this->maxx = maxx;
    this->maxy = maxy;
    this->player_names = std::move(player_names);
}

Event::Event(uint32_t event_no, uint8_t player_number, uint32_t x, uint32_t y) {
    type = PIXEL;
    this->event_no = event_no;
    this->player_number = player_number;
    this->x = x;
    this->y = y;
}

Event::Event(uint32_t event_no, uint8_t player_number) {
    type = PLAYER_ELIMINATED;
    this->event_no = event_no;
    this->player_number = player_number;
}

size_t Event::get_player_names_len() {
    size_t names_len = 0;
    for (std::string &player_name : player_names) {
        names_len += player_name.length() + 1; // string length + '\0' char
    }

    return names_len;
}

/**
 * Class describing game events.
 *
 * Author:   Mikołaj Mazurczyk
 * Index no: 426819
 */

#ifndef EVENT_H
#define EVENT_H

#include <vector>
#include <string>

#include "server_utils.h"

class Event {
public:
    uint32_t event_no;
    event_t type;
    uint32_t x{};
    uint32_t maxx{};
    uint32_t y{};
    uint32_t maxy{};
    uint8_t player_number{};
    std::vector<std::string> player_names;

    /**
     * GAME_OVER event constructor.
     */
    explicit Event(uint32_t event_no);

    /**
     * NEW_GAME event constructor.
     */
    Event(uint32_t event_no, uint32_t maxx, uint32_t maxy, std::vector<std::string> player_names);

    /**
     * PIXEL event constructor.
     */
    Event(uint32_t event_no, uint8_t player_number, uint32_t x, uint32_t y);

    /**
     * PLAYER_ELIMINATED event constructor.
     */
    Event(uint32_t event_no, uint8_t player_number);

    /**
     * Returns sum of all player names' lengths stored in the event
     * (including '\0' character between every player name).
     */
    size_t get_player_names_len();
};

#endif //EVENT_H

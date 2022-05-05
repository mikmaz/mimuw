/**
 * Main server's class.
 *
 * Author:   Mikołaj Mazurczyk
 * Index no: 426819
 */

#ifndef WORMSSERVER_H
#define WORMSSERVER_H

#include "WormsEngine.h"
#include "Socket.h"

class WormsServer {
private:
    Socket sock;
    bool *run_server;  // if false, server terminates
    const size_t max_datagram_size = 550;
    const size_t max_player_name_size = 20;
    WormsEngine game_engine;
    std::vector<Player> players;
    std::vector<Event> events;
public:
    WormsServer(ServerArgs &parsed_args, bool *run_server) : sock(parsed_args.get_port()),
    game_engine(parsed_args, players, events) {
        this->run_server = run_server;
    }

    /**
     * Sends all events with 'event_no' greater than player's 'next_expected_event' to the player.
     * @param player
     */
    void send_events(Player &player);

    /**
     * Main server's loop.
     */
    void Run();
};

#endif //WORMSSERVER_H

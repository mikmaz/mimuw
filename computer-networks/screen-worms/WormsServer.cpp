#include "WormsServer.h"
#include "crc32.h"
#include <iostream>


void WormsServer::send_events(Player &player) {
    if (!player.is_connected() ||!player.has_events_to_receive((uint32_t) events.size())) {
        return;
    }

    uint8_t buffer[max_datagram_size];
    size_t datagram_size = sizeof(datagram_game_id);
    ((datagram_game_id *) &buffer)->game_id = htonl(game_engine.get_game_id());

    Event *event;
    size_t event_size;
    size_t events_starting_pos;
    size_t pn_datagram_idx; // pn - player name
    for (size_t i = player.get_next_expected_event(); i < events.size(); i++) {
        event = &events[i];
        event_size = sizeof(datagram_event_header) + sizeof(uint32_t); // constant event fields + crc32

        switch (event->type) {
            case NEW_GAME:
                event_size += sizeof(datagram_new_game_data) + event->get_player_names_len();
                break;
            case PIXEL:
                event_size += sizeof(datagram_pixel_data);
                break;
            case PLAYER_ELIMINATED:
                event_size += sizeof(datagram_player_eliminated_data);
            default:
                break;
        }

        if (datagram_size + event_size > max_datagram_size) {
            break;
        }

        events_starting_pos = datagram_size;
        auto *event_header = (datagram_event_header *)(buffer + datagram_size);
        event_header->len = htonl((uint32_t) event_size - 2 * sizeof(uint32_t)); // - (game_id + crc32)
        event_header->event_no = htonl(event->event_no);
        event_header->event_type = event->type; // uint8_t, no need to convert
        datagram_size += sizeof(datagram_event_header);

        switch (event->type) {
            case NEW_GAME: {
                auto new_game_data = (datagram_new_game_data *) (buffer + datagram_size);
                new_game_data->maxx = htonl(event->maxx);
                new_game_data->maxy = htonl(event->maxy);
                datagram_size += sizeof(datagram_new_game_data);
                pn_datagram_idx = 0;
                for (std::string &player_name : event->player_names) {
                    for (char j : player_name) {
                        new_game_data->player_names[pn_datagram_idx] = j;
                        pn_datagram_idx++;
                    }
                    new_game_data->player_names[pn_datagram_idx] = '\0';
                    pn_datagram_idx++;
                }
                datagram_size += pn_datagram_idx;
                break;
            }
            case PIXEL: {
                auto pixel_data = (datagram_pixel_data *) (buffer + datagram_size);
                pixel_data->player_number = event->player_number; // uint8_t, no need to convert
                pixel_data->x = htonl(event->x);
                pixel_data->y = htonl(event->y);
                datagram_size += sizeof(datagram_pixel_data);
                break;
            }
            case PLAYER_ELIMINATED: {
                auto player_eliminated_data = (datagram_player_eliminated_data *) (buffer + datagram_size);
                player_eliminated_data->player_number = event->player_number;
                datagram_size += sizeof(datagram_player_eliminated_data);
                break;
            }
            default:
                break;
        }

        *((uint32_t *) (buffer + datagram_size)) = htonl(crc32(0, buffer + events_starting_pos,
                                                               datagram_size - events_starting_pos));
        datagram_size += sizeof(uint32_t); // crc32
        player.increment_next_expected_event();
    }

    sockaddr_in6 ip6 = player.get_writing_ip6_addr();
    ssize_t sent_size = sendto(sock.get_fd(), buffer, datagram_size, MSG_DONTWAIT,
                               (sockaddr *) &ip6, sizeof(ip6));

    if (sent_size == EAGAIN || sent_size == EWOULDBLOCK) {
        sent_size = sendto(sock.get_fd(), buffer, datagram_size, 0, (sockaddr *) &ip6, sizeof(ip6));
    }

    if (sent_size != (ssize_t) datagram_size) {
        std::cerr << "Sending events failed.\n";
    }

    send_events(player);
}

void WormsServer::Run() {
    while (*run_server) {
        if (game_engine.interval_passed()) {
            game_engine.run_round();
            for (Player &player : players) {
                send_events(player);
            }
            game_engine.update_next_round_time();
        }
        else {
            if (sock.run_poll((int) (game_engine.time_to_next_round() / 1000))) {
                uint8_t buffer[max_datagram_size];
                sockaddr_in6 ip6_addr{};
                socklen_t ip6_addr_len = sizeof(sockaddr_storage);
                ssize_t received_size = recvfrom(sock.get_fd(), &buffer, sizeof(buffer), 0, (sockaddr *) &ip6_addr, &ip6_addr_len);
                if (received_size < (ssize_t) sizeof(clients_datagram) ||
                        received_size > (ssize_t) (sizeof(clients_datagram) + max_player_name_size)) {
                    continue;
                }

                auto *received_datagram = (clients_datagram *) buffer;
                if (received_datagram->turn_direction > 2) {
                    continue;
                }

                size_t player_name_size = received_size - sizeof(clients_datagram);
                bool skip_msg = false;
                for (size_t i = 0; i < player_name_size; i++) {
                    if (received_datagram->player_name[i] < 33 || received_datagram->player_name[i] > 126) {
                        skip_msg = true;
                        break;
                    }
                }
                if (skip_msg) {
                    continue;
                }

                std::string player_name((char *) received_datagram->player_name, player_name_size);
                Player *clients_player = nullptr;
                for (Player &player : players) {
                    if (player.is_connected() &&
                    player.the_same_player(player_name, &ip6_addr.sin6_port, &ip6_addr.sin6_addr)) {
                        uint64_t clients_session_id = be64toh(received_datagram->session_id);
                        switch (player.compare_session_id(clients_session_id)) {
                            case -1:
                                player.disconnect();
                                break;
                            case 0:
                                clients_player = &player;
                                break;
                            default:
                                skip_msg = true;
                        }

                        break;
                    }
                    else if (player.name() == player_name) {
                        skip_msg = true;
                        break;
                    }
                }

                if (skip_msg) {
                    continue;
                }

                if (clients_player == nullptr) {
                    Player new_clients_player = Player(player_name, be64toh(received_datagram->session_id),
                                                       ip6_addr.sin6_port, &ip6_addr.sin6_addr);
                    players.push_back(new_clients_player);
                    clients_player = &players.back();
                }

                clients_player->update_last_contact_time();
                clients_player->connect();
                clients_player->update_worm_turn_dir(received_datagram->turn_direction);
                clients_player->update_next_expected_event(ntohl(received_datagram->next_expected_event_no));

                if (!game_engine.game_is_active()) {
                    clients_player->update_readiness();
                }

                send_events(*clients_player);
            }
        }
    }
}

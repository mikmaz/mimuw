/**
 * Interface for getting, parsing and storing server's option arguments.
 *
 * Author:   Mikołaj Mazurczyk
 * Index no: 426819
 */

#ifndef SERVERARGS_H
#define SERVERARGS_H

#include <unordered_map>

typedef std::unordered_map<char, std::string> input_t;

class ArgsException : public std::runtime_error {
public:
    explicit ArgsException(char const* const msg) : std::runtime_error(msg) {};
};

/**
 * Converts string to 4 bytes unsigned integer. Function throws 'runtime_error' if @p str:
 * - isn't decimal,
 * - is smaller than 1,
 * - is greater than UINT32_MAX.
 */
uint32_t str_to_positive_uint32(std::string &str);

/**
 * Reads option arguments and returns them in a map. Throws ArgsException if program arguments don't match task's
 * specification.
 */
input_t get_input_args(int argc, char *argv[]);

class ServerArgs {
private:
    uint16_t port;
    uint32_t random_seed;
    uint32_t turning_speed;
    uint32_t rounds_per_sec;
    uint32_t board_width;
    uint32_t board_height;

    static uint16_t parse_port_num(input_t &args);

    static uint32_t parse_random_seed(input_t &args);

    static uint32_t parse_turning_speed(input_t &args);

    static uint32_t parse_rounds_per_sec(input_t &args);

    static uint32_t parse_board_dim(input_t &args, char opt);

public:
    explicit ServerArgs(input_t &args);

    [[nodiscard]] uint32_t get_port() const {
        return port;
    }

    [[nodiscard]] uint32_t get_random_seed() const {
        return random_seed;
    }

    [[nodiscard]] uint32_t get_turning_speed() const {
        return turning_speed;
    }

    [[nodiscard]] uint32_t get_rounds_per_sec() const {
        return rounds_per_sec;
    }

    [[nodiscard]] uint32_t get_board_width() const {
        return board_width;
    }

    [[nodiscard]] uint32_t get_board_height() const {
        return board_height;
    }
};

#endif //SERVERARGS_H

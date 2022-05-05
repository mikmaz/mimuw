#include "ServerArgs.h"
#include <regex>
#include <unistd.h>

uint32_t str_to_positive_uint32(std::string &str) {
    if (!std::regex_match(str, std::regex("[1-9][0-9]*")))
        throw std::runtime_error("(str_to_positive_uint32) String isn't numeric value greater than zero.");

    unsigned long int res = std::strtoul(str.c_str(), nullptr, 10);
    if (res > UINT32_MAX) {
        throw std::runtime_error("(str_to_positive_uint32) Argument value out of range.");
    }

    return res;
}

input_t get_input_args(int argc, char *argv[]) {
    input_t args = {};
    int opt;
    opterr = 0;

    while ((opt = getopt(argc, argv, ":p:s:t:v:w:h:")) != -1) {
        if (opt == '?') {
            throw ArgsException("Unknown option.");
        }
        else if (opt == ':') {
            throw ArgsException("Option without argument.");
        }
        else {
            std::string arg(optarg);
            args[(char)opt] = arg;
        }
    }

    if (optind < argc)
        throw ArgsException("Non-option argument.");

    return args;
}

uint32_t ServerArgs::parse_rounds_per_sec(input_t &args) {
    if (args.find('v') == args.end()) {
        return 50;
    }

    uint32_t parsed_rounds_per_sec = str_to_positive_uint32(args['v']);
    if (parsed_rounds_per_sec > 250) {
        throw ArgsException("Rounds per second out of proper range.");
    }
    return parsed_rounds_per_sec;
}

uint32_t ServerArgs::parse_board_dim(input_t &args, char opt) {
    if (args.find(opt) == args.end()) {
        return (opt == 'w') ? 640 : 480;
    }

    uint32_t parsed_dim = str_to_positive_uint32(args[opt]);
    if (parsed_dim < 16 || parsed_dim > 4096) {
        throw ArgsException("Board dimension out of proper range.");
    }
    return parsed_dim;
}

uint32_t ServerArgs::parse_turning_speed(input_t &args) {
    if (args.find('t') == args.end()) {
        return 6;
    }
    else {
        uint32_t parsed_turning_speed = str_to_positive_uint32(args['t']);
        if (parsed_turning_speed > 90) {
            throw ArgsException("Turning speed out of proper range.");
        }

        return parsed_turning_speed;
    }
}

uint16_t ServerArgs::parse_port_num(input_t &args) {
    const int RESTRICTED_PORTS = 1023;
    if (args.find('p') == args.end()) {
        return 2021;
    }

    uint32_t port_num = str_to_positive_uint32(args['p']);
    if (port_num <= RESTRICTED_PORTS || port_num > UINT16_MAX) {
        throw ArgsException("Port number out of proper range.");
    }

    return port_num;
}

uint32_t ServerArgs::parse_random_seed(input_t &args) {
    if (args.find('s') == args.end()) {
        return (uint32_t)time(NULL);
    }
    else {
        if (args['s'] == "0") { // since 'str_to_positive_uint32' will throw an error if seed's value is 0
            return 0;
        }

        return str_to_positive_uint32(args['s']);
    }
}

ServerArgs::ServerArgs(input_t &args) {
    try {
        this->port = parse_port_num(args);
        this->random_seed = parse_random_seed(args);
        this->turning_speed = parse_turning_speed(args);
        this->rounds_per_sec = parse_rounds_per_sec(args);
        this->board_width = parse_board_dim(args, 'w');
        this->board_height = parse_board_dim(args, 'h');
    }
    catch (std::runtime_error &e) {
        throw ArgsException(e.what());
    }
}

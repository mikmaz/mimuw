#include <iostream>
#include <regex>
#include <csignal>
#include <math.h>
#include <set>
#include <sys/time.h>

#include "WormsServer.h"

bool run_server = true;

void catch_sigint([[maybe_unused]] int sig) {
    printf("\nStopping server...\n");
    run_server = false;
}

void set_sigint_handler() {
    sigset_t block_mask;
    sigemptyset(&block_mask);

    struct sigaction action{};
    action.sa_handler = catch_sigint;
    action.sa_mask = block_mask;
    action.sa_flags = SA_RESTART;

    if (sigaction(SIGINT, &action, nullptr) != 0) {
        throw std::runtime_error("sigaction");
    }
}

int main(int argc, char *argv[]) {
    try {
        input_t args = get_input_args(argc, argv);
        ServerArgs server_args(args);
        set_sigint_handler();
        WormsServer server(server_args, &run_server);
        server.Run();
    }
    catch (ArgsException &ae) {
        std::cerr << "RUNTIME ERROR: " << ae.what() << '\n';
        std::cout << "Usage: " << argv[0] << " [-p n] [-s n] [-t n] [-v n] [-w n] [-h n]\n";

        return EXIT_FAILURE;
    }
    catch (std::runtime_error &e) {
        std::cerr << "RUNTIME ERROR: " << e.what() << '\n';
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

#include "server_utils.h"

#include <sys/time.h>
#include <stdexcept>

uint64_t current_msec_time() {
    timeval tval{};
    if (gettimeofday(&tval, NULL) == -1) {
        throw std::runtime_error("gettimeofday");
    }
    return ((uint64_t) tval.tv_sec) * 1000000 + ((uint64_t) tval.tv_usec);
}

uint32_t RandomGenerator::generate() {
    uint32_t res = current_val;
    current_val = (uint32_t) ((((uint64_t) current_val) * mult_const) % mod_const);
    return res;
}

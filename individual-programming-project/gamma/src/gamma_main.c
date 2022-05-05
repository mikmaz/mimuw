#include "gamma.h"
#include "get_mode.h"
#include "batch_mode.h"
#include "interactive_mode.h"
#include <stdio.h>
#include <inttypes.h>

int main() {
  uint64_t line = 0;
  int mode = 0;
  gamma_t *g;

  if (!(g = get_game_mode(&line, &mode)))
    return 0;

  if (mode == 'B') {
    printf("OK %" PRIu64 "\n", line);
    run_batch_mode(g, &line);
  }
  else if (mode == 'I') {
    if (!run_interactive_mode(g)) {
      gamma_delete(g);
      return 1;
    }

    int ret = print_final_results(g);
    gamma_delete(g);
    return (ret) ? 0 : 1;
  }

  return 0;
}

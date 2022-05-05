#include "batch_mode.h"
#include "get_mode.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <inttypes.h>

#define UINT32_MAX_LEN ((int)floor(log10(UINT32_MAX)) + 1)

bool proper_command(int c) {
  return (c == 'm' || c == 'g' || c == 'b' || c == 'f' || c == 'q' || c == 'p');
}

void execute_command(gamma_t *g, uint32_t *params, int params_no, uint64_t line,
                     int command) {
  if (command == 'm' && params_no == 3) {
    printf("%d\n", gamma_move(g, params[0], params[1], params[2]));
  }
  else if (command == 'g' && params_no == 3) {
    printf("%d\n", gamma_golden_move(g, params[0], params[1], params[2]));
  }
  else if (command == 'b' && params_no == 1) {
    printf("%" PRIu64 "\n", gamma_busy_fields(g, params[0]));
  }
  else if (command == 'f' && params_no == 1) {
    printf("%" PRIu64 "\n", gamma_free_fields(g, params[0]));
  }
  else if (command == 'q' && params_no == 1) {
    printf("%d\n", gamma_golden_possible(g, params[0]));
  }
  else if (command == 'p' && params_no == 0) {
    char *buff = gamma_board(g);
    if (buff) {
      printf("%s", buff);
      free(buff);
    }
    else {
      print_line_err(line);
    }
  }
  else {
    print_line_err(line);
  }
}

int parse_first_char_of_batch_mode_line(int c, int command, uint64_t line, gamma_t *g) {
  if (c == '#') {
    if (skip_line() == EOF) {
      print_line_err(line);
      return -1;
    }
    else {
      return 0;
    }
  }

  if (c == '\n') {
    return 0;
  }

  if (!proper_command(command) || check_input_char(c = getchar()) != 0) {
    // If input line is actually a proper command.
    if (command == 'p' && c =='\n') {
      execute_command(g, NULL, 0, line, command);
      return 0;
    }

    print_line_err(line);
    if (c == '\n') {
      return 0;
    }

    if (skip_line() == EOF) {
      return -1;
    }
    else {
      return 0;
    }
  }

  return 1;
}

void run_batch_mode(gamma_t *g, uint64_t *line) {
  int c = getchar(), c_type = 0;

  int params_read = 0, param_chars_read = 0;
  uint32_t input_params[3] = { 0 };
  char param[UINT32_MAX_LEN + 1];
  memset(param, 0, UINT32_MAX_LEN + 1);

  int command = 0;
  int ret = 0;
  while (c != EOF) {
    *line += 1;
    command = c;
    if (!(ret = parse_first_char_of_batch_mode_line(c, command, *line, g))) {
      if (ret == 0) {
        c = getchar();
        continue;
      }
      else {
        break;
      }
    }

    while ((c = getchar()) != EOF) {
      if (c == '\n') {
        // If line has ended but there is parameter to convert.
        if (param_chars_read > 0 && params_read < 3) {
          if (!convert_param_str_to_num(&params_read, param, &param_chars_read,
                                        input_params, *line)) {
            c = getchar();
            break;
          }
        }

        if (param_chars_read == 0) {
          execute_command(g, input_params, params_read, *line, command);
          c = getchar();
          reset_game_mode_variables(&params_read, param, &param_chars_read,
                                    input_params);
          break;
        }
        else {
          print_line_err(*line);
          c = getchar();
          reset_game_mode_variables(&params_read, param, &param_chars_read,
                                    input_params);
          break;
        }
      }

      c_type = check_input_char(c);
      // Incorrect char read or there are too many parameters.
      if (c_type == -1 || (c_type && (param_chars_read == UINT32_MAX_LEN ||
                                      params_read == 3))) {
        print_line_err(*line);
        if (skip_line() == '\n') {
          c = getchar();
        }
        reset_game_mode_variables(&params_read, param, &param_chars_read,
                                  input_params);
        break;
      }

      if (c_type == 0 && param_chars_read > 0) {
        if (!convert_param_str_to_num(&params_read, param, &param_chars_read, input_params, *line)) {
          if (skip_line() == '\n')
            c = getchar();

          break;
        }
      }

      if (c_type) {
        param[param_chars_read] = (char)c;
        param_chars_read += 1;
      }
    }

    // If line didn't end with newline character.
    if ((param_chars_read || params_read) && c == EOF) {
      print_line_err(*line);
    }
  }

  if (g)
    gamma_delete(g);
}

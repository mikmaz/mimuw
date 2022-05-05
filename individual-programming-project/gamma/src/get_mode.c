#include "get_mode.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <inttypes.h>

#define UINT32_MAX_LEN ((int)floor(log10(UINT32_MAX)) + 1)
#define WIDTH 0
#define HEIGHT 1
#define PLAYERS 2
#define AREAS 3

int check_input_char(int c) {
  if (c >= '0' && c <= '9') return 1;
  // When 'c' is whitespace character.
  if (c == 32 || c == 9 || c == 11 || c== 12 || c == 13) return 0;
  return -1; // if character is invalid
}

int skip_line() {
  int c = EOF;
  while ((c = getchar()) != '\n' && c != EOF) {}
  return c;
}

void print_line_err(uint64_t line) {
  fprintf(stderr, "ERROR %" PRIu64 "\n", line);
}

void reset_game_mode_variables(int *params_read, char *param,
                               int *param_chars_read, uint32_t *input_params) {
  *param_chars_read = 0;
  memset(param, 0, UINT32_MAX_LEN + 1);
  for (int i = 0; i < *params_read; i++) {
    input_params[i] = 0;
  }
  *params_read = 0;
}

int parse_first_char_of_mode_line(int c, uint64_t line) {
  if (c == '#') { // line starting with '#' should be ignored
    if (skip_line() == EOF) { // line didn't end with newline character
      print_line_err(line);
      return -1;
    }
    else {
      return 0;
    }
  }
  else if (c == '\n') { // line starting with newline character can be skipped
    return 0;
  }
  else if (c != 'B' && c != 'I') { // if first character don't indicate any mode
    print_line_err(line);
    if (skip_line() == EOF) {
      return -1;
    }
    else {
      return 0;
    }
  }
  else if (check_input_char(c = getchar()) != 0) { // if first character
                                                      // indicates mode but next
                                                      // character is not
                                                      // whitespace
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

int convert_param_str_to_num(int *params_read, char *param,
                             int *param_chars_read, uint32_t *input_params,
                             uint64_t line) {
  param[*param_chars_read] = '\0';
  uint64_t param_num = strtoul(param, NULL, 0);
  if (errno == ERANGE || param_num > UINT32_MAX ||
      (param[0] == '0' && param[1] != '\0')) {
    print_line_err(line);
    reset_game_mode_variables(params_read, param, param_chars_read,
                              input_params);
    return 0;
  }

  input_params[*params_read] = param_num;
  *params_read += 1;
  memset(param, 0, UINT32_MAX_LEN + 1);
  *param_chars_read = 0;

  return 1;
}

gamma_t* get_game_mode(uint64_t *line, int *mode) {
  int c = getchar(), c_type = 0;

  int params_read = 0, param_chars_read = 0;
  uint32_t input_params[4] = { 0 };
  char param[UINT32_MAX_LEN + 1];
  memset(param, 0, UINT32_MAX_LEN + 1);

  gamma_t *g = NULL;
  int ret = 0;
  while (c != EOF) {
    *line += 1;
    *mode = c;

    if (!(ret = parse_first_char_of_mode_line(c, *line))) {
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
        if (param_chars_read > 0 && params_read < 4) {
          if (!convert_param_str_to_num(&params_read, param, &param_chars_read,
                                        input_params, *line)) {
            c = getchar();
            break;
          }
        }

        // If we have full needed number of parameters try to init game.
        if (params_read == 4 && param_chars_read == 0) {
          g = gamma_new(input_params[WIDTH], input_params[HEIGHT],
                        input_params[PLAYERS],input_params[AREAS]);
          if (!g) {
            print_line_err(*line);
            c = getchar();
            reset_game_mode_variables(&params_read, param, &param_chars_read,
                                      input_params);
            break;
          }
          else {
            return g;
          }
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
                                      params_read == 4))) {
        print_line_err(*line);
        if (skip_line() == '\n')
          c = getchar();

        reset_game_mode_variables(&params_read, param, &param_chars_read,
                                  input_params);
        break;
      }

      if (c_type == 0 && param_chars_read > 0) {
        if (!convert_param_str_to_num(&params_read, param, &param_chars_read,
                                      input_params, *line)) {
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

  return NULL;
}

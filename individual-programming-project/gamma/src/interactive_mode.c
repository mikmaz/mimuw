#include "interactive_mode.h"
#include <sys/ioctl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ncurses.h>
#include <inttypes.h>

#define EOT 0x04
#define KEY_SPACE ' '
#define KEY_G 'g'
#define KEY_CAP_G 'G'
#define KEY_C 'c'
#define KEY_CAP_C 'C'
#define COLOR_PAIRS_NEEDED 5
#define NON_PLAYER_COLORS_NO 3
#define MAX_PLAYER_COLORS 5
#define DEFAULT_COLOR 1
#define GOLDEN_COLOR 2
#define DEFAULT_BOARD_COLOR 3

int print_final_results(gamma_t *g) {
  char *final_board = gamma_board(g);
  if (!final_board) {
    fprintf(stderr, "Couldn't get the final board string.\n");
    return 0;
  }
  else {
    printf("\n############ FINAL RESUlTS ############\n");
    printf("----- FINAL BOARD -----\n");
    printf("%s\n", final_board);

    printf("----- PLAYERS SCORES -----\n");
    for (uint32_t player = 1; player <= g->players_no; player++) {
      printf("Player %d busy fields: %" PRIu64 "\n", player, g->players[player].fields);
    }

    free(final_board);
    return 1;
  }
}

uint32_t num_len(uint32_t num) {
  return floor(log10(num)) + 1;
}

int set_cell_color(bool current_cursor_position, uint32_t player,
                   int player_colors_no) {
  if (current_cursor_position) {
    if (attron(COLOR_PAIR(DEFAULT_COLOR)) == ERR ||
        attron(A_STANDOUT) == ERR) {
      fprintf(stderr, "attron failed\n");
      return 0;
    }
  }
  else if (player) {
    if (attron(COLOR_PAIR((player % player_colors_no) + 1 +
                          NON_PLAYER_COLORS_NO)) == ERR) {
      fprintf(stderr, "attron failed\n");
      return 0;
    }
  }
  else {
    if (attron(COLOR_PAIR(DEFAULT_COLOR)) == ERR) {
      fprintf(stderr, "attron failed\n");
      return 0;
    }
  }

  return 1;
}

int print_cell(uint32_t cell_width, uint32_t player) {
  char player_str[cell_width];
  memset(player_str, 0, cell_width);

  if (player) {
    for (uint32_t j = 0; j < cell_width - num_len(player); j++)
      if (addch(' ') == ERR) {
        fprintf(stderr, "addch failed\n");
        return 0;
      }

    if (!sprintf(player_str, "%d", player)) {
      fprintf(stderr, "sprintf failed\n");
      return 0;
    }

    if (printw(player_str) == ERR) {
      fprintf(stderr, "printw failed\n");
      return 0;
    }
  }
  else {
    for (uint32_t j = 0; j < cell_width; j++) {
      if (addch('.') == ERR) {
        fprintf(stderr, "addch failed\n");
        return 0;
      }
    }
  }

  return 1;
}

int print_board(gamma_t *g, uint32_t cursor_x, uint32_t cursor_y, int player_colors_no) {
  uint32_t cell_width = num_len(g->players_no) + 1;

  bool current_cursor_position = false;
  for (int64_t y = g->board_height - 1; y >= 0; y--) {
    for (uint32_t x = 0; x < g->board_width; x++) {
      uint32_t player = g->board[x][y];

      if (x == (cursor_x / cell_width) && g->board_height - y - 1 == cursor_y)
        current_cursor_position = true;

      if (!set_cell_color(current_cursor_position, player, player_colors_no) ||
          !print_cell(cell_width, player))
        return 0;

      if (current_cursor_position) {
        current_cursor_position = false;
        if (attroff(A_STANDOUT) != OK) {
          fprintf(stderr, "attroff failed\n");
          return 0;
        }
      }
    }

    if (attron(COLOR_PAIR(DEFAULT_COLOR)) != OK) {
      fprintf(stderr, "attron failed\n");
      return 0;
    }

    if (addch('\n') == ERR) {
      fprintf(stderr, "addch failed\n");
      return 0;
    }
  }

  if (refresh() == ERR) {
    fprintf(stderr, "refresh failed\n");
    return 0;
  }

  return 1;
}

int min_val(int a, int b) {
  return (a < b) ? a : b;
}

int set_non_player_colors() {
  if (init_pair(DEFAULT_COLOR, COLOR_WHITE, COLOR_BLACK) != OK) {
    fprintf(stderr, "init_pair failed\n");
    return 0;
  }

  if (init_pair(GOLDEN_COLOR, COLOR_YELLOW, COLOR_BLACK) != OK) {
    fprintf(stderr, "init_pair failed\n");
    return 0;
  }

  if (init_pair(DEFAULT_BOARD_COLOR, COLOR_BLACK, COLOR_WHITE) != OK) {
    fprintf(stderr, "init_pair failed\n");
    return 0;
  }

  return 1;
}

int set_player_colors(int player_colors_no) {
  int i, color = 0;
  for (i = 1; i <= player_colors_no; i++) {
    if (i == 2)
      color += 1;

    if (init_pair(NON_PLAYER_COLORS_NO + i, COLOR_WHITE, (short)(color + i)) !=
        OK) {
      fprintf(stderr, "init_pair failed\n");
      return 0;
    }
  }

  return 1;
}

int setup_colors(int *player_colors_no) {
  if (COLOR_PAIRS - 1 < COLOR_PAIRS_NEEDED) {
    fprintf(stderr, "Not enough color pairs.\n");
    return 0;
  }

  *player_colors_no = min_val(MAX_PLAYER_COLORS, COLOR_PAIRS - 1 -
                                                 NON_PLAYER_COLORS_NO);

  if (!set_non_player_colors() || !set_player_colors(*player_colors_no)) {
    return 0;
  }

  return 1;
}

int check_terminal_window_size(uint64_t width_needed, uint64_t height_needed) {
  struct winsize win_size;
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &win_size) == ERR) {
    fprintf(stderr, "ioctl failed\n");
    return 0;
  }
  else if (width_needed <= win_size.ws_col &&
           height_needed <= win_size.ws_row) {
    return 1;
  }
  else {
    fprintf(stderr, "Insufficient terminal size.\n");
    return 0;
  }
}

int setup_ncurses() {
  // init curses structure
  if (initscr() == NULL) {
    fprintf(stderr, "initscr failed\n");
    return 0;
  }

  // don't echo user key input when typing
  if (noecho() == ERR) {
    fprintf(stderr, "noecho failed\n");
    if (endwin() == ERR)
      fprintf(stderr, "endwin failed\n");
    return 0;
  }

  // available reading characters one by one
  if (cbreak() == ERR) {
    fprintf(stderr, "cbreak failed\n");
    if (endwin() == ERR)
      fprintf(stderr, "endwin failed\n");
    return 0;
  }

  if (has_colors() == FALSE) {
    fprintf(stderr, "Terminal doesn't support colors\n");
    if (endwin() == ERR)
      fprintf(stderr, "endwin failed\n");
    return 0;
  }

  // enable coloring
  if (start_color() == ERR) {
    fprintf(stderr, "start_color failed\n");
    if (endwin() == ERR)
      fprintf(stderr, "endwin failed\n");
    return 0;
  }

  return 1;
}

WINDOW* setup_interactive_board(gamma_t *g, int player_colors_no) {
  uint32_t cell_width = num_len(g->players_no) + 1;

  // Create new window with width and height of game board and with cursor
  // starting at point (0, 0).
  WINDOW *interactive_board = newwin(g->board_height,
                                     (int)(g->board_width *cell_width + 1),
                                     0, 0);
  if (!interactive_board) {
    fprintf(stderr, "newwin failed\n");
    return NULL;
  }

  if (keypad(interactive_board, true) != OK) {
    fprintf(stderr, "keypad failed\n");
    if (delwin(interactive_board) == ERR)
      fprintf(stderr, "delwin failed\n");
    return NULL;
  }

  if (!print_board(g, 0, 0, player_colors_no)) {
    if (delwin(interactive_board) == ERR)
      fprintf(stderr, "delwin failed\n");
    return NULL;
  }

  if (wmove(interactive_board, 0, 0) == ERR) {
    fprintf(stderr, "wmove failed\n");
    if (delwin(interactive_board) == ERR)
      fprintf(stderr, "delwin failed\n");
    return NULL;
  }

  if (wrefresh(interactive_board) == ERR) {
    fprintf(stderr, "wrefresh failed\n");
    if (delwin(interactive_board) == ERR)
      fprintf(stderr, "delwin failed\n");
    return NULL;
  }

  if (curs_set(0) == ERR) {
    fprintf(stderr, "Can't hide cursor.\n");
  }

  return interactive_board;
}

int print_player_info(gamma_t *g,uint32_t current_player, int player_colors_no) {
  if (attron(COLOR_PAIR((current_player % player_colors_no) + 1 +
                        NON_PLAYER_COLORS_NO)) == ERR) {
    fprintf(stderr, "attron failed\n");
    return 0;
  }

  if (mvprintw((int)(g->board_height + 1), 0, "PLAYER %d:", current_player) ==
      ERR) {
    fprintf(stderr, "mvprintw failed\n");
    return 0;
  }

  if (attron(COLOR_PAIR(DEFAULT_COLOR)) == ERR) {
    fprintf(stderr, "attron failed\n");
    return 0;
  }

  if (mvprintw((int)(g->board_height + 2), 0, "BUSY FIELDS %d",
               gamma_busy_fields(g, current_player)) == ERR ||
               mvprintw((int)(g->board_height + 3), 0, "FREE FIELDS %d",
                        gamma_free_fields(g, current_player)) == ERR) {
    fprintf(stderr, "mvprintw failed\n");
    return 0;
  }

  if (gamma_golden_possible(g, current_player)) {
    if (attron(COLOR_PAIR(GOLDEN_COLOR)) == ERR) {
      fprintf(stderr, "attron failed\n");
      return 0;
    }

    if (mvprintw((int)(g->board_height + 4), 0, "GOLDEN MOVE POSSIBLE: YES") ==
        ERR) {
      fprintf(stderr, "mvprintw failed\n");
      return 0;
    }
  }
  else {
    if (mvprintw((int)(g->board_height + 4), 0, "GOLDEN MOVE POSSIBLE: NO") ==
        ERR) {
      fprintf(stderr, "mvprintw failed\n");
      return 0;
    }
  }

  return 1;
}

bool skip_players(gamma_t *g, uint32_t *current_player) {
  uint32_t players_count = 0;
  while (players_count < g->players_no) {
    *current_player = ((*current_player) % g->players_no) + 1;
    if (gamma_any_move_possible(g, *current_player))
      return true;
    players_count += 1;
  }

  return false;
}

void react_to_user_key_press(int c, gamma_t *g, uint32_t *x, uint32_t *y,
                             uint32_t cell_width, uint32_t *current_player,
                             bool *any_move_possible) {
  switch (c) {
    case KEY_UP:
      if (*y > 0)
        (*y) -= 1;
      break;
    case KEY_DOWN:
      if ((*y) + 1 < g->board_height)
        *y += 1;
      break;
    case KEY_LEFT:
      if (*x != 0)
        *x -= cell_width;
      break;
    case KEY_RIGHT:
      if ((*x) + cell_width < g->board_width * cell_width)
        *x += cell_width;
      break;
    case KEY_SPACE:
      if (gamma_move(g, *current_player, (*x) / cell_width,
                     g->board_height - (*y) - 1))
        *any_move_possible = skip_players(g, current_player);
      break;
    case KEY_G:
    case KEY_CAP_G:
      if (gamma_golden_move(g, *current_player, (*x) / cell_width,
                            g->board_height - (*y) - 1))
        *any_move_possible = skip_players(g, current_player);
      break;
    case KEY_C:
    case KEY_CAP_C:
      *any_move_possible = skip_players(g, current_player);
      break;
    default:
      break;
  }
}

int close_interactive_window(WINDOW *interactive_board) {
  int ret = 1;
  if (delwin(interactive_board) == ERR) {
    fprintf(stderr, "delwin failed\n");
    ret = 0;
  }

  if (endwin() == ERR) {
    fprintf(stderr, "endwin failed\n");
    ret = 0;
  }

  return ret;
}

int run_interactive_mode(gamma_t *g) {
  uint32_t current_player = 1;
  uint32_t x = 0, y = 0;
  uint32_t cell_width = num_len(g->players_no) + 1;

  uint64_t width_needed = g->board_width * cell_width + 1;
  uint64_t height_needed = g->board_height + 4;
  int player_colors_no = 0;


  if (!check_terminal_window_size(width_needed, height_needed))
    return 0;

  if (!setup_ncurses())
    return 0;

  if (!setup_colors(&player_colors_no)) {
    if (endwin() == ERR)
      fprintf(stderr, "endwin failed\n");
    return 0;
  }

  WINDOW *interactive_board = setup_interactive_board(g, player_colors_no);
  if (!interactive_board) {
    close_interactive_window(interactive_board);
    return 0;
  }

  bool any_move_possible = true;
  int c = 0;
  while (any_move_possible && c != EOT) {
    if (clear() == ERR) {
      fprintf(stderr, "clear failed\n");
      close_interactive_window(interactive_board);
      return 0;
    }

    if (wmove(interactive_board, y, x) == ERR) {
      fprintf(stderr, "wmove failed\n");
      close_interactive_window(interactive_board);
      return 0;
    }

    if (!print_board(g, x, y, player_colors_no) ||
        !print_player_info(g, current_player, player_colors_no)) {
      close_interactive_window(interactive_board);
      return 0;
    }

    if (refresh() == ERR) {
      fprintf(stderr, "refresh failed\n");
      close_interactive_window(interactive_board);
      return 0;
    }

    c = wgetch(interactive_board);
    if (c == ERR) {
      fprintf(stderr, "wgetch failed\n");
      close_interactive_window(interactive_board);
      return 0;
    }

    react_to_user_key_press(c, g, &x, &y, cell_width, &current_player,
                            &any_move_possible);
  }

  if (!close_interactive_window(interactive_board))
    return 0;
  else
    return 1;
}

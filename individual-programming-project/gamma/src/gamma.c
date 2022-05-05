#include "gamma.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

void free_board(void **board, uint32_t width) {
  if (!board) {
    return;
  }
  else {
    uint32_t i;
    for (i = 0; i < width; i++) {
      free(board[i]);
    }

    free(board);
  }
}

void gamma_delete(gamma_t *g) {
    if (!g)
        return;

    if (g->board)
        free_board((void **) g->board, g->board_width);

    if (g->visited)
        free_board((void **) g->visited, g->board_width);

    if (g->players)
        free(g->players);

    free(g);
}

void init_gamma_members(gamma_t *g, uint32_t height, uint32_t players,
                        uint32_t areas) {
  g->max_areas = areas;
  g->board_height = height;
  g->players_no = players;
  g->board_width = 0;
  g->busy_fields = 0;
  g->board = NULL;
  g->visited = NULL;
  g->players = NULL;
}

gamma_t* init_gamma_boards(gamma_t *g, uint32_t width) {
  g->board = malloc(width * sizeof(uint32_t *));
  if (!g->board) {
    gamma_delete(g);
    return NULL;
  }

  g->visited = malloc(width * sizeof(bool *));
  if (!g->visited) {
    gamma_delete(g);
    return NULL;
  }

  uint32_t i;
  for (i = 0; i < width; i++) {
    g->board[i] = malloc(g->board_height * sizeof(uint32_t));
    if (!g->board[i]) {
      gamma_delete(g);
      return NULL;
    }

    g->visited[i] = malloc(g->board_height * sizeof(bool));
    if (!g->visited[i]) {
      // Free this field since 'gamma_delete' will free only columns up to
      // current game board width value.
      free(g->board[i]);
      gamma_delete(g);
      return NULL;
    }

    g->board_width += 1;
    memset(g->board[i], 0, g->board_height * sizeof(uint32_t));
    memset(g->visited[i], 0, g->board_height * sizeof(bool));
  }

  return g;
}

gamma_t* init_gamma_players(gamma_t* g, uint32_t players) {
  g->players = malloc((players + 1) * sizeof(player_t));
  if (!g->players) {
    gamma_delete(g);
    return NULL;
  }

  uint32_t i;
  for (i = 0; i < players + 1; i++) {
    g->players[i].areas = 0;
    g->players[i].fields = 0;
    g->players[i].golden_move_available = true;
    g->players[i].areas_borders = 0;
  }

  return g;
}

gamma_t* gamma_new(uint32_t width, uint32_t height, uint32_t players,
                   uint32_t areas) {
  if (!width || !height || !players || !areas)
    return NULL;

  gamma_t *gamma_game = malloc(sizeof(gamma_t));
  if (!gamma_game)
    return NULL;

  init_gamma_members(gamma_game, height, players, areas);

  if (!init_gamma_boards(gamma_game, width))
    return NULL;

  if (!init_gamma_players(gamma_game, players))
    return NULL;

  return gamma_game;
}

bool check_adjacent_fields(gamma_t *g, uint32_t player, uint32_t x,
                           uint32_t y) {
  if (x != g->board_width - 1 && g->board[x + 1][y] == player)
    return true;

  if (x != 0 && g->board[x - 1][y] == player)
    return true;

  if (y != g->board_height - 1 && g->board[x][y + 1] == player)
    return true;

  if (y != 0 && g->board[x][y - 1] == player)
    return true;

  return false;
}

void visit_adjacent_fields(gamma_t *g, uint32_t player, uint32_t x,
                           uint32_t y) {
  if (g->board[x][y] != player)
    return;

  g->visited[x][y] = true;

  if (x != g->board_width - 1 && !g->visited[x + 1][y])
    visit_adjacent_fields(g, player, x + 1, y);

  if (x != 0 && !g->visited[x - 1][y])
    visit_adjacent_fields(g, player, x - 1, y);

  if (y != g->board_height - 1 && !g->visited[x][y + 1])
    visit_adjacent_fields(g, player, x, y + 1);

  if (y != 0 && !g->visited[x][y - 1])
    visit_adjacent_fields(g, player, x, y - 1);
}

bool border_already_updated(const uint32_t *adjacent_players,
                            uint32_t already_checked) {
  if (already_checked == 0)
    return false;

  uint32_t i;
  for (i = 0; i < already_checked; i++) {
    if (adjacent_players[i] == adjacent_players[already_checked])
      return true;
  }

  return false;
}

void update_adjacent_field_border(gamma_t *g, uint32_t player, uint32_t x,
                                  uint32_t y, uint32_t *adjacent_players,
                                  uint32_t already_checked) {
  if ((adjacent_players[already_checked] = g->board[x][y]) != 0) {
    if (!border_already_updated(adjacent_players, already_checked))
      g->players[adjacent_players[already_checked]].areas_borders -= 1;
  }
  else {
    if (!check_adjacent_fields(g, player, x, y))
      g->players[player].areas_borders += 1;
  }
}

void update_areas_borders(gamma_t *g, uint32_t player, uint32_t x, uint32_t y) {
  // Stores id of players surrounding (x, y) field, '0' indicates free field.
  uint32_t adjacent_players[4] = { 0, 0, 0, 0};
  // Number of surrounding fields that already have been checked.
  uint32_t already_checked = 0;

  if (x != g->board_width - 1)
    update_adjacent_field_border(g, player, x + 1, y, adjacent_players,
                                 already_checked);
  already_checked += 1;

  if (x != 0)
    update_adjacent_field_border(g, player, x - 1, y, adjacent_players,
                                 already_checked);
  already_checked += 1;

  if (y != g->board_height - 1)
    update_adjacent_field_border(g, player, x, y + 1, adjacent_players,
                                 already_checked);
  already_checked += 1;

  if (y != 0)
    update_adjacent_field_border(g, player, x, y - 1, adjacent_players,
                                 already_checked);
}

void update_adjacent_field_border_after_golden_move(gamma_t *g, uint32_t player,
                                                    uint32_t enemy,
                                                    uint32_t x, uint32_t y) {
  if (g->board[x][y])
    return;

  if (!check_adjacent_fields(g, player, x, y))
    g->players[player].areas_borders += 1;

  if (!check_adjacent_fields(g, enemy, x, y))
    g->players[enemy].areas_borders -= 1;
}

void update_areas_borders_after_golden_move(gamma_t *g, uint32_t player,
                                            uint32_t enemy, uint32_t x,
                                            uint32_t y) {
  if (x != g->board_width - 1)
    update_adjacent_field_border_after_golden_move(g, player, enemy,x + 1, y);

  if (x != 0)
    update_adjacent_field_border_after_golden_move(g, player, enemy, x - 1, y);

  if (y != g->board_height - 1)
    update_adjacent_field_border_after_golden_move(g, player, enemy, x, y + 1);

  if (y != 0)
    update_adjacent_field_border_after_golden_move(g, player, enemy, x,y - 1);
}

void unvisit_adjacent_fields(gamma_t *g, uint32_t player, uint32_t x,
                             uint32_t y) {
  g->visited[x][y] = false;

  if (x != g->board_width - 1 && g->visited[x + 1][y])
    unvisit_adjacent_fields(g, player, x + 1, y);

  if (x != 0 && g->visited[x - 1][y])
    unvisit_adjacent_fields(g, player, x - 1, y);

  if (y != g->board_height - 1 && g->visited[x][y + 1])
    unvisit_adjacent_fields(g, player, x, y + 1);

  if (y != 0 && g->visited[x][y - 1])
    unvisit_adjacent_fields(g, player, x, y - 1);
}

uint64_t count_areas_around_field(gamma_t *g, uint32_t player, uint32_t x,
                                  uint32_t y) {
  uint32_t areas_around_field = 0;

  if (x != g->board_width - 1 && g->board[x + 1][y] == player) {
    areas_around_field += 1;
    visit_adjacent_fields(g, player, x + 1, y);
  }

  if (x != 0 && g->board[x - 1][y] == player && g->visited[x - 1][y] == false) {
    areas_around_field += 1;
    visit_adjacent_fields(g, player, x - 1, y);
  }

  if (y != g->board_height - 1 && g->board[x][y + 1] == player &&
      g->visited[x][y + 1] == false) {
    areas_around_field += 1;
    visit_adjacent_fields(g, player, x, y + 1);
  }

  if (y != 0 && g->board[x][y - 1] == player && g->visited[x][y - 1] == false) {
    areas_around_field += 1;
    visit_adjacent_fields(g, player, x, y - 1);
  }

  unvisit_adjacent_fields(g, player, x, y);
  g->visited[x][y] = false;

  return areas_around_field;
}

bool gamma_move(gamma_t *g, uint32_t player, uint32_t x, uint32_t y) {
  if (!g || g->players_no < player || !player || x >= g->board_width ||
      y >= g->board_height)
    return false;

  if (!g->board[x][y]) {
    if (g->max_areas == g->players[player].areas &&
        !check_adjacent_fields(g, player, x, y)) {
      return false;
    }
    else {
      uint32_t player_areas_around_field = count_areas_around_field(g, player,
                                                                    x, y);
      update_areas_borders(g, player, x, y);
      g->board[x][y] = player;
      g->players[player].fields += 1;
      g->busy_fields += 1;
      g->players[player].areas = g->players[player].areas + 1 -
          player_areas_around_field;
      return true;
    }
  }

  return false;
}

bool gamma_golden_move(gamma_t *g, uint32_t player, uint32_t x, uint32_t y) {
  if (!g || g->players_no < player || !player || x >= g->board_width ||
      y >= g->board_height || !g->players[player].golden_move_available ||
      !g->board[x][y] || g->board[x][y] == player)
    return false;

  uint32_t player_areas_around_field = count_areas_around_field(g, player, x,
                                                                y);
  uint32_t enemy = g->board[x][y];
  g->board[x][y] = player;
  uint32_t enemy_areas_around_field = count_areas_around_field(g, enemy, x, y);

  uint64_t enemy_areas = g->players[enemy].areas + enemy_areas_around_field - 1;
  uint64_t players_areas = g->players[player].areas + 1 -
      player_areas_around_field;

  if (enemy_areas > g->max_areas || players_areas > g->max_areas) {
    g->board[x][y] = enemy;
    return false;
  }

  g->board[x][y] = 0;
  update_areas_borders_after_golden_move(g, player, enemy, x, y);
  g->board[x][y] = player;

  g->players[player].areas = players_areas;
  g->players[player].fields += 1;
  g->players[player].golden_move_available = false;

  g->players[enemy].areas = enemy_areas;
  g->players[enemy].fields -= 1;

  return true;
}

uint64_t gamma_busy_fields(gamma_t *g, uint32_t player) {
  if (!g || g->players_no < player || player == 0)
    return 0;
  else
    return g->players[player].fields;
}

uint64_t gamma_free_fields(gamma_t *g, uint32_t player) {
  if (!g || g->players_no < player || player == 0)
    return 0;

  if (g->players[player].areas == g->max_areas) {
    return g->players[player].areas_borders;
  }
  else {
    return (g->board_width * g->board_height) - g->busy_fields;
  }
}

bool gamma_golden_possible(gamma_t *g, uint32_t player) {
  if (!g || g->players_no < player || player == 0)
    return 0;

  if (!g->players[player].golden_move_available)
    return false;

  // Check if any player different than '@p player' occupies any field.
  uint32_t i;
  for (i = 1; i < g->players_no + 1; i++) {
    if (i != player && g->players[i].fields > 0)
      break;
    else if (i == g->players_no)
      return false;
  }

  if (g->players[player].areas != g->max_areas)
    return true;

  // Now we have to check only enemy fields near 'player' fields.
  for (uint32_t x = 0; x < g->board_width; x++) {
    for (uint32_t y = 0; y < g->board_height; y++) {
      if (g->board[x][y] && g->board[x][y] != player &&
          check_adjacent_fields(g, player, x, y)) {
        uint32_t enemy_player = g->board[x][y];
        g->board[x][y] = player;
        uint64_t enemy_areas_after_deletion = count_areas_around_field(g, enemy_player, x, y);
        g->board[x][y] = enemy_player;

        if (g->players[enemy_player].areas + enemy_areas_after_deletion - 1 <= g->max_areas) {
          return true;
        }
      }
    }
  }

  return false;
}

bool gamma_any_move_possible(gamma_t *g, uint32_t player) {
  return (gamma_golden_possible(g, player) || gamma_free_fields(g, player));
}

char* realloc_buff(char *buff, uint64_t *bsize, uint64_t bfill) {
  *bsize *= 2;
  char *tmp = realloc(buff, (*bsize) * sizeof(char));

  if (!tmp) {
    free(buff);
    return NULL;
  }

  memset(tmp + bfill, 0, *bsize - bfill);

  return tmp;
}

char* add_char_to_buff(char *buff, uint64_t *bsize, uint64_t *bfill, char c) {
  if (*bfill == *bsize - 1) {
    if (!(buff = realloc_buff(buff, bsize, *bfill)))
      return NULL;
  }

  buff[*bfill] = c;
  *bfill += 1;

  return buff;
}

char* add_number_to_buff(char *buff, uint64_t *bsize, uint64_t *bfill,
                         uint32_t num) {
  uint32_t num_len = (floor(log10(num)) + 1) * sizeof(char);
  char str[num_len + 1];
  sprintf(str, "%d", num);

  while (*bfill + num_len > *bsize - 1) {
    if (!(buff = realloc_buff(buff, bsize, *bfill)))
      return NULL;
  }

  uint32_t i;
  for (i = 0; i < num_len; i++) {
    buff[*bfill] = str[i];
    *bfill += 1;
  }

  return buff;
}

char* gamma_board(gamma_t *g) {
  if (!g)
    return NULL;

  char *buff = malloc(2 * sizeof(char));
  if (!buff)
    return NULL;
  uint64_t bsize = 2;
  uint64_t bfill = 0;
  memset(buff, 0, bsize);

  int64_t y;
  uint32_t x;
  for (y = g->board_height - 1; y >= 0; y--) {
    for (x = 0; x < g->board_width; x++) {
      if (!g->board[x][y]) {
        if (!(buff = add_char_to_buff(buff, &bsize, &bfill, '.')))
          return NULL;
      }
      else {
        if (!(buff = add_number_to_buff(buff, &bsize, &bfill, g->board[x][y])))
          return NULL;
      }
    }

    if (!(buff = add_char_to_buff(buff, &bsize, &bfill, '\n')))
      return NULL;
  }

  buff[bfill] = '\0';
  return buff;
}

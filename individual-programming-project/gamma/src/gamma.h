/** @file
 * Interface for gamma game engine.
 *
 * @author Mikołaj Mazurczyk
 */

#ifndef GAMMA_H
#define GAMMA_H

#include <stdbool.h>
#include <stdint.h>

/**
 * Structure storing information about player at current state of the game.
 */
typedef struct player {
  /// Number of areas owned by the player.
  uint64_t areas;
  /// Number of fields owned by the player.
  uint64_t fields;
  /// Number of fields surrounding areas owned by the player.
  uint64_t areas_borders;
  /// Indicates if player have already used his golden move.
  bool golden_move_available;
} player_t;



/**
 * Structure storing information about current state of the game.
 */
typedef struct gamma {
  /// Two dimensional array storing all game fields. If 'board[x][y] == 0' field
  /// is free, otherwise 'board[x][y]' equals id of the player occupying the
  /// field.
  uint32_t **board;
  /// Two dimensional auxiliary array for game engine.
  bool **visited;
  /// Number of players participating in the game.
  uint32_t players_no;
  /// Array of structures storing information about players participating in the
  /// game. 'players[0]' doesn't contain information about any player for
  /// readability purposes.
  player_t *players;
  /// Height of the game board.
  uint32_t board_height;
  /// Width of the game board.
  uint32_t board_width;
  /// Max number of areas a player can own.
  uint32_t max_areas;
  /// Number of non-free fields of the game.
  uint64_t busy_fields;
} gamma_t;

/** @brief Initializes gamma game structure.
 * Allocates memory for gamma game structure, representing game in initial
 * state.
 * @param[in] width   – game board width,
 * @param[in] height  – game board height,
 * @param[in] players – number of players participating in game,
 * @param[in] areas   – maximal number of areas a player can own.
 * @return Pointer to newly allocated game structure, or NULL in case of an
 * error.
 */
gamma_t* gamma_new(uint32_t width, uint32_t height, uint32_t players,
                   uint32_t areas);

/** @brief Deletes gamma game structure.
 * Free's all allocated memory for @p g, or do nothing if @p g equals NULL.
 * @param[in] g – pointer to gamma game structure.
 */
void gamma_delete(gamma_t *g);

/** @brief Executes player move.
 * Puts @p player pawn on field (@p x, @p y) and updates state of the game.
 * @param[in,out] g  – pointer to gamma game structure,
 * @param[in] player – id of @p player making the move,
 * @param[in] x      – first coordinate of the field where pawn should be put,
 * @param[in] y      – second coordinate of the field where pawn should be put.
 * @return Value @p true if move was correctly executed, @p false if the move is
 * illegal or if any parameter is incorrect.
 */
bool gamma_move(gamma_t *g, uint32_t player, uint32_t x, uint32_t y);

/** @brief Executes golden move.
 * Puts pawn of the @p player on field (@p x, @p y) occupied by other player.
 * @param[in,out] g  – pointer to gamma game structure,
 * @param[in] player – id of the @p player making golden move,
 * @param[in] x      – first coordinate of the field where @p player pawn
 *                      should be put,
 * @param[in] y      – second coordinate of the field where @p player pawn
 *                      should be put.
 * @return Value @p true if move was correctly executed, @p false if the move is
 * illegal or if any parameter is incorrect.
 */
bool gamma_golden_move(gamma_t *g, uint32_t player, uint32_t x, uint32_t y);

/** @brief Returns number of fields occupied by player.
 * Returns number of fields occupied by @p player.
 * @param[in] g      – pointer to gamma game structure,
 * @param[in] player – id of the player.
 * @return Number of fields occupied by the player or @p 0 if any parameter is
 * incorrect.
 */
uint64_t gamma_busy_fields(gamma_t *g, uint32_t player);

/** @brief Returns number of fields player can occupy.
 * Returns number of fields @p player can occupy by making normal move.
 * @param[in] g      – pointer to gamma game structure,
 * @param[in] player – id of the player.
 * @return Number of fields player can occupy or @p 0 if any parameter is
 * incorrect.
 */
uint64_t gamma_free_fields(gamma_t *g, uint32_t player);

/** @brief Checks if player can execute golden move.
 * Checks if player @p can make golden move on any game field.
 * @param[in] g      – pointer to gamma game structure,
 * @param[in] player – id of the player.
 * @return Value @p true if player can make golden move, @p false if player used
 * his golden move or if move would break gamma game rules.
 */
bool gamma_golden_possible(gamma_t *g, uint32_t player);

/** @brief Returns string describing current state of the game board.
 * Allocates memory for buffer storing string which describes current state of
 * the game board. Caller has to free the buffer.
 * @param[in] g – pointer to gamma game structure.
 * @return Pointer to newly allocated buffer describing current state of the
 * board or NULL if buffer couldn't be allocated.
 */
char* gamma_board(gamma_t *g);

/** @brief Checks if player can make any move.
 * Checks if @p player can make any type of move (normal or golden).
 * @param[in] g      - pointer to gamma game structure,
 * @param[in] player - id of the player.
 * @return Value @p true if any move can be made, otherwise false.
 */
bool gamma_any_move_possible(gamma_t *g, uint32_t player);

#endif /* GAMMA_H */

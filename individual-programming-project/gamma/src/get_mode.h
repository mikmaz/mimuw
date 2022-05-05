/** @file
 * Interface for parsing command line input and setting up initial game mode.
 *
 * @author Mikołaj Mazurczyk
 */

#ifndef GET_MODE_H
#define GET_MODE_H

#include "gamma.h"

/** @brief Checks type of input character.
 * Checks type of input character based on gamma program constraints.
 * @param[in] c - ASCII value of character.
 * @return @p 1 if character represents digit, @p 0 if 'c' represents whitespace
 * character, otherwise returns @p -1.
 */
int check_input_char(int c);

/** Skips input line.
 * Reads input characters one by one until newline character  or EOF.
 * @return ASCII value of newline character or EOF depending which came first.
 */
int skip_line();

/** @brief Prints error message.
 * Prints error message on stderr with number of line that generated the error.
 * @param[in] line - number of line.
 */
void print_line_err(uint64_t line);

/** @brief Resets variables used for reading command line parameters.
 * Sets variables used for reading command line parameters to their initial
 * values.
 * @param[in,out] params_read      - number of parameters read form input line,
 * @param[in,out] param            - string storing currently read parameter,
 * @param[in,out] param_chars_read - number of read characters of currently read
 *                                   parameter
 * @param[in,out] input_params     - array storing parameters read from input
 *                                   line converted to unsigned int.
 */
void reset_game_mode_variables(int *params_read, char *param,
                               int *param_chars_read, uint32_t *input_params);

/** @brief Gets information about the game.
 * As long as proper command describing game and its mode wasn't provided
 * function reads input lines and parses them. If input line is correct function
 * tries to initialize gamma game based on given parameters. Caller has to
 * remove returned gamma game.
 * @param[in,out] line - number of input lines read,
 * @param[in,out] mode - mode in which game should be run.
 * @return On success function returns pointer to newly allocated gamma game
 * structure, if EOF character was met function returns NULL.
 */
gamma_t* get_game_mode(uint64_t *line, int *mode);

/** @brief Coverts parameter string to number
 * Function tries to convert @p param to number and store it in @p params_read.
 * On success converted @p param is added to @p params_read and
 * @p param_chars_read is set to 0, @param gets cleared. On error function
 * prints information on stderr and sets all parameters except @p line to their
 * initial values.
 * @param[in,out] params_read      - number of parameters read form input line,
 * @param[in,out] param            - string storing currently read parameter,
 * @param[in,out] param_chars_read - number of read characters of currently read
 *                                   parameter
 * @param[in,out] input_params     - array storing parameters read from input
 *                                   line converted to unsigned int,
 * @param[in] line                 - number of input lines read.
 * @return Value @p 1 on success, @p 0 on error.
 */
int convert_param_str_to_num(int *params_read, char *param,
                             int *param_chars_read, uint32_t *input_params,
                             uint64_t line);

#endif //GET_MODE_H

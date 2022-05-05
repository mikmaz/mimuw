/** @file
 * Interface for running gamma game in interactive mode.
 *
 * @author Mikołaj Mazurczyk
 */

#ifndef INTERACTIVE_MODE_H
#define INTERACTIVE_MODE_H

#include "gamma.h"

/** @brief Print final results of the game.
 * Function allocates a buffer for string storing game board, prints it along
 * with players statistics and frees the buffer. If allocation failed, function
 * prints only error message on stderr.
 * @param[in] g - pointer to gamma game struct.
 * @return Value @p 1 if buffer allocation succeeded, otherwise @p 0.
 */
int print_final_results(gamma_t *g);

/** @brief Runs gamma game in interactive mode.
 * Function runs gamma game in interactive mode using 'ncurses' library. Game
 * ends if:
 *  - all users can't make any move,
 *  - user pressed EOT key,
 *  - fatal error occurred.
 *  On exit interactive window is closed and all structures corresponding to it
 *  are freed. If fatal error occurred, error message is printed on the stderr.
 * @param[in,out] g - pointer to gamma game structure.
 * @return Value @p 1 if game was run successfully, if fatal error occurred @p 0
 * is returned.
 */
int run_interactive_mode(gamma_t *g);


#endif //INTERACTIVE_MODE_H

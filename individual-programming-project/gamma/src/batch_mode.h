/** @file
 * Interface for running gamma game in batch mode.
 *
 * @author Mikołaj Mazurczyk
 */

#ifndef BATCH_MODE_H
#define BATCH_MODE_H

#include "gamma.h"

/** @brief Runs game in batch mode.
 * As long as program don't encounter EOF on stdin, function reads input lines,
 * parses them and if they are correct tries to execute corresponding commands
 * of the gamma game. On error function prints message with number of line of
 * error triggering command on stderr. Before returning function deletes @p g
 * game.
 * @param[in,out] g    - pointer to gamma game structure,
 * @param[in]     line - pointer to number of input lines read.
 */
void run_batch_mode(gamma_t *g, uint64_t *line);

#endif //BATCH_MODE_H

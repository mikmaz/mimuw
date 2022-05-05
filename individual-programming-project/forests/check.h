/** @file
 * 'CHECK' command interface.
 */

#ifndef CHECK_H
#define CHECK_H

#include "bst.h"
#include <stdbool.h>

/**
 * Checks if 'entities' are found in any forest.
 * @return 'true' if entities were found,
 * @return otherwise 'false'.
 */
bool check_any_forest(Forest forests_bst_root, InputEntities entities);

/**
 * Checks if 'entities' are found in specific forest.
 * @return 'true' if entities were found,
 * @return otherwise 'false'.
 */
bool check_specific_forest(Forest forests_bst_root, InputEntities entities);

#endif //CHECK_H

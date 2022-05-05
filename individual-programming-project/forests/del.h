/** @file
 * Interface for 'DEL' command.
 */

#ifndef DEL_H
#define DEL_H

#include "bst.h"

/**
 * Deletes given entity from forests BST tree (if entity is not found structure
 * remains unchanged).
 * @return root of forests BST tree with deleted entity.
 */
BstTree delete_forest(Forest forests_bst_root, InputEntities entities);

#endif //DEL_H

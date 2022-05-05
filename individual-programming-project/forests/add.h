/** @file
 * 'ADD' command interface.
 */

#ifndef ADD_H
#define ADD_H

#include "bst.h"

/**
 * Adds new animal to the tree.
 * @param animals_node - node of BST tree containing information about animals
 *        on certain tree.
 * @param entities - entities that are being added.
 *
 * @return pointer to node of newly added animal or the one that already
 *         existed if it had the same name as in 'entities' parameter.
 * @return NULL if fatal error occurred (malloc failed).
 */
Animal add_animal(Animal animals_node, InputEntities entities);

/**
 * Adds new tree to the forest.
 * @param trees_node - node of BST tree containing information about trees of
 *        certain forest.
 * @param entities - entities that are being added.
 *
 * @return pointer to node of newly added tree or the one that already existed
 *         if it had the same name as in 'entities' parameter.
 * @return NULL if fatal error occurred (malloc failed).
 */
Tree add_tree(Tree trees_node, InputEntities entities);

/**
 * Adds new forest to the forests BST tree.
 * @param forests_bst_root - root of the BST tree containing information about
 *        forests added in the program.
 * @param entities - entities that are being added.
 *
 * @return pointer to root of the forests BST tree.
 * @return NULL if fatal error occurred (malloc failed).
 */
Forest add_forest(Tree forests_bst_root, InputEntities entities);

#endif //ADD_H

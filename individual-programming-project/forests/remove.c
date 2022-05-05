#include "remove.h"
#include <stdlib.h>

void remove_animals_bst(Animal animals_bst_root) {
    if (!animals_bst_root) return;

    remove_animals_bst(animals_bst_root->left);
    animals_bst_root->left = NULL;
    remove_animals_bst(animals_bst_root->right);
    animals_bst_root->right = NULL;

    free(animals_bst_root->value);
    free(animals_bst_root);
    animals_bst_root = NULL;
}

void remove_trees_bst(Tree trees_bst_root) {
    if (!trees_bst_root) return;

    remove_trees_bst(trees_bst_root->left);
    trees_bst_root->left = NULL;
    remove_trees_bst(trees_bst_root->right);
    trees_bst_root->right = NULL;

    remove_animals_bst(trees_bst_root->entities_root);
    trees_bst_root->entities_root = NULL;

    free(trees_bst_root->value);
    free(trees_bst_root);

}

void remove_forests_bst(Forest forests_bst_root) {
    if (!forests_bst_root) return;

    remove_forests_bst(forests_bst_root->left);
    forests_bst_root->left = NULL;
    remove_forests_bst(forests_bst_root->right);
    forests_bst_root->right = NULL;

    remove_trees_bst(forests_bst_root->entities_root);
    forests_bst_root->entities_root = NULL;

    free(forests_bst_root->value);
    free(forests_bst_root);
    forests_bst_root = NULL;
}
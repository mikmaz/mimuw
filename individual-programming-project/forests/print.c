#include "print.h"
#include <stdio.h>

void print_bst(BstTree node) {
    if (!node) return;
    if (node->left) print_bst(node->left);
    printf("%s\n", node->value);
    if (node->right) print_bst(node->right);
}

void print_animals(Animal animals_bst_root) {
    print_bst(animals_bst_root);
}

void print_trees(Tree trees_bst_root, InputEntities entities) {
    if (!(entities->tree)) {
        print_bst(trees_bst_root);
    }
    else {
        Tree tree = find(trees_bst_root, entities->tree);
        if (tree && tree->entities_root) {
            print_animals(tree->entities_root);
        }
    }
}

void print_forests(Forest forests_bst_root, InputEntities entities) {
    if (!forests_bst_root) return;
    if (!(entities->forest)) {
        print_bst(forests_bst_root);
    }
    else {
        Forest forest = find(forests_bst_root, entities->forest);
        if (forest && forest->entities_root) {
            print_trees(forest->entities_root, entities);
        }
    }
}

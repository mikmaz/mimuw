#include "check.h"
#include <string.h>

bool check_specific_animal(Animal animals_bst_root, InputEntities entities) {
    Animal found_animal = find(animals_bst_root, entities->animal);

    if (found_animal)
        return true;
    else
        return false;
}

bool check_specific_tree(Tree trees_bst_root, InputEntities entities) {
    Tree found_tree = find(trees_bst_root, entities->tree);

    if (!found_tree) {
        return false;
    }
    else {
        if (!(entities->animal))  {
            return true;
        }
        else {
            return check_specific_animal(found_tree->entities_root, entities);
        }
    }
}

bool check_any_tree(Tree trees_bst_root, InputEntities entities) {
    if (!trees_bst_root) return false;

    bool res;
    res = check_specific_animal(trees_bst_root->entities_root, entities);

    if (!res && trees_bst_root->left) {
        res = check_any_tree(trees_bst_root->left, entities);
    }

    if (!res && trees_bst_root->right) {
        res = check_any_tree(trees_bst_root->right, entities);
    }

    return res;
}

bool check_any_forest(Forest forests_bst_root, InputEntities entities) {
    if (!forests_bst_root) return false;

    bool res;
    if (strcmp(entities->tree, "*") == 0) {
        res = check_any_tree(forests_bst_root->entities_root, entities);
    }
    else {
        res = check_specific_tree(forests_bst_root->entities_root, entities);
    }

    if (!res && forests_bst_root->left) {
        res = check_any_forest(forests_bst_root->left, entities);
    }

    if (!res && forests_bst_root->right) {
        res = check_any_forest(forests_bst_root->right, entities);
    }

    return res;
}

bool check_specific_forest(Forest forests_bst_root, InputEntities entities) {
    Forest found_forest = find(forests_bst_root, entities->forest);

    if (!found_forest) {
        return false;
    }
    else {
        if (!(entities->tree))  {
            return true;
        }
        else {
            if (strcmp(entities->tree, "*") == 0) {
                return check_any_tree(found_forest->entities_root, entities);
            }
            else {
                return check_specific_tree(found_forest->entities_root,
                                           entities);
            }
        }
    }
}
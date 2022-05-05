#include "del.h"
#include "remove.h"

BstTree delete_animal(Animal animals_bst_root, InputEntities entities) {
    return delete(animals_bst_root, entities->animal);
}

BstTree delete_tree(Tree trees_bst_root, InputEntities entities) {
    Tree found_tree = find(trees_bst_root, entities->tree);

    if (found_tree) {
        if (entities->animal) {
            found_tree->entities_root = delete_animal(found_tree->entities_root,
                                                      entities);
        }
        else {
            // If certain animal wasn't specified whole tree is deleted.
            remove_animals_bst(trees_bst_root->entities_root);
            return delete(trees_bst_root, entities->tree);
        }
    }

    return trees_bst_root;
}

BstTree delete_forest(Forest forests_bst_root, InputEntities entities) {
    Forest found_forest = find(forests_bst_root, entities->forest);

    if (found_forest) {
        if (entities->tree) {
            found_forest->entities_root = delete_tree(
                    found_forest->entities_root, entities);
        }
        else {
            // If certain tree wasn't specified whole forest is deleted.
            remove_trees_bst(found_forest->entities_root);
            return delete(forests_bst_root, entities->forest);
        }
    }

    return forests_bst_root;
}

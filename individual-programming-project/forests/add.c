#include "add.h"
#include "remove.h"
#include <stdlib.h>

Animal add_animal(Animal animals_node, InputEntities entities) {
    Animal new_animal = insert(animals_node, entities->animal);

    // If fatal error occurred
    if (!new_animal) {
        // We won't be able to tell later if 'entities->animal' has been freed,
        // so we have to do it now.
        free(entities->animal);
        entities->animal = NULL;
        return NULL;
    }
    else {
        // If animal with this name already exists.
        if (entities->animal != new_animal->value) {
            free(entities->animal);
            entities->animal = NULL;
        }
        return new_animal;
    }
}

Tree add_tree(Tree trees_node, InputEntities entities) {
    Tree new_tree = insert(trees_node, entities->tree);
    Animal new_animal = NULL;

    // If there is animal to add and inserting tree succeeded.
    if (new_tree && entities->animal) {
        if (!(new_tree->entities_root)) {
            new_tree->entities_root = create_node(entities->animal);
            new_animal = new_tree->entities_root;
        }
        else {
            new_animal = add_animal(new_tree->entities_root, entities);
        }
    }

    if (!new_tree || (entities->animal && !new_animal)) {
        if (!new_tree) {
            // We won't be able to tell later if 'entities->tree' has been
            // freed, so we have to do it now.
            free(entities->tree);
            entities->tree = NULL;
        }
        return NULL;
    }
    else if (entities->tree != new_tree->value) {
        free(entities->tree);
        entities->tree = NULL;
    }

    return new_tree;
}

Forest add_forest(Tree forests_bst_root, InputEntities entities) {
    Forest new_forest = NULL;
    Tree new_tree = NULL;

    // If there are no forests.
    if (!forests_bst_root) {
        forests_bst_root = create_node(entities->forest);
        new_forest = forests_bst_root;
    }
    else {
        new_forest = insert(forests_bst_root, entities->forest);
    }

    // If there is tree to add and inserting forest succeeded.
    if (new_forest && entities->tree) {
        if (!(new_forest->entities_root)) {
            new_forest->entities_root = create_node(entities->tree);
        }
        new_tree = add_tree(new_forest->entities_root, entities);
    }

    if (!new_forest || (entities->tree && !new_tree)) {
        if (!new_forest) {
            // We won't be able to tell later if 'entities->forest' has been
            // freed, so we have to do it now.
            free(entities->forest);
            entities->forest = NULL;
        }

        // On fatal error function frees all memory used to store forests,
        // animals and trees.
        remove_forests_bst(forests_bst_root);
        return NULL;
    }
    else if (entities->forest != new_forest->value) {
        free(entities->forest);
        entities->forest = NULL;
    }

    return forests_bst_root;
}
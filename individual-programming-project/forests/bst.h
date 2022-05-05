/** @file
 * BST tree interface, with related types and structure for entities stored in
 * it.
 */

#ifndef BST_H
#define BST_H

typedef struct Node* BstTree;

typedef struct Node* Forest;

typedef struct Node* Animal;

typedef struct Node* Tree;

typedef struct InputForest* InputEntities;

struct InputForest {
    char* forest;
    char* tree;
    char* animal;
};

struct Node {
    char* value;
    BstTree left;
    BstTree right;
    // Root of BST tree storing lower (from current) level entities. (for
    // forests it will be trees, for trees animals, for animals this value
    // will always be 'NULL'.
    BstTree entities_root;
};

/**
 * @return new node with 'value' parameter sto to 'new_value', other parameters
 *         are set to 'NULL'.
 */
BstTree create_node(char* new_value);

/**
 * @return Pointer to node containing 'entity' (either if it has been just
 *         created or it existed), or NULL if malloc didn't succeed.
 */
BstTree insert(BstTree node, char* entity);

/**
 * @return minimal value stored in 'node' or in its subtree.
 */
BstTree min_value(BstTree node);

/**
 * @return Root of given tree after deletion.
 */
BstTree delete(BstTree root, char* entity);

/**
 * @return Pointer to node containing 'entity'
 * @return 'NULL' if it hasn't been found.
 */
BstTree find(BstTree node, char* entity);

#endif //BST_H

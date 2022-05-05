#include "bst.h"
#include <stdlib.h>
#include <string.h>

BstTree create_node(char* new_value) {
    BstTree new_node = malloc(sizeof(struct Node));

    if (!new_node) {
        return NULL;
    }
    else {
        new_node->value = new_value;
        new_node->left = NULL;
        new_node->right = NULL;
        new_node->entities_root = NULL;

        return new_node;
    }
}

BstTree insert(BstTree node, char* entity) {
    int strcmp_result = strcmp(node->value, entity);

    if (strcmp_result < 0) {
        if (!(node->right)) {
            BstTree new_node = create_node(entity);

            if (!new_node) {
                return NULL;
            }
            else {
                node->right = new_node;
                return new_node;
            }
        }
        else {
            return insert(node->right, entity);
        }
    }
    else if (strcmp_result > 0) {
        if (!(node->left)) {
            BstTree new_node = create_node(entity);

            if (!new_node) {
                return NULL;
            }
            else {
                node->left = new_node;
                return new_node;
            }
        }
        else {
            return insert(node->left, entity);
        }
    }

    return node;
}

BstTree min_value(BstTree node){
    BstTree temp = node;
    while (temp->left != NULL)
        temp = temp->left;
    return temp;
}

BstTree delete(BstTree node, char* entity) {
    if (!node) return NULL;
    int strcmp_result = strcmp(node->value, entity);

    if(strcmp_result > 0) {
        node->left = delete(node->left, entity);
    }
    else if (strcmp_result < 0) {
        node->right = delete(node->right, entity);
    }
    else {
        if (!(node->left) && !(node->right)) {
            free(node->value);
            free(node);
            node = NULL;
        }
        else if(!(node->left)) {
            BstTree tmp = node->right;
            free(node->value);
            free(node);
            node = tmp;
        }
        else if(!(node->right)) {
            BstTree tmp = node->left;
            free(node->value);
            free(node);
            node = tmp;
        }
        else {
            BstTree tmp = min_value(node->right);
            char* tmp2 = tmp->value;
            tmp->value = node->value;
            node->value = tmp2;
            node->entities_root = tmp->entities_root;
            node->right = delete(node->right, entity);
        }
    }

    return node;
}

BstTree find(BstTree node, char* entity) {
    if (!node) return NULL;
    int strcmp_result = strcmp(node->value, entity);

    if (strcmp_result == 0) {
        return node;
    }
    else if (strcmp_result > 0) {
        return find(node->left, entity);
    }
    else {
        return find(node->right, entity);
    }
}
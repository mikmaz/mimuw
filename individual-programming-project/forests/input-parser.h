/** @file
 * Interface for parsing the input.
 */

#ifndef INPUT_PARSER_H
#define INPUT_PARSER_H

#include "bst.h"
#include <stdbool.h>

/**
 * Structure storing parsed single input line (one that ends with '\n').
 */
typedef struct {
    char *command;
    char *forest;
    char *tree;
    char *animal;
} ParsedInputLine;

/**
 *
 * @param parsed_line - pointer to structure which will store parsed input line.
 *        Initially all of its parameters should be set to 'NULL'.
 */
void parse_input_line(ParsedInputLine *parsed_line);

/**
 * Determines whether command should be ignored or produces an error. If non
 * form the above, function executes command on forest. After its
 * runtime all members of 'parsed_line' and 'entities' are set to 'NULL'.
 * @param parsed_line - structure storing parsed input line.
 * @param forests_root - root of BST tree storing forests and tress and animals
 *        in them.
 * @param entities - structure which will be used if commands are proper.
 *        Initially all of its members should be set to 'NULL'.
 * @return root of forests BST tree.
 * @return 'NULL' if forests BST tree is empty.
 * @return BST tree node with all its values set to 'NULL' if fatal error
 *         occurred.
 */
Forest examine_and_execute_command(ParsedInputLine *parsed_line,
                                   Forest forests_root, InputEntities entities);

void set_parsed_words_to_null(ParsedInputLine *parsed_line);

void null_input_entities(InputEntities entities);

#endif //INPUT_PARSER_H

#include "input-parser.h"
#include "remove.h"
#include <stdlib.h>
#include <string.h>

int main() {
    ParsedInputLine *parsed_line = malloc(sizeof(ParsedInputLine));
    if (!parsed_line)
        return 1;

    set_parsed_words_to_null(parsed_line);

    InputEntities entities = malloc(sizeof(struct InputForest));
    if (!entities) {
        free(parsed_line);
        return 1;
    }

    null_input_entities(entities);

    parse_input_line(parsed_line);
    Forest forests_root = NULL;

    while (strcmp(parsed_line->command, "EOF") != 0) {
        forests_root = examine_and_execute_command(parsed_line, forests_root, entities);

        // If fatal error occurred function above returns node with its
        // parameters set to 'NULL'.
        if (forests_root && !forests_root->value) {
            free(forests_root);
            free(entities);
            free(parsed_line);
            return 1;
        }

        parse_input_line(parsed_line);
    }

    remove_forests_bst(forests_root);
    if (parsed_line->command)
        free(parsed_line->command);
    free(entities);
    free(parsed_line);

    return 0;
}

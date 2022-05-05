#include "input-parser.h"
#include "remove.h"
#include "check.h"
#include "print.h"
#include "del.h"
#include "add.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int check_input_char(int c) {
    if(c == 35) return 2; // '#' character
    if (c >= 33 && c <= 255) return 1; // any proper non-whitespace character
    // When 'c' is whitespace character.
    if (c == 32 || c == 9 || c == 11 || c== 12 || c == 13) return 0;
    return -1; // if character is invalid
}

void set_parsed_words_to_null(ParsedInputLine *parsed_line) {
    parsed_line->command = NULL;
    parsed_line->forest = NULL;
    parsed_line->tree = NULL;
    parsed_line->animal = NULL;
}

void free_parsed_words(ParsedInputLine *parsed_line) {
    if (parsed_line->command) free(parsed_line->command);
    if (parsed_line->forest) free(parsed_line->forest);
    if (parsed_line->tree) free(parsed_line->tree);
    if (parsed_line->animal) free(parsed_line->animal);

    set_parsed_words_to_null(parsed_line);
}

void add_parsed_word(char *parsed_word, ParsedInputLine *parsed_line) {
    if (!parsed_line->command) {
        parsed_line->command = parsed_word;
    }
    else if (!parsed_line->forest) {
        parsed_line->forest = parsed_word;
    }
    else if (!parsed_line->tree) {
        parsed_line->tree = parsed_word;
    }
    else if (!parsed_line->animal) {
        parsed_line->animal = parsed_word;
    }
}

void set_error_command(ParsedInputLine *parsed_line) {
    free_parsed_words(parsed_line);

    parsed_line->command = malloc(6);
    char *error = "ERROR";

    int i;
    for (i = 0; i < 6; i++)
        parsed_line->command[i] = error[i];
}

void set_ignore_command(ParsedInputLine *parsed_line) {
    free_parsed_words(parsed_line);

    parsed_line->command = malloc(7);
    char *ignore = "IGNORE";

    int i;
    for (i = 0; i < 7; i++)
        parsed_line->command[i] = ignore[i];
}

void set_eof_command(ParsedInputLine *parsed_line) {
    free_parsed_words(parsed_line);

    parsed_line->command = malloc(4);
    char *eof = "EOF";

    int i;
    for (i = 0; i < 4; i++)
        parsed_line->command[i] = eof[i];
}

void parse_input_line(ParsedInputLine *parsed_line) {
    set_parsed_words_to_null(parsed_line);

    unsigned int buff_size = 1; // size of buffer storing words from input
    unsigned int word_length = 0; // length of currently read word
    int char_type;
    int words_read = 0; // how many words has been already read
    int c = EOF;
    char *tmp = NULL;
    bool first_char = true;
    char *current_word = malloc(buff_size); // pointer to currently read word
    if (!current_word)
        return;

    while ((c = getchar()) != '\n' && c != EOF) {
        char_type = check_input_char(c);

        // 'c' is '#' character.
        if (char_type == 2) {
            // if its first character of input line it can be ignored
            if (first_char) {
                while ((c = getchar()) != '\n' && c != EOF) {}
                break;
            }
            else {
                // Treat it as any other proper non-whitespace character.
                char_type = 1;
            }
        }

        // If 'c' is wrong character or there is too many words.
        if ((char_type == 1 && words_read == 4) || char_type == -1) {
            set_error_command(parsed_line);
            free(current_word);
            while ((c = getchar()) != '\n' && c != EOF) {}

            return;
        }

        //If c is proper non-whitespace character.
        if (char_type == 1) {
            current_word[word_length++] = (char)c;

            // If buffer is full, extend it.
            if(word_length == buff_size) {
                buff_size = word_length * 2;
                tmp = realloc(current_word, buff_size);

                if (!tmp) {
                    free_parsed_words(parsed_line);
                    free(current_word);
                    return;
                }
                else {
                    current_word = tmp;
                    memset(current_word + word_length, 0,
                            buff_size - word_length);
                }
            }
        }
        else if (word_length != 0) { // if we reached end of the word
            current_word[word_length] = '\0';
            add_parsed_word(current_word, parsed_line);

            words_read += 1;
            buff_size = 1;
            word_length = 0;

            current_word = malloc(sizeof(buff_size));
            if (!current_word) {
                free_parsed_words(parsed_line);
                return;
            }
        }

        first_char = false;
    }

    // If line was empty or it was a comment.
    if (word_length == 0 && words_read == 0 && c != EOF) {
        set_ignore_command(parsed_line);
        return;
    }

    if (c == EOF) {
        if (word_length == 0 && words_read == 0)
            set_eof_command(parsed_line);
        else // if line ended with EOF and not with '\n' character
            set_error_command(parsed_line);
        return;
    }

    // If there was left word to save.
    if (word_length != 0) {
        if (current_word[word_length] != '\0')
            current_word[word_length] = '\0';

        add_parsed_word(current_word, parsed_line);
    }

    // Since 'IGNORE' and 'EOF' will have special meaning as first words, if
    // they were given in input we have set error now.
    if (strcmp(parsed_line->command, "IGNORE") == 0 ||
            strcmp(parsed_line->command, "EOF") == 0) {
        set_error_command(parsed_line);
        return;
    }
}

void set_entities(ParsedInputLine *parsed_line, InputEntities entities) {
    entities->forest = parsed_line->forest;
    entities->tree = parsed_line->tree;
    entities->animal = parsed_line->animal;
}

void null_input_entities(InputEntities entities) {
    entities->forest = NULL;
    entities->tree = NULL;
    entities->animal = NULL;
}

void free_entities(InputEntities entities) {
    if (entities->forest) free(entities->forest);
    if (entities->tree) free(entities->tree);
    if (entities->animal) free(entities->animal);
    null_input_entities(entities);
}

bool check_checking_command(ParsedInputLine *parsed_line) {
    if (parsed_line->animal) {
        if (strcmp(parsed_line->animal, "*") == 0) {
            return false;
        }
        else {
            return true;
        }
    }
    else if (parsed_line->tree) {
        if (strcmp(parsed_line->tree, "*") == 0) {
            return false;
        }
        else {
            return true;
        }
    }
    else if (!parsed_line->forest || strcmp(parsed_line->forest, "*") == 0) {
        return false;
    }
    else {
        return true;
    }
}

Forest examine_and_execute_command(ParsedInputLine *parsed_line,
        Forest forests_root, InputEntities entities) {
    int err = 0; // flag for fatal error
    Forest tmp = NULL;
    set_entities(parsed_line, entities);

    if (strcmp(parsed_line->command, "ADD") == 0) {
        if (!parsed_line->forest) {
            fprintf(stderr, "ERROR\n");
        }
        else {
            tmp = add_forest(forests_root, entities);
            if (!tmp)
                err = 1;
            else
                printf("OK\n");

            forests_root = tmp;
        }

        null_input_entities(entities);
    }
    else if (strcmp(parsed_line->command, "DEL") == 0) {
        if (entities->forest) {
            forests_root = delete_forest(forests_root, entities);
            free_entities(entities);
        }
        else {
            remove_forests_bst(forests_root);
            forests_root = NULL;
        }

        printf("OK\n");
    }
    else if (strcmp(parsed_line->command, "PRINT") == 0) {
        if (parsed_line->animal)
            fprintf(stderr, "ERROR\n");
        else
            print_forests(forests_root, entities);

        free_entities(entities);
    }
    else if (strcmp(parsed_line->command, "CHECK") == 0) {
        if (!check_checking_command(parsed_line)) {
            fprintf(stderr, "ERROR\n");
        }
        else {
            bool check_result;
            if (strcmp(entities->forest, "*") == 0)
                check_result = check_any_forest(forests_root, entities);
            else
                check_result = check_specific_forest(forests_root, entities);

            if (check_result)
                printf("YES\n");
            else
                printf("NO\n");
        }

        free_entities(entities);
    }
    else if (strcmp(parsed_line->command, "IGNORE") != 0) {
        fprintf(stderr, "ERROR\n");
        free_entities(entities);
    }

    free(parsed_line->command);
    parsed_line->command = NULL;
    set_parsed_words_to_null(parsed_line);

    if (err) {
        tmp = malloc(sizeof(struct Node));
        tmp->value = NULL;
        forests_root = tmp;
    }

    return forests_root;
}

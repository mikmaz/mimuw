CC = gcc
CFLAGS = -Wall -Wextra -std=c11 -O2
LDFLAGS =

.PHONY: all clean

all: forests

forests: main.o input-parser.o print.o del.o check.o add.o remove.o bst.o
	$(CC) $(LDFLAGS) -o $@ $^

bst.o: bst.c bst.h
	$(CC) $(CFLAGS) -c $<

remove.o: remove.c remove.h
	$(CC) $(CFLAGS) -c $<

add.o: add.c add.h remove.h
	$(CC) $(CFLAGS) -c $<

check.o: check.c check.h
	$(CC) $(CFLAGS) -c $<

del.o: del.c del.h remove.h
	$(CC) $(CFLAGS) -c $<

print.o: print.c print.h

input-parser.o: input-parser.c input-parser.h remove.h check.h print.h del.h add.h
	$(CC) $(CFLAGS) -c $<

main.o: main.c input-parser.h remove.h
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o forests

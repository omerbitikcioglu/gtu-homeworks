CC=gcc
CFLAGS=-g -Wall -Wextra
LDFLAGS=-pthread

all: pCp

pCp: main.c
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

clean:
	rm -rf pCp

.PHONY: all
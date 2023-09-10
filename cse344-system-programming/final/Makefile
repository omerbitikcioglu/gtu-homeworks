CC=gcc
CFLAGS=-g -Wall -Wextra
LDFLAGS=-pthread

all: BibakBOXClient BibakBOXServer

BibakBOXClient: client.c utils.c
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

BibakBOXServer: server.c utils.c
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

clean:
	rm -f BibakBOXServer BibakBOXClient

.PHONY: all
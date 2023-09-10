CC=gcc
CFLAGS=-Wall -Wextra
LDFLAGS=

all: appendMeMore myDup checkOffset

appendMeMore: part1.c
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

myDup: part2.c
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

checkOffset: part3.c
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

clean:
	rm -f appendMeMore myDup checkOffset

.PHONY: all
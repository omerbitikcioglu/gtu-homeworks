CC=gcc
CFLAGS=-g -Wall -Wextra
LDFLAGS=

all: biboServer biboClient

biboClient: client.c client.h bibo_cl_sv.h
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

biboServer: server.c bibo_cl_sv.h client_queue.h request_queue.h
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

clean:
	rm -f biboServer biboClient *.log

.PHONY: all
#ifndef MIDTERM_CLIENT_H
#define MIDTERM_CLIENT_H

#include <sys/types.h>

typedef struct client {
    int id;
    pid_t pid;
    char *fifo_req_name;
    char *fifo_res_name;
} client_t;

#endif //MIDTERM_CLIENT_H

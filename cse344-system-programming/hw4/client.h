#ifndef MIDTERM_CLIENT_H
#define MIDTERM_CLIENT_H

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "bibo_cl_sv.h"
#include "request_queue.h"

typedef struct client {
    int id;
    pid_t pid;
    char *fifo_req_name;
    char *fifo_res_name;
} client_t;

client_t *createClient(pid_t cl_pid, int cl_count) {
    /* Get client info */
    client_t *cl = (client_t *) malloc(sizeof(client_t));
    if(cl) {
        cl->id = cl_count;
        cl->pid = cl_pid;

        /* Get client read fifo name */
        cl->fifo_req_name = (char *) malloc(sizeof(CLIENT_FIFO_NAME_LEN));
        snprintf(cl->fifo_req_name, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_REQ_TEMPLATE, cl->pid);

        /* Get client write fifo name */
        cl->fifo_res_name = (char *) malloc(sizeof(CLIENT_FIFO_NAME_LEN));
        snprintf(cl->fifo_res_name, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_RES_TEMPLATE, cl->pid);
    }
    return cl;
}

#endif //MIDTERM_CLIENT_H

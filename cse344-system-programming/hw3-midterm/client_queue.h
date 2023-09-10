#include <stdlib.h>
#include <stdio.h>
#include "client.h"
#include "bibo_cl_sv.h"

#ifndef MIDTERM_CLIENT_QUEUE_H
#define MIDTERM_CLIENT_QUEUE_H

typedef struct client_node {
    client_t *cl;
    struct client_node *next;
} client_node_t;

typedef struct client_queue {
    client_node_t* head;
    client_node_t* tail;
} client_queue_t;

/* A utility function to create a new client node */
client_node_t* newNode(pid_t cl_pid)
{
    client_t* new_cl = (client_t*) malloc(sizeof(client_t));
    client_node_t* new_cl_node = (client_node_t*) malloc(sizeof(client_node_t));

    new_cl->pid = cl_pid;

    /* Get client read fifo name */
    char *cl_fifo_req_name = (char *) malloc(sizeof(CLIENT_FIFO_NAME_LEN));
    snprintf(cl_fifo_req_name, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_REQ_TEMPLATE, cl_pid);
    new_cl->fifo_req_name = cl_fifo_req_name;

    /* Get client write fifo name */
    char *cl_fifo_res_name = (char *) malloc(sizeof(CLIENT_FIFO_NAME_LEN));
    snprintf(cl_fifo_res_name, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_RES_TEMPLATE, cl_pid);
    new_cl->fifo_res_name = cl_fifo_res_name;

    new_cl_node->cl = new_cl;
    new_cl_node->next = NULL;
    return new_cl_node;
}

/* Create an empty client queue */
client_queue_t* createQueue()
{
    client_queue_t* q = (client_queue_t*) malloc(sizeof(client_queue_t));
    q->head = q->tail = NULL;
    return q;
}

/* Add a client to the queue */
void enqueueClient(client_queue_t *q, client_t* cl) {
    /* Create a new client node */
    client_node_t* new_node = (client_node_t *) malloc(sizeof(client_node_t));
    new_node->cl = cl;
    new_node->next = NULL;

    /* Add the new node to the tail of the queue */
    if (q->tail == NULL) {
        q->head = new_node;
    } else {
        q->tail->next = new_node;
    }
    q->tail = new_node;
}

/* Remove and return the first client from the queue */
client_t *dequeueClient(client_queue_t *q) {
    if (q->head == NULL) { /* Queue is empty */
        return NULL;
    } else {
        /* Remove the first node from the queue */
        client_node_t *node = q->head;
        q->head = q->head->next;
        if (q->head == NULL) {
            q->tail = NULL;
        }
        /* Get the client file descriptor and free the node */
        client_t *cl = node->cl;
        free(node);
        return cl;
    }
}

/* Check if the given cl_pid is in the queue or not */
int isClientInQueue(client_queue_t* queue, pid_t cl_pid) {
    client_node_t* current = queue->head;
    while (current != NULL) {
        if (current->cl->pid == cl_pid) {
            return 1; /* Found */
        }
        current = current->next;
    }
    return 0; /* Not found */
}


#endif //MIDTERM_CLIENT_QUEUE_H

#include <stdlib.h>
#include <stdio.h>
#include "client.h"
#include "bibo_cl_sv.h"

#ifndef CLIENT_QUEUE_H
#define CLIENT_QUEUE_H

typedef struct client_node_t {
    client_t *cl;
    struct client_node_t *next;
} client_node_t;

typedef struct {
    client_node_t* head;
    client_node_t* tail;
} client_queue_t;

/* Create an empty client queue */
client_queue_t* createClientQueue()
{
    client_queue_t* q = (client_queue_t*) malloc(sizeof(client_queue_t));
    if(q != NULL)
    {
        q->head = q->tail = NULL;
    }
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
int isClientInQueue(client_queue_t* cl_q, client_t *cl) {
    client_node_t* current = cl_q->head;
    while (current != NULL) {
        client_t *cur_cl = current->cl;
        if (cur_cl->pid == cl->pid) {
            return 1; /* Found */
        }
        current = current->next;
    }
    return 0; /* Not found */
}

void freeClientQueue(client_queue_t *cl_q) {
    client_node_t *current = cl_q->head;
    client_node_t *temp_node;
    while (current != NULL) {
        client_t *cl = current->cl;
        free(cl);
        temp_node = current->next;
        free(current);
        current = temp_node;
    }
    free(cl_q);
}


#endif //CLIENT_QUEUE_H

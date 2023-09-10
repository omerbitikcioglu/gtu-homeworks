#ifndef REQUEST_QUEUE_H
#define REQUEST_QUEUE_H

#include <stdlib.h>
#include <stdio.h>
#include "client.h"
#include "bibo_cl_sv.h"

typedef struct cl_req_node_t {
    request_t *cl_req;
    struct cl_req_node_t *next;
} cl_req_node_t;

typedef struct {
    cl_req_node_t* head;
    cl_req_node_t* tail;
} cl_req_queue_t;

/* Create an empty client request queue */
cl_req_queue_t* createClientRequestQueue()
{
    cl_req_queue_t* q = (cl_req_queue_t*) malloc(sizeof(cl_req_queue_t));
    if(q != NULL)
    {
        q->head = q->tail = NULL;
    }
    return q;
}

/* Add a new client request to the queue */
void enqueueClientRequest(cl_req_queue_t *q, request_t *cl_req) {
    cl_req_node_t *cl_req_node = (cl_req_node_t *) malloc(sizeof(cl_req_node_t));

    cl_req_node->cl_req = cl_req;
    cl_req_node->next = NULL;

    /* Add the new request node to the tail of the queue */
    if (q->tail == NULL) {
        q->head = cl_req_node;
    } else {
        q->tail->next = cl_req_node;
    }
    q->tail = cl_req_node;
}

/* Remove and return the first client request from the queue */
request_t *dequeueClientRequest(cl_req_queue_t *q) {
    if (q->head == NULL) { /* Queue is empty */
        return NULL;
    } else {
        /* Remove the first node from the queue */
        cl_req_node_t *node = q->head;
        q->head = q->head->next;
        if (q->head == NULL) {
            q->tail = NULL;
        }
        /* Get the client request and free the node */
        request_t *cl_req = node->cl_req;
        free(node);
        return cl_req;
    }
}

void freeClientRequestQueue(cl_req_queue_t *q) {
    cl_req_node_t *current = q->head;
    cl_req_node_t *temp;
    while (current != NULL) {
        request_t *req = current->cl_req;
        free(req);
        temp = current->next;
        free(current);
        current = temp;
    }
    free(q);
}

#endif //REQUEST_QUEUE_H

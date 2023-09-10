#ifndef MIDTERM_BIBO_CL_SV_H
#define MIDTERM_BIBO_CL_SV_H

#include <unistd.h>

#define CLIENT_FIFO_REQ_TEMPLATE "/tmp/bibo_cl_req.%d"
#define CLIENT_FIFO_RES_TEMPLATE "/tmp/bibo_cl_res.%d"
#define CLIENT_FIFO_NAME_LEN (sizeof(CLIENT_FIFO_REQ_TEMPLATE) + 20)

#define SERVER_FIFO_TEMPLATE "/tmp/bibo_sv.%d"
#define SERVER_FIFO_NAME_LEN (sizeof(SERVER_FIFO_TEMPLATE) + 20)

#define SERVER_LOG_TEMPLATE "sv_%d.log"
#define SERVER_LOG_NAME_LEN (sizeof(SERVER_LOG_TEMPLATE) + 20)

#define MAX_RESPONSE_LEN 1024 // 1 KB

typedef struct request_t {
    char cmd[100];
    pid_t cl_pid;
} request_t;

typedef enum response_code_e {
    OK,
    WAIT,
    FAIL
} response_code_e;

typedef struct response_t {
    char msg[MAX_RESPONSE_LEN];
    response_code_e code;
} response_t;

#endif //MIDTERM_BIBO_CL_SV_H

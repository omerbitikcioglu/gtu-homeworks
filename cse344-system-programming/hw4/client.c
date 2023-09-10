/*
 *
 * System Programming - HW4
 *
 * biboClient source code
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/stat.h>
#include <errno.h>
#include "client.h"
#include "bibo_cl_sv.h"

/* Write request to the opened fifo */
int sendRequest(int fifo_fd, request_t *req) {
    int status = write(fifo_fd, req, sizeof(request_t));
    if (status == -1) {
        perror("write req to fifo");
        exit(1);
    }
    return status;
}

/* Read response from the opened fifo */
response_t *getResponse(int fifo_fd) {
    response_t *res = (response_t *) malloc(sizeof(response_t));
    if (read(fifo_fd, res, sizeof(response_t)) == -1) {
        perror("Error read response");
        free(res);
        exit(1);
    }
    return res;
}

/* Signal handler for ^C (SIGINT) */
void signintHandler() {
    char cl_fifo_req_name[CLIENT_FIFO_NAME_LEN];
    snprintf(cl_fifo_req_name, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_REQ_TEMPLATE, getpid());

    char cl_fifo_res_name[CLIENT_FIFO_NAME_LEN];
    snprintf(cl_fifo_res_name, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_RES_TEMPLATE, getpid());

    /* Unlink client fifos */
    unlink(cl_fifo_req_name);
    unlink(cl_fifo_res_name);

    kill(getpid(), SIGKILL);
}

/* Send connection request to the server
 * sv_fifo_name: fifo name of the server
 * req_type: "Connect" or "tryConnect" */
void connectServer(client_t cl, char *sv_fifo, char *req_type) {
    request_t *req = (request_t *) malloc(sizeof(request_t));
    req->cl_pid = cl.pid;
    strcpy(req->cmd, req_type);
    int sv_fd = open(sv_fifo, O_WRONLY);
    if (sv_fd == -1) {
        perror("open");
        exit(1);
    }
    sendRequest(sv_fd, req);
    free(req);
    close(sv_fd);
}

/* Call when connection to server is established
 * cl_pid: PID of the client
 * cl_fifo_req_name: Send request to server using this fifo
 * cl_fifo_res_name: Get response from server using this fifo */
void connectionEstablished(client_t cl, char *sv_fifo) {
    printf("Connection established:\n");
    int connected = 1;
    while (connected) {
        request_t *req = (request_t *) malloc(sizeof(request_t));
        req->cl_pid = cl.pid;
        /* Read the command to be executed on server */
        if (fgets(req->cmd, sizeof(req->cmd), stdin) == NULL) {
            printf("Error reading input command!\n");
            unlink(cl.fifo_res_name);
            unlink(cl.fifo_req_name);
            exit(1);
        }
        char *newline_ptr = strchr(req->cmd, '\n');
        if (newline_ptr != NULL) {
            *newline_ptr = '\0';
        }
        /* Send request to execute the command on server */
        int sv_fd = open(sv_fifo, O_WRONLY);
        if (sv_fd == -1) {
            perror("Error open client request fifo");
            exit(1);
        }
        sendRequest(sv_fd, req);
        free(req);
        close(sv_fd);
        /* Get response from the server */
        int cl_fd = open(cl.fifo_res_name, O_RDONLY);
        if (cl_fd == -1) {
            perror("Error open client response fifo");
            exit(1);
        }
        response_t *res = getResponse(cl_fd);
        printf("%s\n", res->msg);
        fflush(stdout);
        if (strcmp(res->msg, "Quiting") == 0
            || strcmp(res->msg, "Killing") == 0) {
            connected = 0;
        }
        free(res);
        close(cl_fd);
    }
}

int main(int argc, char *argv[]) {
    /* Check argument count, print usage */
    if (argc != 3) {
        fflush(stdout);
        fprintf(stderr,
                "Usage: %s <Connect/tryConnect> ServerPID\n", argv[0]);
        fflush(stderr);
        exit(1);
    }

    /* Check Connect/tryConnect arg, print usage */
    if (strcmp(argv[1], "Connect") != 0
        && strcmp(argv[1], "tryConnect") != 0) {
        fflush(stdout);
        fprintf(stderr,
                "Usage: %s <Connect/tryConnect> ServerPID\n"
                "You typed %s instead of Connect/tryConnect.\n", argv[0], argv[1]);
        fflush(stderr);
        exit(1);
    }

    /* Set up signal handler for SIGINT */
    struct sigaction sa_1;
    sa_1.sa_handler = signintHandler;
    sigemptyset(&sa_1.sa_mask);
    sa_1.sa_flags = 0;
    if (sigaction(SIGINT, &sa_1, NULL) == -1) {
        perror("Error setting up signal handler");
        exit(1);
    }

    char *ptr;
    pid_t sv_pid = strtol(argv[2], &ptr, 10);

    /* Get server fifo name */
    char sv_fifo_name[SERVER_FIFO_NAME_LEN];
    snprintf(sv_fifo_name, SERVER_FIFO_NAME_LEN, SERVER_FIFO_TEMPLATE, sv_pid);

    /* Check the existence of the server process and its fifo file */
    if (kill(sv_pid, 0) != 0
        || access(sv_fifo_name, F_OK) == -1) {
        fflush(stdout);
        fprintf(stderr,
                "Server process PID %d is not ready!\n", sv_pid);
        fflush(stderr);
        exit(1);
    }

    client_t client;
    client.pid = getpid();

    /* Get client req fifo name */
    char cl_fifo_req_name[CLIENT_FIFO_NAME_LEN];
    snprintf(cl_fifo_req_name, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_REQ_TEMPLATE, client.pid);
    client.fifo_req_name = cl_fifo_req_name;

    /* Create client request FIFO */
    if (mkfifo(client.fifo_req_name, S_IRUSR | S_IWUSR) == -1
        && errno != EEXIST) {
        perror("mkfifo");
        exit(1);
    }

    /* Get client res fifo name */
    char cl_fifo_res_name[CLIENT_FIFO_NAME_LEN];
    snprintf(cl_fifo_res_name, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_RES_TEMPLATE, client.pid);
    client.fifo_res_name = cl_fifo_res_name;

    /* Create client response FIFO */
    if (mkfifo(client.fifo_res_name, S_IRUSR | S_IWUSR) == -1
        && errno != EEXIST) {
        perror("mkfifo");
        exit(1);
    }

    /* Send connection request to the server */
    connectServer(client, sv_fifo_name, argv[1]);
    /* Get connection response */
    response_t *res;
    int cl_fifo_fd = open(client.fifo_res_name, O_RDONLY);
    if (cl_fifo_fd == -1) {
        perror("Error open client res fifo");
        exit(1);
    }
    res = getResponse(cl_fifo_fd);
    close(cl_fifo_fd);

    if (res->code == OK) {
        connectionEstablished(client, sv_fifo_name);
    } else if (res->code == WAIT) {
        printf("Waiting for Queue...");
        fflush(stdout);
        /* Wait for queue from server */
        int cl_fifo_fd = open(client.fifo_res_name, O_RDONLY);
        if (cl_fifo_fd == -1) {
            perror("Error open client res fifo");
            exit(1);
        }
        response_t *res = getResponse(cl_fifo_fd);
        if(res) {
            free(res);
            close(cl_fifo_fd);
            connectionEstablished(client, sv_fifo_name);
        }
    } else {
        printf("Connection failed!\n");
    }

    fflush(stdout);
    unlink(client.fifo_res_name);
    unlink(client.fifo_req_name);
    return 0;
}
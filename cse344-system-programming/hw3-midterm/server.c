/*
 *
 * System Programming Midterm
 *
 * biboServer source code
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/file.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <dirent.h>
#include "bibo_cl_sv.h"
#include "client_queue.h"

/* Signal handler for ^C (SIGINT) */
void signint_handler() {
    /* Unlink server fifo */
    char sv_fifo_name[SERVER_FIFO_NAME_LEN];
    snprintf(sv_fifo_name, SERVER_FIFO_NAME_LEN, SERVER_FIFO_TEMPLATE, getpid());
    unlink(sv_fifo_name);

    killpg(getpid(), SIGKILL);
}

/* Send the given response to the client process
 * returns -1 on fail, 0 on success */
int sendResponseToClient(char *cl_fifo_name, response_t *res) {
    int cl_fd = open(cl_fifo_name, O_WRONLY);
    if (cl_fd == -1) {
        perror("open");
        return -1;
    }
    if (write(cl_fd, res, sizeof(response_t)) != sizeof(response_t)) {
        perror("Error write response to client");
        return -1;
    }
    close(cl_fd);
    free(res);
    return 0;
}

void processRequestList(char *sv_dir, response_t *res) {
    DIR *directory = opendir(sv_dir);
    if (directory == NULL) {
        perror("Error open sv_dir");
        _exit(1);
    }
    struct dirent *entry;
    strcpy(res->msg, "");
    while ((entry = readdir(directory)) != NULL) {
        strcat(res->msg, entry->d_name);
        strcat(res->msg, " ");
    }
    closedir(directory);
}

char *processReadFile(const char *filename, int line_num) {
    char *res = (char *) malloc(1024);
    strcpy(res, "");

    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
        fprintf(stderr, "Failed to open file: %s\n", filename);
        free(res);
        return NULL;
    }
    /* Acquire a shared lock on the file */
    if (flock(fileno(fp), LOCK_SH) == -1) {
        perror("Failed to acquire lock");
        fclose(fp);
        return NULL;
    }

    if (line_num > 0) {
        /* Read a specific line */
        char buffer[256];
        int current_line = 1;
        while (fgets(buffer, sizeof(buffer), fp) != NULL) {
            if (current_line == line_num) {
                strcat(res, buffer);
                break;
            }
            current_line++;
        }
        if (current_line < line_num) {
            sprintf(buffer, "Line %d does not exist in the file.\n", line_num);
        }
    } else {
        /* Read the entire content of the file */
        char buffer[256];
        while (fgets(buffer, sizeof(buffer), fp) != NULL) {
            strcat(res, buffer);
        }
    }

    if (flock(fileno(fp), LOCK_UN) == -1) {
        perror("Failed to release lock");
        return NULL;
    }
    fclose(fp);

    return res;
}

int processWriteToFile(const char *filename, int line_num, const char *string) {
    FILE *fp = fopen(filename, "a+");
    if (fp == NULL) {
        fprintf(stderr, "Failed to open file: %s\n", filename);
        return -1;
    }
    /* Acquire a shared lock on the file */
    if (flock(fileno(fp), LOCK_SH) == -1) {
        perror("Failed to acquire lock");
        fclose(fp);
        return -1;
    }

    if (line_num > 0) {
        /* Write to a specific line */
        char buffer[256];
        FILE *temp = tmpfile();
        int current_line = 1;
        int found = 0;

        while (fgets(buffer, sizeof(buffer), fp) != NULL) {
            if (current_line == line_num) {
                fputs(string, temp);
                fputs("\n", temp);
                found = 1;
            } else {
                fputs(buffer, temp);
            }
            current_line++;
        }

        if (!found && current_line < line_num) {
            /* Add empty lines if the requested line does not exist */
            while (current_line < line_num) {
                fputs("\n", temp);
                current_line++;
            }
            fputs(string, temp);
            fputs("\n", temp);
        }

        /* Replace the original file with the updated contents */
        fseek(fp, 0, SEEK_SET);
        ftruncate(fileno(fp), 0);
        fseek(temp, 0, SEEK_SET);
        char ch;
        while ((ch = fgetc(temp)) != EOF) {
            fputc(ch, fp);
        }
        fclose(temp);
    } else {
        /* Write to the end of file */
        fputs(string, fp);
        fputs("\n", fp);
    }

    if (flock(fileno(fp), LOCK_UN) == -1) {
        perror("Failed to release lock");
        return -1;
    }
    fclose(fp);
    return 0;
}

void processRequestKillServer(client_t *cl, response_t *res) {
    printf("kill signal from client%d.. terminating...\n", cl->id);
    strcpy(res->msg, "Killing");
    sendResponseToClient(cl->fifo_res_name, res);

    /* Unlink server fifo */
    char sv_fifo_name[SERVER_FIFO_NAME_LEN];
    snprintf(sv_fifo_name, SERVER_FIFO_NAME_LEN, SERVER_FIFO_TEMPLATE, getppid());
    unlink(sv_fifo_name);

    /* Kill server and all of its child processes */
    if (killpg(getppid(), SIGKILL) == -1) {
        perror("killpg");
        _exit(EXIT_FAILURE);
    }
}

/* Serves the connected client's commands
 * cl: Client to be served
 * sv_dir: directory path of the server */
void serveClient(client_t *cl, char *sv_dir) {
    request_t req;
    int continueToServe = 1;
    while (continueToServe) {
        /* Open client fifo to receive commands */
        int cl_fd = open(cl->fifo_req_name, O_RDONLY);
        if (cl_fd == -1) {
            perror("Error open client req fifo");
            exit(1);
        }
        /* Read command from the request */
        if (read(cl_fd, &req, sizeof(request_t)) != sizeof(request_t)) {
            perror("Error read client request");
            close(cl_fd);
            continue;
        }
        close(cl_fd);

        /* Prepare the response and send it to the client */
        response_t *res = (response_t *) malloc(sizeof(response_t));

        char *req_word = strtok(req.cmd, " ");
        if (strcmp(req_word, "help") == 0) {

            strcpy(res->msg, "Available commands are :\n"
                             "help, list, readF, writeT, upload, download, quit, killServer");

        } else if (strcmp(req_word, "list") == 0) {

            processRequestList(sv_dir, res);

        } else if (strcmp(req_word, "readF") == 0) {

            /* Get file path */
            char filename[50];
            char *temp;
            strcpy(filename, sv_dir);
            strcat(filename, "/");
            temp = strtok(NULL, " ");
            strcat(filename, temp);

            /* Get line number */
            int line_num;
            temp = strtok(NULL, " ");
            if (temp == NULL)
                line_num = 0;
            else
                line_num = atoi(temp);

            char *result;
            result = processReadFile(filename, line_num);
            if (result) {
                strcpy(res->msg, result);
                free(result);
            }

        } else if (strcmp(req_word, "writeT") == 0) {

            /* Get file path */
            char filename[50];
            char *temp;
            strcpy(filename, sv_dir);
            strcat(filename, "/");
            temp = strtok(NULL, " ");
            strcat(filename, temp);

            /* Get line number */
            int line_num;
            temp = strtok(NULL, " ");
            if (temp == NULL)
                line_num = 0;
            else
                line_num = atoi(temp);

            /* Get the string */
            char str[1024*5];
            temp = strtok(NULL, "\n");
            strcpy(str, temp);

            processWriteToFile(filename, line_num, str);
            sprintf(res->msg, "Given string written to %s at line %d.", filename, line_num);

        } else if (strcmp(req_word, "upload") == 0) {

            strcpy(res->msg, "To be implemented.");

        } else if (strcmp(req_word, "download") == 0) {

            strcpy(res->msg, "To be implemented.");

        } else if (strcmp(req_word, "quit") == 0) {

            strcpy(res->msg, "Quiting");
            continueToServe = 0;

        } else if (strcmp(req_word, "killServer") == 0) {

            processRequestKillServer(cl, res);

        } else {

            sprintf(res->msg, "Invalid command[%s], type \"help\" to see available commands.", req_word);

        }
        sendResponseToClient(cl->fifo_res_name, res);
    }
}

int main(int argc, char *argv[]) {
    /* Check argument count, print usage */
    if (argc != 3) {
        fflush(stdout);
        fprintf(stderr,
                "Usage: %s <dirname> <max. #ofClients>\n", argv[0]);
        fflush(stderr);
        exit(1);
    }

    /* Set up signal handler for SIGINT */
    struct sigaction sa_1;
    sa_1.sa_handler = signint_handler;
    sigemptyset(&sa_1.sa_mask);
    sa_1.sa_flags = 0;
    if (sigaction(SIGINT, &sa_1, NULL) == -1) {
        perror("Error setting up signal handler");
        exit(1);
    }

    pid_t sv_pid = getpid();

    char *ptr;
    int max_connection = strtol(argv[2], &ptr, 10);

    char log_filename[SERVER_LOG_NAME_LEN];
    snprintf(log_filename, SERVER_LOG_NAME_LEN, SERVER_LOG_TEMPLATE, sv_pid);

    char sv_fifo_name[SERVER_FIFO_NAME_LEN];
    snprintf(sv_fifo_name, SERVER_FIFO_NAME_LEN, SERVER_FIFO_TEMPLATE, sv_pid);

    client_queue_t *client_queue = createQueue();

    /* Create directory, if it not exists */
    if (mkdir(argv[1], 0777) == -1 && errno != EEXIST) {
        perror("mkdir");
        exit(1);
    }

    /* Open the log file */
    FILE *log_fp = fopen(log_filename, "w+");
    if (log_fp == NULL) {
        fprintf(stderr, "Error: cannot create log file %s\n", log_filename);
        exit(1);
    }
    /* Use log file later, close for now */
    fclose(log_fp);

    /* Create server FIFO */
    if (mkfifo(sv_fifo_name, S_IRUSR | S_IWUSR) == -1
        && errno != EEXIST) {
        perror("mkfifo");
        exit(1);
    }

    printf("Server Started PID %d...\n", sv_pid);
    printf("waiting for clients...\n");
    request_t req;
    int connections = 0, clients = 0;
    /* Get connection requests from the clients */
    while (1) {
        client_t *cl;

        /* Wait for new connection requests */
        int sv_fd = open(sv_fifo_name, O_RDONLY);
        if (sv_fd == -1) {
            perror("Error open server fifo");
            exit(1);
        }
        if (read(sv_fd, &req, sizeof(request_t)) != sizeof(request_t)) {
            perror("Error read connection request");
            continue;
        }
        close(sv_fd);

        /* Connect to queued client if exists */
        if (strcmp(req.cmd, "Enqueue") == 0) {
            connections--;
            if (connections < max_connection && client_queue->head != NULL) {
                cl = dequeueClient(client_queue);
            } else {
                continue;
            }
        } else {
            /* Get client info */
            cl = (client_t *) malloc(sizeof(client_t));
            cl->pid = req.cl_pid;

            /* Get client read fifo name */
            cl->fifo_req_name = (char *) malloc(sizeof(CLIENT_FIFO_NAME_LEN));
            snprintf(cl->fifo_req_name, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_REQ_TEMPLATE, cl->pid);

            /* Get client write fifo name */
            cl->fifo_res_name = (char *) malloc(sizeof(CLIENT_FIFO_NAME_LEN));
            snprintf(cl->fifo_res_name, CLIENT_FIFO_NAME_LEN, CLIENT_FIFO_RES_TEMPLATE, cl->pid);
        }

        response_t *res = (response_t *) malloc(sizeof(response_t));
        if (connections == max_connection) {
            printf("Connection request PID %d... Queue FULL!\n", cl->pid);
            /* Add "Connect" request to the queue */
            if (!isClientInQueue(client_queue, cl->pid)
                && strcmp(req.cmd, "Connect") == 0) {
                enqueueClient(client_queue, cl);
                res->code = WAIT;
                sendResponseToClient(cl->fifo_res_name, res);
            } else { /* Immediately fail tryConnect */
                res->code = FAIL;
                sendResponseToClient(cl->fifo_res_name, res);
            }
        } else {
            res->code = OK;
            sendResponseToClient(cl->fifo_res_name, res);
            connections++;
            cl->id = ++clients;
            printf("Client PID %d connected as \"client%d\"\n", cl->pid, cl->id);
            /* Create child process to execute commands */
            pid_t pid = fork();
            switch (pid) {
                case -1:
                    perror("fork");
                    exit(EXIT_FAILURE);
                case 0: /* Child process */
                    serveClient(cl, argv[1]);
                    printf("client%d disconnected...\n", cl->id);
                    /* Send connection request for queued client */
                    int sv_fd = open(sv_fifo_name, O_WRONLY);
                    if (sv_fd == -1) {
                        perror("Error open server fifo");
                        exit(EXIT_FAILURE);
                    }
                    request_t req;
                    strcpy(req.cmd, "Enqueue");
                    if (write(sv_fd, &req, sizeof(request_t)) != sizeof(request_t)) {
                        perror("Error write request");
                        _exit(EXIT_FAILURE);
                    }
                    close(sv_fd);
                    _exit(EXIT_SUCCESS);
                default:
                    while (waitpid(-1, NULL, WNOHANG) > 0)
                        continue;
            }
        }
    }

    return 0;
}
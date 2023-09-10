/*
 *
 * System Programming - HW4
 *
 * biboServer source code
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <dirent.h>
#include <time.h>
#include <pthread.h>
#include "bibo_cl_sv.h"
#include "client_queue.h"
#include "request_queue.h"

#define LOG(string) fprintf(log_fp, "%s [%s]\n", string, getFormattedDateTime())

volatile int terminate_flag = 0;

typedef struct {
    request_t *req;
    int connected;
    int max_connection;
    int cl_count;
    client_queue_t *cl_q;
} connection_req_t;

typedef struct {
    char *sv_dir;
    pthread_mutex_t *mutex;
    pthread_cond_t *cond;
    cl_req_queue_t *cl_requests;
} client_args_t;

/* Signal handler for ^C (SIGINT) */
void signintHandler() {
    char sv_fifo_name[SERVER_FIFO_NAME_LEN];
    snprintf(sv_fifo_name, SERVER_FIFO_NAME_LEN, SERVER_FIFO_TEMPLATE, getpid());

    unlink(sv_fifo_name);
    killpg(getpid(), SIGKILL);
}

/* Get the formatted date and time information */
char *getFormattedDateTime() {
    time_t current_time;
    struct tm *local_time;
    static char date_time[20];

    time(&current_time);
    local_time = localtime(&current_time);

    strftime(date_time, sizeof(date_time), "%Y-%m-%d %H:%M:%S", local_time);

    return date_time;
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

/* Process the "list" request from the client */
void processList(char *sv_dir, response_t *res) {
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

/* Process the "readF" request from the client */
char *processReadFile(const char *filename, int line_num) {
    char *res = (char *) malloc(1024);
    strcpy(res, "");

    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
        fprintf(stderr, "Failed to open file: %s\n", filename);
        free(res);
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

    fclose(fp);
    return res;
}

/* Process the "writeT" request from the client */
int processWriteToFile(const char *filename, int line_num, char *string) {
    strcat(string, "\n");
    FILE *fp = fopen(filename, "a+");
    if (fp == NULL) {
        fprintf(stderr, "Failed to open file: %s\n", filename);
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
                fputs(buffer, temp);
                found = 1;
            } else {
                fputs(buffer, temp);
            }
            current_line++;
        }
        if (!found && current_line <= line_num) {
            /* Add empty lines if the requested line does not exist */
            while (current_line < line_num) {
                fputs("\n", temp);
                current_line++;
            }
            fputs(string, temp);
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
    }

    fclose(fp);
    return 0;
}

/* Serves the connected client's commands
 * cl: Client to be served
 * sv_dir: directory path of the server */
void *serveClients(void *args) {
    client_args_t *client_args = (client_args_t *) args;

    /* Get the client arguments */
    char *sv_dir = client_args->sv_dir;
    pthread_mutex_t *mutex = client_args->mutex;
    pthread_cond_t *cond = client_args->cond;
    cl_req_queue_t *cl_requests = client_args->cl_requests;

    while (!terminate_flag) {
        pthread_mutex_lock(mutex);
        while (cl_requests->head == NULL) {
            pthread_cond_wait(cond, mutex);
        }
        request_t *req = dequeueClientRequest(cl_requests);
        pthread_mutex_unlock(mutex);

        client_t *cl = createClient(req->cl_pid, 0);
        char *req_word = strtok(req->cmd, " ");

        /* Prepare the response and send it to the client */
        response_t *res = (response_t *) malloc(sizeof(response_t));
        if (strcmp(req_word, "help") == 0) {
            strcpy(res->msg, "Available commands are :\n"
                             "help, list, readF, writeT, upload, download, quit, killServer");
        } else if (strcmp(req_word, "list") == 0) {
            pthread_mutex_lock(mutex);
            processList(sv_dir, res);
            pthread_mutex_unlock(mutex);
        } else if (strcmp(req_word, "readF") == 0) {
            /* Get file path */
            char filename[50];
            char *temp;
            strcpy(filename, sv_dir);
            strcat(filename, "/");
            temp = strtok(NULL, " ");
            if (temp) {
                strcat(filename, temp);
            }
            /* Get line number */
            int line_num;
            temp = strtok(NULL, " ");
            if (temp == NULL)
                line_num = 0;
            else
                line_num = atoi(temp);
            char *result;
            pthread_mutex_lock(mutex);
            result = processReadFile(filename, line_num);
            pthread_mutex_unlock(mutex);
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
            if (temp) {
                strcat(filename, temp);
            }
            /* Get line number */
            int line_num;
            temp = strtok(NULL, " ");
            if (temp == NULL)
                line_num = 0;
            else
                line_num = atoi(temp);
            /* Get the string */
            char str[1024 * 5];
            temp = strtok(NULL, "\n");
            if (temp) {
                strcpy(str, temp);
                pthread_mutex_lock(mutex);
                processWriteToFile(filename, line_num, str);
                pthread_mutex_unlock(mutex);
            } else {
                pthread_mutex_lock(mutex);
                processWriteToFile(filename, line_num, "");
                pthread_mutex_unlock(mutex);
            }
            sprintf(res->msg, "Given string written to %s at line %d.", filename, line_num);
        } else if (strcmp(req_word, "upload") == 0) {
            strcpy(res->msg, "To be implemented.");
            /* TODO : Implement upload */
        } else if (strcmp(req_word, "download") == 0) {
            strcpy(res->msg, "To be implemented.");
            /* TODO : Implement download */
        } else {
            sprintf(res->msg, "Invalid command[%s], type \"help\" to see available commands.", req_word);
        }
        sendResponseToClient(cl->fifo_res_name, res);
        free(cl);
    }
    pthread_exit(NULL);
}

/* Handle connection requests from the clients
 * returns:
 * pointer to the connected client on success,
 * NULL on fail */
client_t *handleConnectionRequest(connection_req_t connection_req) {
    request_t *req = connection_req.req;
    int connected = connection_req.connected;
    int max_connection = connection_req.max_connection;
    int cl_count = connection_req.cl_count;
    client_queue_t *cl_q = connection_req.cl_q;
    client_t *cl = createClient(req->cl_pid, cl_count);

    /* Handle connection request */
    response_t *res = (response_t *) malloc(sizeof(response_t));
    if (connected == max_connection) {
        printf("Connection request PID %d... Queue FULL!\n", cl->pid);
        /* Add "Connect" request to the queue */
        if (!isClientInQueue(cl_q, cl) && strcmp(req->cmd, "Connect") == 0) {
            enqueueClient(cl_q, cl);
            res->code = WAIT;
            sendResponseToClient(cl->fifo_res_name, res);
            return NULL;
        } else { /* Immediately fail tryConnect */
            res->code = FAIL;
            sendResponseToClient(cl->fifo_res_name, res);
            free(cl);
            return NULL;
        }
    } else {
        /* Send connected info and print log */
        res->code = OK;
        sendResponseToClient(cl->fifo_res_name, res);
        return cl;
    }
}

int main(int argc, char *argv[]) {
    /* Check argument count, print usage */
    if (argc != 4) {
        fflush(stdout);
        fprintf(stderr,
                "Usage: %s <dirname> <max. #ofClients> <poolSize>\n", argv[0]);
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

    /* Initialize the necessary data for the server */
    pid_t sv_pid = getpid();
    time_t t = time(NULL);
    if (t == -1) {
        perror("Error getting time");
        exit(1);
    }
    client_queue_t *client_queue = createClientQueue();
    if (client_queue == NULL) {
        fprintf(stderr, "Error creating client queue!\n");
        exit(1);
    }
    cl_req_queue_t *cl_req_queue = createClientRequestQueue();
    if (cl_req_queue == NULL) {
        fprintf(stderr, "Error creating client request queue!\n");
        exit(1);
    }
    int max_connection = atoi(argv[2]);
    if (max_connection == 0) {
        fprintf(stderr, "Error getting max. #ofClients argument!\n");
        exit(1);
    }
    int pool_size = atoi(argv[3]);
    if (pool_size == 0) {
        fprintf(stderr, "Error getting poolSize argument!\n");
        exit(1);
    }
    pthread_t thread_pool[pool_size];

    /* Generate names for log file and fifo file*/
    char log_filename[SERVER_LOG_NAME_LEN];
    char sv_fifo_name[SERVER_FIFO_NAME_LEN];
    snprintf(log_filename, SERVER_LOG_NAME_LEN, SERVER_LOG_TEMPLATE, sv_pid);
    snprintf(sv_fifo_name, SERVER_FIFO_NAME_LEN, SERVER_FIFO_TEMPLATE, sv_pid);

    /* Create server directory, if not exists */
    if (mkdir(argv[1], 0777) == -1 && errno != EEXIST) {
        perror("Error creating server directory");
        exit(1);
    }
    /* Open the log file */
    FILE *log_fp = fopen(log_filename, "a");
    if (log_fp == NULL) {
        fprintf(stderr, "Error: cannot create log file %s\n", log_filename);
        exit(1);
    }
    /* Create server FIFO */
    if (mkfifo(sv_fifo_name, S_IRUSR | S_IWUSR) == -1
        && errno != EEXIST) {
        perror("mkfifo");
        exit(1);
    }

    LOG("Server started");
    printf("Server Started PID %d...\n", sv_pid);
    printf("waiting for clients...\n");

    pthread_mutex_t mutex;
    pthread_mutex_init(&mutex, NULL);

    pthread_cond_t cond;
    pthread_cond_init(&cond, NULL);

    /* Create the thread pool for serving client requests */
    for (int i = 0; i < pool_size; i++) {
        client_args_t cl_args;
        cl_args.mutex = &mutex;
        cl_args.cl_requests = cl_req_queue;
        cl_args.cond = &cond;
        cl_args.sv_dir = argv[1];
        if (pthread_create(&thread_pool[i], NULL, serveClients, &cl_args) != 0) {
            perror("Failed to create thread");
            exit(EXIT_FAILURE);
        }
        if(pthread_detach(thread_pool[i]) != 0) {
            perror("Failed to detach the thread");
            exit(EXIT_FAILURE);
        }
    }

    int connected = 0;
    int client_count = 0;
    client_t *connected_clients[max_connection];
    request_t *req = (request_t *) malloc(sizeof(request_t));
    int serve = 1;
    while (serve) {
        if (connected < max_connection && client_queue->head != NULL) {
            client_t *client = dequeueClient(client_queue);
            int cl_fd = open(client->fifo_res_name, O_WRONLY);
            if(cl_fd == -1) {
                perror("open");
                exit(EXIT_FAILURE);
            }
            if(write(cl_fd, "temp", 4) == -1){
                perror("write");
                exit(EXIT_FAILURE);
            }
            client_count++;
            client->id = client_count;
            connected_clients[connected] = client;
            connected++;
            printf("Client PID %d connected as \"client%d\"\n", client->pid, client->id);
            LOG("Client connected");
        }

        /* Wait for client requests */
        int sv_fd;
        sv_fd = open(sv_fifo_name, O_RDONLY);
        if (sv_fd == -1) {
            perror("Error open server fifo");
            exit(EXIT_FAILURE);
        }
        if (read(sv_fd, req, sizeof(request_t)) != sizeof(request_t)) {
            perror("Error reading connection request");
            close(sv_fd);
            continue;
        }
        close(sv_fd);

        /* Handle client connection requests */
        if (strcmp(req->cmd, "Connect") == 0 || strcmp(req->cmd, "tryConnect") == 0) {
            connection_req_t con_req;
            con_req.req = req;
            con_req.connected = connected;
            con_req.max_connection = max_connection;
            con_req.cl_count = client_count;
            con_req.cl_q = client_queue;

            client_t *client = handleConnectionRequest(con_req);
            if (client) {
                client_count++;
                client->id = client_count;
                connected_clients[connected] = client;
                connected++;
                printf("Client PID %d connected as \"client%d\"\n", client->pid, client->id);
                LOG("Client connected");
            }
        } else { /* Handle other requests from connected clients */
            /* Check if the client is connected */
            int found = 0;
            int cl_index;
            for (int i = 0; i < connected && !found; ++i) {
                if (connected_clients[i]->pid == req->cl_pid) {
                    found = 1;
                    cl_index = i;
                }
            }
            if (found) {
                client_t *client = connected_clients[cl_index];
                if (strcmp(req->cmd, "quit") == 0) {
                    /* Remove the client from connected clients array */
                    for (int i = cl_index; i < connected - 1; ++i) {
                        connected_clients[i] = connected_clients[i + 1];
                    }
                    connected--;
                    response_t *res = (response_t *) malloc(sizeof(response_t));
                    strcpy(res->msg, "Quiting");
                    sendResponseToClient(client->fifo_res_name, res);
                    printf("client%d disconnected...\n", client->id);
                    LOG("Client disconnected");
                    free(client);
                } else if (strcmp(req->cmd, "killServer") == 0) {
                    printf("kill signal from client%d.. terminating...\n", client->id);
                    LOG("Kill signal received");
                    free(req);
                    response_t *res = (response_t *) malloc(sizeof(response_t));
                    strcpy(res->msg, "Killing");
                    sendResponseToClient(client->fifo_res_name, res);
                    freeClientQueue(client_queue);
                    freeClientRequestQueue(cl_req_queue);
                    /* Free clients and unlink client fifos */
                    for (int i = 0; i < connected; ++i) {
                        client_t *cl = connected_clients[i];
                        unlink(cl->fifo_res_name);
                        unlink(cl->fifo_req_name);
                        free(cl);
                    }
                    unlink(sv_fifo_name);
                    terminate_flag = 1;
                    for (int i = 0; i < pool_size; ++i) {
                        if(pthread_join(thread_pool[i], NULL) != 0) {
                            perror("pthread_join");
                            exit(EXIT_FAILURE);
                        }
                    }
                    serve = 0;
                } else {
                    /* Add the request to the request queue */
                    pthread_mutex_lock(&mutex);
                    enqueueClientRequest(cl_req_queue, req);
                    /* Send signal to the threads to handle this request */
                    pthread_cond_signal(&cond);
                    pthread_mutex_unlock(&mutex);
                }
            } else {
                fprintf(stderr, "Client PID %d is not connected!\n", req->cl_pid);
            }
        }
    }
    fclose(log_fp);
    pthread_mutex_destroy(&mutex);
    pthread_cond_destroy(&cond);
    printf("bye\n");
    return 0;
}
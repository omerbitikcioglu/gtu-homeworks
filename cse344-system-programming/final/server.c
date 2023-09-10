/*
 * System Programming Final Homework
 *
 * Server code
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <pthread.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include "utils.h"

typedef struct {
    int socketFd;
    dir_args_t dirArgs;
} server_args_t;

void sigint_handler(int signum) {
    printf("\nExiting(%d)...\n", signum);
    stopFlag = 1;
}

void *handle_client(void *arg);

void serveClient(int clientSocket, dir_args_t dirArgs);

void traverseDirectory(const char *dirname, file_info_t **fileInfos, int *count);

int main(int argc, char *argv[]) {
    /* Check argument count */
    if (argc != 4) {
        fprintf(stderr, "Usage: %s [directory] [threadPoolSize] [portNumber]\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    /* Get arguments */
    char *serverDirName = argv[1];
    int threadPoolSize = atoi(argv[2]);
    int portNumber = atoi(argv[3]);

    /* Local variables */
    isServer = 1;
    struct sigaction sa;
    pthread_t threads[threadPoolSize];
    pthread_mutex_t dirMutex, dirUpdateMutex;

    /* Set up the signal handler */
    sa.sa_handler = sigint_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;

    /* Register the signal handler for SIGINT */
    if (sigaction(SIGINT, &sa, NULL) == -1) {
        perror("Error: cannot register signal handler");
        exit(EXIT_FAILURE);
    }

    /* Create socket */
    int serverSocket = socket(AF_INET, SOCK_STREAM | SOCK_NONBLOCK, 0);
    if (serverSocket == -1) {
        perror("Socket creation failed");
        exit(EXIT_FAILURE);
    }

    /* Bind socket to a specific address and port */
    struct sockaddr_in serverAddress;
    serverAddress.sin_family = AF_INET;
    serverAddress.sin_addr.s_addr = INADDR_ANY;
    serverAddress.sin_port = htons(portNumber);
    if (bind(serverSocket, (struct sockaddr *) &serverAddress, sizeof(serverAddress)) < 0) {
        perror("Binding failed");
        exit(EXIT_FAILURE);
    }

    /* Listen for client connections */
    if (listen(serverSocket, threadPoolSize) == -1) {
        perror("Listening failed");
        exit(EXIT_FAILURE);
    }
    printf("Server listening on port %d...\n", portNumber);
    fflush(stdout);

    /* Create server directory */
    if (mkdir(serverDirName, S_IRWXU | S_IRWXG | S_IRWXO) == -1 && errno != EEXIST) {
        perror("Error creating server directory");
        exit(EXIT_FAILURE);
    }

    /* Initialize mutexes */
    pthread_mutex_init(&dirMutex, NULL);
    pthread_mutex_init(&dirUpdateMutex, NULL);

    /* Create directory updates struct */
    dir_updates_t *dirUpdates = (dir_updates_t *) malloc(sizeof(dir_updates_t));
    dirUpdates->addedUpdatedFileList = NULL;
    dirUpdates->deletedFileList = NULL;
    dirUpdates->addedUpdatedCount = 0;
    dirUpdates->deletedCount = 0;

    /* Create directory args */
    dir_args_t dirArgs;
    dirArgs.name = serverDirName;
    dirArgs.updates = dirUpdates;
    dirArgs.mutex = &dirMutex;
    dirArgs.updateMutex = &dirUpdateMutex;

    /* Create a new thread for checking file changes in directory */
    pthread_t threadCheckFileChanges;
    pthread_create(&threadCheckFileChanges, NULL, checkFileChanges, &dirArgs);

    /* Create a thread pool */
    server_args_t sv_args;
    sv_args.socketFd = serverSocket;
    sv_args.dirArgs = dirArgs;

    for (int i = 0; i < threadPoolSize; i++) {
        if (pthread_create(&threads[i], NULL, handle_client, (void *) &sv_args) != 0) {
            perror("Thread creation failed");
            stopFlag = 1;
            break;
        }
    }

    /* Wait for all threads to finish */
    for (int i = 0; i < threadPoolSize; i++) {
        pthread_join(threads[i], NULL);
    }

    pthread_join(threadCheckFileChanges, NULL);

    /* Clean up */
    pthread_mutex_destroy(&dirMutex);
    pthread_mutex_destroy(&dirUpdateMutex);
    free(dirUpdates);

    printf("Bye...\n");

    return 0;
}

void *handle_client(void *arg) {
    server_args_t sv_args = *(server_args_t *) arg;
    int socketFd = sv_args.socketFd;
    dir_args_t dirArgs = sv_args.dirArgs;

    struct sockaddr_in clientAddress;
    int clientSocket, clientAddressLength;

    while (!stopFlag) {
        /* Accept client connection */
        clientAddressLength = sizeof(clientAddress);
        clientSocket = accept(socketFd,
                              (struct sockaddr *) &clientAddress,
                              (socklen_t *) &clientAddressLength);
        if (clientSocket < 0) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
                sleep(1);
                continue;
            } else {
                perror("Acceptance failed");
                stopFlag = 1;
                continue;
            }
        }

        char clientIP[INET_ADDRSTRLEN];
        inet_ntop(AF_INET, &(clientAddress.sin_addr), clientIP, INET_ADDRSTRLEN);

        printf("Client(%s) connected.\n", clientIP);
        serveClient(clientSocket, dirArgs);
        printf("Client(%s) disconnected.\n", clientIP);

        close(clientSocket);
    }

    pthread_exit(NULL);
}

void serveClient(int clientSocket, dir_args_t dirArgs) {
    /* Buffer for received data */
    char buf[BUFFER_SIZE];

    /* Send connected info to the client */
    strcpy(buf, "connected");
    if (sendData(clientSocket, buf, strlen(buf)) == -1) {
        stopFlag = 1;
        return;
    }

    /* Sync with the client on connection */
    syncPeerWithMeOnConnect(clientSocket, dirArgs.name, dirArgs.mutex);
    syncMeWithPeerOnConnect(clientSocket, dirArgs.name, dirArgs.mutex);

    printf("\n\tSync on connection done!\n\n");
    fflush(stdout);

    while (!stopFlag) {
        /* Stay sync with client */
        syncPeerWithMe(clientSocket, dirArgs.name, dirArgs.updates, dirArgs.updateMutex);
        if(syncMeWithPeer(clientSocket, dirArgs.name, dirArgs.mutex) == 1) {
            /* "bye" request received from the client */
            break;
        }
    }

    strcpy(buf, "bye");
    if(stopFlag) {
        sendData(clientSocket, buf, strlen(buf));
    }
}


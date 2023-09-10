/*
 * System Programming Final Homework
 *
 * Client code
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <pthread.h>
#include "utils.h"

void connectionEstablished(int clientSocket, const char *clientDirName);

void communicateWithServer(int clientSocket, dir_args_t args);

void sigint_handler(int signum) {
    printf("\nExiting(%d)...\n", signum);
    stopFlag = 1;
}

int main(int argc, char *argv[]) {
    if (argc != 3 && argc != 4) {
        fprintf(stderr, "Usage: %s [name] [portNumber] [serverIP -> optional]\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    /* Set up the signal handler */
    struct sigaction sa;
    sa.sa_handler = sigint_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;

    /* Register the signal handler for SIGINT */
    if (sigaction(SIGINT, &sa, NULL) == -1) {
        perror("Error register signal handler");
        exit(EXIT_FAILURE);
    }

    /* Get the arguments */
    char *clientDirName = argv[1];
    int portNumber = atoi(argv[2]);

    /* Set the server IP */
    char serverIP[INET_ADDRSTRLEN];
    if (argc == 4) {
        strncpy(serverIP, argv[3], INET_ADDRSTRLEN);
    } else {
        char *ip = getHostMachineIP();
        strncpy(serverIP, ip, INET_ADDRSTRLEN);
    }

    printf("Connection request to %s:%d...\n", serverIP, portNumber);

    /* Create the client socket */
    int clientSocket = socket(AF_INET, SOCK_STREAM, 0);
    if (clientSocket == -1) {
        perror("Socket creation failed");
        exit(EXIT_FAILURE);
    }

    /* Set up server details */
    struct sockaddr_in serverAddress;
    serverAddress.sin_family = AF_INET;
    serverAddress.sin_port = htons(portNumber);
    int returnValue = inet_pton(AF_INET, serverIP, &serverAddress.sin_addr);
    if (returnValue != 1) {
        if (returnValue == 0) {
            fprintf(stderr, "Invalid address\n");
        }
        if (returnValue == -1) {
            perror("Address not supported");
        }
        exit(EXIT_FAILURE);
    }

    /* Connect to the server */
    int connection = connect(clientSocket, (struct sockaddr *) &serverAddress, sizeof(serverAddress));
    if (connection == -1) {
        perror("Connection failed");
        exit(EXIT_FAILURE);
    } else if (connection == 0) {
        msg_buffer_t *msg;
        msg = receiveData(clientSocket);
        if (msg && strncmp(msg->buf, "connected", msg->bufSize) == 0) {
            free(msg->buf);
            free(msg);
            connectionEstablished(clientSocket, clientDirName);
        }
    }
    close(clientSocket);

    printf("Bye...\n");

    return 0;
}

void connectionEstablished(int clientSocket, const char *clientDirName) {
    pthread_mutex_t dirMutex, dirUpdateMutex;

    printf("Connection established.\n");

    /* Create the client directory */
    while (mkdir(clientDirName, S_IRWXU | S_IRWXG | S_IRWXO) == -1 && errno != EEXIST) {
        perror("Error creating client directory");
        exit(EXIT_FAILURE);
    }

    /* Initialize mutexes */
    pthread_mutex_init(&dirMutex, NULL);
    pthread_mutex_init(&dirUpdateMutex, NULL);

    /* Sync with server on connection */
    syncMeWithPeerOnConnect(clientSocket, clientDirName, &dirMutex);
    syncPeerWithMeOnConnect(clientSocket, clientDirName, &dirMutex);

    printf("\n\tSync on connection done!\n\n");
    fflush(stdout);

    /* Create a struct to hold directory updates */
    dir_updates_t *dirUpdates = (dir_updates_t *) malloc(sizeof(dir_updates_t));
    dirUpdates->addedUpdatedFileList = NULL;
    dirUpdates->deletedFileList = NULL;
    dirUpdates->addedUpdatedCount = 0;
    dirUpdates->deletedCount = 0;

    /* Create directory args */
    dir_args_t dirArgs;
    dirArgs.mutex = &dirMutex;
    dirArgs.updateMutex = &dirUpdateMutex;
    dirArgs.updates = dirUpdates;
    dirArgs.name = clientDirName;

    /* Create a new thread for checking file changes in directory */
    pthread_t threadCheckFileChanges;
    pthread_create(&threadCheckFileChanges, NULL, checkFileChanges, &dirArgs);

    communicateWithServer(clientSocket, dirArgs);

    pthread_join(threadCheckFileChanges, NULL);

    /* Clean up */
    pthread_mutex_destroy(&dirMutex);
    pthread_mutex_destroy(&dirUpdateMutex);
    free(dirUpdates);
}

/* Communication with the server happens here */
void communicateWithServer(int clientSocket, dir_args_t args) {
    /* Stay synchronized */
    while (!stopFlag) {
        if (syncMeWithPeer(clientSocket, args.name, args.mutex) == 1) {
            /* Server shutdown */
            serverBye = 1;
            fflush(stdout);
            break;
        }
        syncPeerWithMe(clientSocket, args.name, args.updates, args.updateMutex);
    }
    if (!serverBye) {
        char byeBuf[4];
        strcpy(byeBuf, "bye");
        sendData(clientSocket, byeBuf, strlen(byeBuf));
    }
}

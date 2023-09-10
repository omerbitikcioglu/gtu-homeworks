/*
 * System Programming - HW5 -
 */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/time.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include <semaphore.h>
#include <signal.h>

/* File type holding opened file descriptors
 * and file names */
typedef struct {
    char fileName[256];
    int srcFd;
    int destFd;
} file_t;

/* Buffer to hold opened files */
file_t **buffer;
int bufferIndex = 0;

/* Ending flags */
int producerDone = 0;
volatile int stopFlag = 0;

/* Mutexes */
pthread_mutex_t bufferMutex;
pthread_mutex_t flagMutex;
pthread_mutex_t printMutex;
pthread_mutex_t statMutex;

/* Semaphores */
sem_t semEmpty;
sem_t semFull;

/* Statistics */
int numOfFilesCopied = 0;
long numOfBytesCopied = 0;
int numOfRegularFileCopied = 0;
int numOfFifoCopied = 0;

int openFiles(const char *srcDirName, const char *destDirName);

void sigint_handler(int signum) {
    printf("Exiting(%d)...\n", signum);
    stopFlag = 1;
}

void *producer(void *args) {
    char **pArgs = (char **) args;
    char *srcDirName = pArgs[0];
    char *destDirName = pArgs[1];
    int nConsumer = atoi(pArgs[2]);

    /* Open files on both sides and pass to buffer */
    if(openFiles(srcDirName, destDirName) == -1) {
        /* Wake up stuck consumers */
        for (int i = 0; i < nConsumer; ++i) {
            sem_post(&semFull);
        }
        pthread_exit(NULL);
    }

    /* Set the done flag */
    pthread_mutex_lock(&flagMutex);
    producerDone = 1;
    pthread_mutex_unlock(&flagMutex);

    /* Wake up stuck consumers */
    for (int i = 0; i < nConsumer; ++i) {
        sem_post(&semFull);
    }

    pthread_exit(NULL);
}

/* Helper recursive function for producer thread */
int openFiles(const char *srcDirName, const char *destDirName) {
    DIR *srcDir, *destDir;
    struct dirent *entry;
    struct stat fileStat;
    char srcPath[4096];
    char destPath[4096];
    int srcFd, destFd;

    /* Open the source directory */
    srcDir = opendir(srcDirName);
    if (srcDir == NULL) {
        perror("Error: opening source directory");
        exit(EXIT_FAILURE);
    }

    /* Create the destination directory, if not exists */
    if (mkdir(destDirName, S_IRWXU | S_IRWXG | S_IRWXO) == -1 && errno != EEXIST) {
        perror("Error: creating destination directory");
        exit(EXIT_FAILURE);
    } else { /* Open the destination directory */
        destDir = opendir(destDirName);
        if (destDir == NULL) {
            perror("Error: opening destination directory");
            exit(EXIT_FAILURE);
        }
    }

    /* Iterate over the directory entries,
     * open files for reading and writing,
     * pass file descriptors and their names of the opened files into the buffer */
    while ((entry = readdir(srcDir)) != NULL) {
        file_t *fileItem;
        /* Signal received */
        if(stopFlag) {
            closedir(srcDir);
            closedir(destDir);
            return -1;
        }
        /* Skip . and .. files */
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }
        /* Build the source and destination paths of the file */
        snprintf(srcPath, sizeof(srcPath), "%s/%s", srcDirName, entry->d_name);
        snprintf(destPath, sizeof(destPath), "%s/%s", destDirName, entry->d_name);

        if (entry->d_type == DT_DIR) {
            /* srcPath -> source subdirectory
             * destPath -> destination subdirectory */
            if(openFiles(srcPath, destPath) == -1) {
                closedir(srcDir);
                closedir(destDir);
                return -1;
            }
            continue;
        } else if (entry->d_type == DT_FIFO) {
            /* Extract the mode of the FIFO */
            if (stat(srcPath, &fileStat) == -1) {
                perror("Error: getting file information");
                close(srcFd);
                close(destFd);
                continue;
            }
            mode_t mode = fileStat.st_mode;
            /* Make the same FIFO on destination folder */
            if (mkfifo(destPath, mode) == -1 && errno != EEXIST) {
                perror("Error: make fifo on destination");
                close(srcFd);
                close(destFd);
                continue;
            }
            pthread_mutex_lock(&statMutex);
            numOfFifoCopied++;
            pthread_mutex_unlock(&statMutex);
        } else if (entry->d_type == DT_REG) {
            /* Open the source file for reading */
            srcFd = open(srcPath, O_RDONLY);
            if (srcFd == -1) {
                pthread_mutex_lock(&printMutex);
                fprintf(stderr, "Error(%d): opening source file [%s]\n", errno, srcPath);
                pthread_mutex_unlock(&printMutex);
                continue;
            }
            /* Open the destination file for writing, creating it if it doesn't exist or truncating it if it does */
            destFd = open(destPath, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
            if (destFd == -1) {
                close(srcFd);
                pthread_mutex_lock(&printMutex);
                fprintf(stderr, "Error(%d): opening destination file [%s]\n", errno, destPath);
                pthread_mutex_unlock(&printMutex);
                continue;
            }
            /* Produce the file item */
            fileItem = (file_t *) malloc(sizeof(file_t));
            strcpy(fileItem->fileName, entry->d_name);
            fileItem->srcFd = srcFd;
            fileItem->destFd = destFd;

            /* Add the produced file item to the buffer */
            sem_wait(&semEmpty);
            pthread_mutex_lock(&bufferMutex);
            buffer[bufferIndex] = fileItem;
            bufferIndex++;
            pthread_mutex_unlock(&bufferMutex);
            sem_post(&semFull);

            pthread_mutex_lock(&statMutex);
            numOfRegularFileCopied++;
            pthread_mutex_unlock(&statMutex);
        }

        pthread_mutex_lock(&statMutex);
        numOfFilesCopied++;
        pthread_mutex_unlock(&statMutex);
    }
    closedir(srcDir);
    closedir(destDir);
    return 0;
}

/* Copy files from source directory to destination directory,
 * until producer done with producing file items and
 * there is no more files to be copied in the buffer */
void *consumer(void *args) {

    (void) args; /* Escape no-use */
    file_t *fileItem;

    while (!stopFlag) {
        pthread_mutex_lock(&flagMutex);
        if (producerDone && bufferIndex == 0) {
            pthread_mutex_unlock(&flagMutex);
            pthread_exit(NULL);
        }
        pthread_mutex_unlock(&flagMutex);

        /* Get a file item from buffer */
        sem_wait(&semFull);
        pthread_mutex_lock(&bufferMutex);
        if (bufferIndex == 0 || stopFlag) { /* Extra check for stuck consumers */
            pthread_mutex_unlock(&bufferMutex);
            pthread_exit(NULL);
        }
        fileItem = buffer[bufferIndex - 1];
        bufferIndex--;
        pthread_mutex_unlock(&bufferMutex);
        sem_post(&semEmpty);

        /* Copy the file contents */
        char buf[4096];
        ssize_t bytesRead, bytesWritten;
        while ((bytesRead = read(fileItem->srcFd, buf, sizeof(buf))) > 0) {
            bytesWritten = write(fileItem->destFd, buf, bytesRead);
            if (bytesWritten != bytesRead) {
                perror("Error writing to destination file");
                close(fileItem->srcFd);
                close(fileItem->destFd);
                continue;
            }
            pthread_mutex_lock(&statMutex);
            numOfBytesCopied += bytesWritten;
            pthread_mutex_unlock(&statMutex);
        }
        if (bytesRead == -1) {
            perror("Error reading from source file");
            close(fileItem->srcFd);
            close(fileItem->destFd);
            continue;
        }
        pthread_mutex_lock(&printMutex);
        printf("File %s is copied successfully.\n", fileItem->fileName);
        pthread_mutex_unlock(&printMutex);

        close(fileItem->srcFd);
        close(fileItem->destFd);
        free(fileItem);
    }

    pthread_exit(NULL);
}

int main(int argc, char *argv[]) {

    /* Check argument count */
    if (argc != 5) {
        fprintf(stderr, "Correct usage: %s bufferSize #ofConsumers sourceDir destDir\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    struct sigaction sa;

    /* Set up the signal handler */
    sa.sa_handler = sigint_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;

    /* Register the signal handler for SIGINT */
    if (sigaction(SIGINT, &sa, NULL) == -1) {
        perror("Error: cannot register signal handler");
        exit(EXIT_FAILURE);
    }

    /* Take arguments */
    int bufferSize = atoi(argv[1]);
    int numConsumers = atoi(argv[2]);

    /* Initialize the mutex and the semaphores */
    pthread_mutex_init(&bufferMutex, NULL);
    pthread_mutex_init(&flagMutex, NULL);
    pthread_mutex_init(&printMutex, NULL);
    pthread_mutex_init(&statMutex, NULL);
    sem_init(&semEmpty, 0, bufferSize);
    sem_init(&semFull, 0, 0);

    /* Allocate memory for the buffer */
    buffer = (file_t **) malloc(sizeof(file_t *) * bufferSize);
    if (buffer == NULL) {
        fprintf(stderr, "Error: allocating memory for the buffer");
        exit(EXIT_FAILURE);
    }

    /* Get producer arguments */
    char *pArgs[3];
    pArgs[0] = argv[3]; /* Src dir */
    pArgs[1] = argv[4]; /* Dest dir */
    pArgs[2] = argv[2]; /* # of consumers */

    /* Start timer */
    struct timeval start, end;
    gettimeofday(&start, NULL);

    /* Start the producer thread */
    pthread_t producerThread;
    if (pthread_create(&producerThread, NULL, producer, pArgs) != 0) {
        perror("Error: creating new producer thread");
        exit(EXIT_FAILURE);
    }

    /* Start the consumer threads */
    pthread_t consumerThread[numConsumers];
    for (int i = 0; i < numConsumers; ++i) {
        if (pthread_create(&consumerThread[i], NULL, consumer, NULL) != 0) {
            perror("Error: creating new consumer thread");
            exit(EXIT_FAILURE);
        }
    }

    /* Join to the producer thread */
    if (pthread_join(producerThread, NULL) != 0) {
        perror("Error: join producer thread");
        exit(EXIT_FAILURE);
    }

    /* Join to the consumer threads */
    for (int i = 0; i < numConsumers; ++i) {
        if (pthread_join(consumerThread[i], NULL) != 0) {
            perror("Error: join consumer thread");
            exit(EXIT_FAILURE);
        }
    }
    /* End timer */
    gettimeofday(&end, NULL);

    if(stopFlag) {
        file_t *fileItem;
        while (bufferIndex > 0) {
            fileItem = buffer[bufferIndex - 1];
            close(fileItem->srcFd);
            close(fileItem->destFd);
            free(fileItem);
            bufferIndex--;
        }
    }
    free(buffer);

    /* Destroy the semaphores and the mutex */
    sem_destroy(&semEmpty);
    sem_destroy(&semFull);
    pthread_mutex_destroy(&bufferMutex);
    pthread_mutex_destroy(&flagMutex);
    pthread_mutex_destroy(&printMutex);
    pthread_mutex_destroy(&statMutex);

    /* Calculate the total time to copy files in the directory */
    long seconds, microseconds;
    seconds = end.tv_sec - start.tv_sec;
    microseconds = end.tv_usec - start.tv_usec;
    if (microseconds < 0) {
        seconds--;
        microseconds += 1000000;
    }

    /* Print statistics */
    printf("Elapsed time: %ld seconds and %ld microseconds.\n", seconds, microseconds);
    printf("Number of files copied: %d (%ld bytes)\n"
           "\t%d regular file\n"
           "\t%d FIFO\n",
           numOfFilesCopied, numOfBytesCopied, numOfRegularFileCopied, numOfFifoCopied);

    return 0;
}

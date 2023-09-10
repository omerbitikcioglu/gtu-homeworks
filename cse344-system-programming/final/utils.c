#include "utils.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <fcntl.h>
#include <utime.h>
#include <ctype.h>
#include <pthread.h>
#include <errno.h>
#include <libgen.h>

int stopFlag = 0;
int serverBye = 0;
int isServer = 0;

char *getHostMachineIP() {
    char hostname[MAX_NAME_LENGTH + 1];
    gethostname(hostname, sizeof(hostname));

    struct addrinfo hints, *res;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    int returnValue = getaddrinfo(hostname, NULL, &hints, &res);
    if (returnValue != 0) {
        const char *errMsg = gai_strerror(returnValue);
        fprintf(stderr, "%s\n", errMsg);
        return NULL;
    }

    struct sockaddr_in *addr = (struct sockaddr_in *) res->ai_addr;
    char *ip = inet_ntoa(addr->sin_addr);

    freeaddrinfo(res);
    return ip;
}

void traverseDirectory(const char *dirname, file_info_t **fileInfos, int *count) {
    *fileInfos = NULL;
    *count = 0;

    file_info_t *subDirFileInfos = NULL;
    int subDirFileCount = 0;

    DIR *dir = opendir(dirname);
    if (dir == NULL) {
        perror("Failed to open directory");
        return;
    }

    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }
        if (entry == NULL) {
            return;
        }

        /* Construct the name and path to the file or subdirectory */
        char name[MAX_NAME_LENGTH + 1];
        snprintf(name, sizeof(name), "%s", entry->d_name);

        char path[MAX_PATH_LENGTH + 1];
        snprintf(path, sizeof(path), "%s/%s", dirname, entry->d_name);

        struct stat st;
        if (lstat(path, &st) == -1) {
            perror("Failed to get file information");
            continue;
        }

        if (S_ISDIR(st.st_mode)) {
            /* It's a subdirectory, recursively traverse it */
            traverseDirectory(path, &subDirFileInfos, &subDirFileCount);
        } else if (S_ISREG(st.st_mode)) {
            /* It's a regular file, collect its information */
            file_info_t fileInfo;
            strncpy(fileInfo.name, name, sizeof(fileInfo.name));
            strncpy(fileInfo.path, path, sizeof(fileInfo.path));
            fileInfo.size = st.st_size;
            fileInfo.modified = st.st_mtime;
            fileInfo.sameSize = 0;
            fileInfo.sameFile = 0;

            /* Add the file info to the array */
            *fileInfos = realloc(*fileInfos, (*count + 1) * sizeof(file_info_t));
            (*fileInfos)[*count] = fileInfo;
            (*count)++;
        }

        if (subDirFileInfos) {
            for (int i = 0; i < subDirFileCount; ++i) {
                /* Add it to the file infos as regular file */
                *fileInfos = realloc(*fileInfos, (*count + 1) * sizeof(file_info_t));
                (*fileInfos)[*count] = subDirFileInfos[i];
                (*count)++;
            }
            free(subDirFileInfos);
            subDirFileInfos = NULL;
        }
    }

    closedir(dir);
}

int compareFileContent(const char *filePath, const char *data) {
    FILE *file = fopen(filePath, "rb");

    if (file == NULL) {
        perror("Failed to open file");
        return 1;
    }

    /* Calculate the file size */
    fseek(file, 0, SEEK_END);
    size_t fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    /* Allocate memory for file content */
    char *fileData = (char *) malloc(fileSize);
    if (fileData == NULL) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    /* Read the entire file content */
    size_t bytesRead = fread(fileData, 1, fileSize, file);

    /* Compare the file content with the given data */
    int result = 1; /* Assume data is different */
    if (bytesRead == fileSize && memcmp(fileData, data, fileSize) == 0) {
        result = 0; /* Data is identical */
    }

    fclose(file);
    free(fileData);

    return result;
}

file_info_t *
findAddedUpdatedFiles(file_info_t *dir1, size_t count1, file_info_t *dir2, size_t count2, int *resultCount) {

    /* Allocate memory for the filesAddedUpdated array */
    file_info_t *filesAddedUpdated = (file_info_t *) malloc(count1 * sizeof(file_info_t));
    int count = 0;

    file_info_t dir1_file, dir2_file;

    /* Compare dir1 files with dir2 files */
    for (size_t i = 0; i < count1; i++) {
        dir1_file = dir1[i];

        /* Skip .log files */
        size_t length = strlen(dir1_file.name);
        if (length >= 4 && strcmp(dir1_file.name + length - 4, ".log") == 0) {
            continue;
        }

        for (size_t j = 0; j < count2; j++) {
            dir2_file = dir2[j];

            /* Check if the file exists in the dir2 directory */
            if (strcmp(dir1_file.name, dir2_file.name) == 0) {
                dir1_file.sameFile = 1;

                if (dir1_file.modified > dir2_file.modified) {

                    if (dir1_file.size == dir2_file.size) dir1_file.sameSize = 1;

                    /* Add the file to the filesAddedUpdated array */
                    filesAddedUpdated[count] = dir1_file;
                    count++;
                }

                break;
            }
        }
        /* If the file is not found in the dir2 directory, add it to the filesAddedUpdated array */
        if (!dir1_file.sameFile) {
            filesAddedUpdated[count] = dir1_file;
            count++;
        }
    }

    *resultCount = count;

    return filesAddedUpdated;
}

file_info_t *findDeletedFiles(file_info_t *dir1, size_t count1, file_info_t *dir2, size_t count2, int *resultCount) {

    /* Allocate memory for the filesDeleted array */
    file_info_t *filesDeleted = (file_info_t *) malloc(count1 * sizeof(file_info_t));
    int count = 0;

    /* Iterate over each file in dir1 */
    for (size_t i = 0; i < count1; i++) {
        int found = 0;

        /* Check if the file exists in dir2 */
        for (size_t j = 0; j < count2; j++) {
            if (strcmp(dir1[i].name, dir2[j].name) == 0) {
                found = 1;
                break;
            }
        }

        /* If the file is not found in dir2, add it to the filesDeleted array */
        if (!found) {
            filesDeleted[count] = dir1[i];
            count++;
        }
    }

    *resultCount = count;

    return filesDeleted;
}

int sendData(int socketFd, const void *data, size_t dataSize) {
    const char *buffer = (const char *) data;
    size_t bytesToSend;
    ssize_t bytesSent;
    size_t totalBytesSent = 0;
    int checkOk = 1;

    /* Clear non-block flag */
    int flags = fcntl(socketFd, F_GETFL, 0);
    fcntl(socketFd, F_SETFL, flags & ~O_NONBLOCK);

    printf("------------------\n");
    printf("Sending data...\n");
    while (totalBytesSent < dataSize) {
        /* Calculate remaining bytes to write */
        if (dataSize - totalBytesSent < BUFFER_SIZE) {
            bytesToSend = dataSize - totalBytesSent;
        } else {
            bytesToSend = BUFFER_SIZE;
        }
        /* Write the block of data to the buffer */
        bytesSent = write(socketFd, buffer + totalBytesSent, bytesToSend);
        if (bytesSent == -1) {
            perror("write");
            return -1;
        }
        totalBytesSent += bytesSent;
    }
#ifdef DEBUG_MOD
    printf("Total bytes sent: %zu\n", totalBytesSent);
#endif
    if (write(socketFd, "stop", strlen("stop")) == -1) {
        perror("send");
        return -1;
    }
    printf("Stop signal sent. End of data...\n");
    if (totalBytesSent != dataSize) {
        fprintf(stderr, "Data sent corrupted! %zu!=%zu\n", totalBytesSent, dataSize);
        return -1;
    }

    /* Block until peer sent the "ok" message */
    if (checkOk) {
        printf("Waiting peer to send \"ok\" msg\n");
        char okBuf[3];
        if (read(socketFd, okBuf, 2) == -1) {
            perror("read ok msg");
            return -1;
        } else {
            if (strcmp(okBuf, "ok") != 0) {
                fprintf(stderr, "OK message is not received! [%s]\n", okBuf);
                return -1;
            } else {
                printf("OK message received\n");

            }
        }
    }
    printf("------------------\n");

    return 0;
}

msg_buffer_t *receiveData(int socketFd) {
    char *buf = NULL;
    char *temp = NULL;
    ssize_t totalBytesRead = 0;
    int stopReading = 0;
    char lastFourBytes[5];

    /* Clear non-block flag */
    int flags = fcntl(socketFd, F_GETFL, 0);
    fcntl(socketFd, F_SETFL, flags & ~O_NONBLOCK);

    /* Receive data until "stop" signal is sent */
    printf("------------------\n");
    printf("Receiving data...\n");
    for (int i = 0; !stopReading; ++i) {
        if (i == 0) {
            buf = (char *) malloc(BUFFER_SIZE + 1);
        } else if (i > 0) {
            temp = (char *) realloc(buf, totalBytesRead + BUFFER_SIZE + 1);
            if (temp) {
                buf = temp;
            }
        }
        ssize_t bytesRead = read(socketFd, buf + totalBytesRead, BUFFER_SIZE);
        if (bytesRead == -1) {
            perror("Failed to read from socket");
            free(buf);
            return NULL;
        } else {
            snprintf(lastFourBytes, 5, "%s", buf + totalBytesRead + bytesRead - 4);
            if (strncmp(lastFourBytes, "stop", 4) == 0) {
                printf("Stop signal received. End of data...\n");
                /* Eliminate "stop" signal */
                bytesRead -= 4;
                stopReading = 1;
            }
            totalBytesRead += bytesRead;
        }
    }
#ifdef DEBUG_MOD
    printf("Total bytes read: %zu \n", totalBytesRead);
#endif
    /* Indicate end */
    buf[totalBytesRead] = '\0';
    if (isprint(buf[0])) {
        printf("String [%s] is read\n", buf);
        fflush(stdout);
    }

    /* Create a return message buffer */
    msg_buffer_t *msgBuffer = (msg_buffer_t *) malloc(sizeof(msg_buffer_t));
    if (msgBuffer) {
        msgBuffer->bufSize = totalBytesRead;
        msgBuffer->buf = buf;
    }

    /* Send ok message to indicate reading is done */
    if (write(socketFd, "ok", strlen("ok")) == -1) {
        perror("write ok to peer");
        return NULL;
    }
    printf("OK message sent\n");
    printf("------------------\n");

    return msgBuffer;
}

int openFileInDirectory(const char *directory, const char *filename) {
    DIR *dir = opendir(directory);
    if (dir == NULL) {
        perror("Failed to open directory");
        return -1;
    }

    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        if (entry->d_type == DT_REG && strcmp(entry->d_name, filename) == 0) {
            char filePath[MAX_PATH_LENGTH + 1];
            snprintf(filePath, sizeof(filePath), "%s/%s", directory, entry->d_name);

            int fd = open(filePath, O_RDONLY);
            if (fd == -1) {
                perror("Failed to open file");
                closedir(dir);
                return -1;
            }

            closedir(dir);
            return fd;
        }
    }

    closedir(dir);
    return -1;
}

char *readFileContent(int fd) {
    char *buffer = (char *) malloc(BUFFER_SIZE);
    char *temp = NULL;
    size_t bufferSize = 0;
    ssize_t bytesRead;

    while ((bytesRead = read(fd, buffer + bufferSize, BUFFER_SIZE)) > 0) {
        bufferSize += bytesRead;
        temp = realloc(buffer, bufferSize + BUFFER_SIZE);
        if (temp == NULL) {
            perror("Failed to allocate memory");
            free(buffer);
            return NULL;
        }
        buffer = temp;
    }

    if (bytesRead == -1) {
        perror("Failed to read file");
        free(buffer);
        return NULL;
    }

    /* Null-terminate the buffer */
    temp = realloc(buffer, bufferSize + 1);
    if (temp == NULL) {
        perror("Failed to allocate memory");
        free(buffer);
        return NULL;
    }
    buffer = temp;
    buffer[bufferSize] = '\0';

    return buffer;
}

/* Create the parent directories if not exists */
void createDirectories(const char *filePath) {
    char parentDirName[MAX_PATH_LENGTH];
    char subDirName[MAX_NAME_LENGTH];

    printf("Creating directories...\n");

    /* Eliminate actual file name */
    char *parentDirPath = strdup(filePath);
    char *lastSlash = strrchr(parentDirPath, '/');
    if (lastSlash != NULL) {
        *lastSlash = '\0';
        printf("Parent dir path [%s]", parentDirPath);
        /* Get directory names */
        char *temp = strtok(parentDirPath, "/");
        strcpy(parentDirName, temp);
        while (temp != NULL) {
            printf("Creating directory [%s]...\n", parentDirName);
            if (mkdir(parentDirName, S_IRWXU | S_IRWXG | S_IRWXO) == -1) {
                if (errno == EEXIST) {
                    printf("Directory exists\n");
                } else {
                    perror("Failed to create directory");
                    exit(EXIT_FAILURE);
                }
            } else {
                printf("Directory [%s] created\n", parentDirName);
            }
            temp = strtok(NULL, "/");
            if (temp != NULL) {
                strcpy(subDirName, temp);
                strcat(parentDirName, "/");
                strcat(parentDirName, subDirName);
            }
        }
    }
    free(parentDirPath);
}

int createFile(const char *filePath, const char *buffer, size_t bufferSize) {

    createDirectories(filePath);

    printf("Creating file [%s]...\n", filePath);
    int fd = open(filePath, O_CREAT | O_WRONLY | O_TRUNC, 0644);
    if (fd == -1) {
        perror("Failed to create file");
        exit(EXIT_FAILURE);
    }

    ssize_t bytesWritten = write(fd, buffer, bufferSize);
    if (bytesWritten == -1) {
        perror("Failed to write to file");
        close(fd);
        exit(EXIT_FAILURE);
    }

    close(fd);

    return 0;
}

int syncPeerWithMeOnConnect(int socketFd, const char *dirName, pthread_mutex_t *dirMutex) {
    file_info_t *localFiles;
    int svFileCount = 0;
    msg_buffer_t *msgBuffer = NULL;

    /* Send name of the local files in directory to peer */
    pthread_mutex_lock(dirMutex);
    traverseDirectory(dirName, &localFiles, &svFileCount);
    pthread_mutex_unlock(dirMutex);
    if (svFileCount != 0) {
        if (sendData(socketFd, localFiles, svFileCount * sizeof(file_info_t)) == -1) {
            fprintf(stderr, "Error sending data\n");
            return -1;
        }
        free(localFiles);
    } else {
        char noFileBuf[7];
        strcpy(noFileBuf, "nofile");
        sendData(socketFd, noFileBuf, strlen(noFileBuf));
        return 0; // No file
    }
    /* Get the number of files requested */
    msgBuffer = receiveData(socketFd);
    int numOfFiles = 0;
    if (msgBuffer) {
        numOfFiles = *(int *) msgBuffer->buf;
        free(msgBuffer->buf);
        free(msgBuffer);
    }
    /* Get the requested files */
    for (int i = 0; i < numOfFiles; ++i) {
        file_info_t *requestedFile = NULL;
        msgBuffer = receiveData(socketFd);
        if (msgBuffer) {
            requestedFile = (file_info_t *) msgBuffer->buf;
            free(msgBuffer);
        }
        /* Find the file in directory, read its content, send to peer */
        if (requestedFile) {
            pthread_mutex_lock(dirMutex);
            sendFileToPeer(socketFd, dirName, *requestedFile);
            pthread_mutex_unlock(dirMutex);
            free(requestedFile);
        }
    }
    return 0;
}

void sendFileToPeer(int socketFd, const char *dirName, file_info_t requestedFile) {
    int noFile = 0;

    char localPath[MAX_PATH_LENGTH + 1];
    char *afterPeerDirName = strstr(requestedFile.path, "/");
    sprintf(localPath, "%s%s", dirName, afterPeerDirName);

    int fd = open(localPath, O_RDONLY);
    if (fd != -1) {
        printf("Reading content of [%s]...\n", localPath);
        char *fileContent = readFileContent(fd);
        close(fd);
        /* Send the file content to the peer */
        if (fileContent) {
            printf("Sending file content of [%s] to peer...\n", localPath);
            if (sendData(socketFd, fileContent, requestedFile.size) == -1) {
                fprintf(stderr, "Error sending data\n");
                return;
            }
            free(fileContent);
        } else {
            fprintf(stderr, "Error receiving file content!\n");
            noFile = 1;
        }
    } else {
        perror("open");
        noFile = 1;
    }
    if (noFile) {
        /* Send "nofile" message */
        char noFileBuf[7];
        strcpy(noFileBuf, "nofile");
        sendData(socketFd, noFileBuf, strlen(noFileBuf));
    }
}

int syncMeWithPeerOnConnect(int socketFd, const char *dirName, pthread_mutex_t *dirMutex) {
    char buf[BUFFER_SIZE];
    file_info_t *localFiles, *peerFiles;
    msg_buffer_t *msgBuffer;
    FILE *logFile;

    /* Open the log file */
    if (!isServer) {
        sprintf(buf, "%s/%s.log", dirName, dirName);
        logFile = fopen(buf, "a");
        if (logFile == NULL) {
            fprintf(stderr, "Failed to open the file.\n");
            return -1;
        }
    }

    /* Compare directory with peer's directory */
    msgBuffer = receiveData(socketFd);
    if (msgBuffer && strcmp(msgBuffer->buf, "nofile") != 0) {
        /* Get the information of peer files */
        peerFiles = (file_info_t *) msgBuffer->buf;
        size_t numOfPeerFiles = msgBuffer->bufSize;
        numOfPeerFiles /= sizeof(file_info_t);
        free(msgBuffer);

        /* Get the information of local files */
        size_t numOfLocalFiles = 0;
        pthread_mutex_lock(dirMutex);
        traverseDirectory(dirName, &localFiles, (int *) &numOfLocalFiles);
        pthread_mutex_unlock(dirMutex);

        /* Find the files added / updated on peer's directory */
        int numOfFilesNeedToBeSync;
        file_info_t *filesToSync = findAddedUpdatedFiles(peerFiles, numOfPeerFiles, localFiles, numOfLocalFiles,
                                                         &numOfFilesNeedToBeSync);
        free(localFiles);
        free(peerFiles);
        /* Send number of requested files */
        if (sendData(socketFd, &numOfFilesNeedToBeSync, sizeof(int)) == -1) {
            fprintf(stderr, "Error sending data\n");
            return -1;
        }

        for (int i = 0; i < numOfFilesNeedToBeSync; ++i) {
            /* Send the file info of requested file to peer */
            if (sendData(socketFd, &filesToSync[i], sizeof(file_info_t)) == -1) {
                fprintf(stderr, "Error sending data\n");
                return -1;
            }

            /* Get the file content from peer */
            printf("Receiving file content of [%s] from peer...\n", filesToSync[i].name);
            char *fileData = NULL;
            msgBuffer = receiveData(socketFd);
            if (msgBuffer && strcmp(msgBuffer->buf, "nofile") != 0) {
                fileData = (char *) msgBuffer->buf;
                free(msgBuffer);
            } else {
                fprintf(stderr, "No data received for [%s]!\n", filesToSync[i].name);
                free(msgBuffer);
                free(msgBuffer->buf);
                continue;
            }

            /* Get the local file path */
            char localFilePath[MAX_PATH_LENGTH + 1];
            char *afterPeerDirName = strstr(filesToSync[i].path, "/");
            sprintf(localFilePath, "%s%s", dirName, afterPeerDirName);

            printf("Local file path of received file [%s]\n", localFilePath);

            /* Make the two file timestamps same */
            if (filesToSync[i].sameSize && compareFileContent(localFilePath, fileData) == 0) {
                printf("Making timestamps same...\n");
                struct utimbuf newTimes;
                newTimes.actime = filesToSync[i].modified;
                newTimes.modtime = filesToSync[i].modified;
                pthread_mutex_lock(dirMutex);
                if (utime(localFilePath, &newTimes) == -1) {
                    perror("Failed to change file modified time");
                    pthread_mutex_unlock(dirMutex);
                    continue;
                }
                pthread_mutex_unlock(dirMutex);
                free(fileData);
                continue;
            }

            /* Create/update the file in the local directory */
            printf("Creating filee [%s]...\n", localFilePath);
            pthread_mutex_lock(dirMutex);
            if (createFile(localFilePath, fileData, filesToSync[i].size) == -1) {
                pthread_mutex_unlock(dirMutex);
                continue;
            }
            pthread_mutex_unlock(dirMutex);
            free(fileData);

            /* Printing log messages */
            if (filesToSync[i].sameFile) {
                sprintf(buf, "File '%s' updated.", localFilePath);
                printf("%s\n", buf);
                if (logFile) {
                    fprintf(logFile, "%s\n", buf);
                }
            } else {
                sprintf(buf, "File '%s' created.", localFilePath);
                printf("%s\n", buf);
                if (logFile) {
                    fprintf(logFile, "%s\n", buf);
                }
            }
        }
        free(filesToSync);
    }
    if (!isServer) {
        fclose(logFile);
    }

    return 0;
}

void deleteFile(const char *directory, const char *filename) {
    DIR *dir;
    struct dirent *entry;

    /* Open the directory */
    dir = opendir(directory);
    if (dir == NULL) {
        perror("Error opening directory");
        return;
    }

    /* Iterate over the directory entries */
    while ((entry = readdir(dir)) != NULL) {
        /* Compare the entry name with the target filename */
        if (strcmp(entry->d_name, filename) == 0) {
            /* Construct the full path of the file */
            char filePath[MAX_PATH_LENGTH + 1];
            snprintf(filePath, sizeof(filePath), "%s/%s", directory, entry->d_name);

            /* Delete the file */
            if (remove(filePath) == 0) {
                printf("FILE [%s] DELETED\n", entry->d_name);
            } else {
                perror("Error deleting file");
            }

            /* Exit the loop after deleting the file */
            break;
        }
    }

    /* Close the directory */
    closedir(dir);
}

int syncMeWithPeer(int socketFd, const char *dirName, pthread_mutex_t *dirMutex) {
    char buf[BUFFER_SIZE + 1];
    char filePath[MAX_PATH_LENGTH + 1];
    msg_buffer_t *msg;
    int flags;

    file_info_t *filesToDelete;
    file_info_t *filesToAdd;
    size_t numOfFilesToDelete = 0;
    size_t numOfFilesToAdd = 0;

    /* Set the socket nonblock */
    flags = fcntl(socketFd, F_GETFL, 0);
    fcntl(socketFd, F_SETFL, flags | O_NONBLOCK);

    fd_set read_fds;
    struct timeval timeout;
    int result;
    FILE *logFile = NULL;

    FD_ZERO(&read_fds);
    FD_SET(socketFd, &read_fds);

    timeout.tv_sec = 1;
    timeout.tv_usec = 0;

    /* Check if the peer send any request */
    result = select(socketFd + 1, &read_fds, NULL, NULL, &timeout);
    if (result == -1) {
        perror("select");
        return -1;
    } else if (result != 0) {
        /* Open the log file */
        if (!isServer) {
            sprintf(buf, "%s/%s.log", dirName, dirName);
            logFile = fopen(buf, "a");
            if (logFile == NULL) {
                fprintf(stderr, "Failed to open the file.\n");
                return -1;
            }
        }
        /* Get the request from peer */
        printf("Waiting for request from peer...\n");
        msg = receiveData(socketFd);
        if (msg) {
            strcpy(buf, msg->buf);
            free(msg->buf);
            free(msg);
            printf("Received request: %s\n", buf);
            if (strcmp(buf, "delete") == 0) {
                printf("\tDELETE\n");
                printf("Getting deleted files list from peer...\n");
                msg = receiveData(socketFd);
                if (msg) {
                    filesToDelete = msg->buf;
                    numOfFilesToDelete = msg->bufSize / sizeof(file_info_t);
                    free(msg);
                    /* Delete files */
                    pthread_mutex_lock(dirMutex);
                    for (size_t i = 0; i < numOfFilesToDelete; ++i) {
                        /* Get the local file path */
                        char *afterPeerDirName = strstr(filesToDelete[i].path, "/");
                        sprintf(filePath, "%s%s", dirName, afterPeerDirName);

                        /* Delete the file */
                        if (remove(filePath) == 0) {
                            sprintf(buf, "File '%s' deleted.", filePath);
                            printf("%s\n", buf);
                            if (logFile) {
                                fprintf(logFile, "%s\n", buf);
                            }
                        } else {
                            perror("Error deleting file");
                            continue;
                        }
                    }
                    pthread_mutex_unlock(dirMutex);
                    free(filesToDelete);
                }
            } else if (strcmp(buf, "add") == 0) {
                printf("\tADD\n");
                printf("Receiving added/updated files list from peer...\n");
                msg = receiveData(socketFd);
                if (msg) {
                    filesToAdd = (file_info_t *) msg->buf;
                    numOfFilesToAdd = msg->bufSize / sizeof(file_info_t);
                    free(msg);
                    for (size_t i = 0; i < numOfFilesToAdd; ++i) {
                        /* Get the file content from peer */
                        printf("Receiving file content [%s] from peer...\n", filesToAdd[i].name);
                        char *fileData = NULL;
                        ssize_t dataSize = 0;
                        msg = receiveData(socketFd);
                        if (msg && strcmp(msg->buf, "nofile") != 0) {
                            fileData = (char *) msg->buf;
                            dataSize = msg->bufSize;
                            free(msg);
                        } else {
                            fprintf(stderr, "No data received for [%s]!\n", filesToAdd[i].name);
                            continue;
                        }
                        /* Add/update the file in the local directory */
                        if (dataSize == filesToAdd[i].size) {
                            printf("File [%s] received from peer\n", filesToAdd[i].name);

                            /* Get the local file path */
                            char *afterPeerDirName = strstr(filesToAdd[i].path, "/");
                            sprintf(filePath, "%s%s", dirName, afterPeerDirName);

                            /* Create/update the file in the local directory */
                            pthread_mutex_lock(dirMutex);
                            if (createFile(filePath, fileData, dataSize) == -1) {
                                fprintf(stderr, "Error creating file [%s]\n", filesToAdd[i].name);
                                pthread_mutex_unlock(dirMutex);
                                continue;
                            }
                            pthread_mutex_unlock(dirMutex);

                            /* Write log messages */
                            if (filesToAdd[i].sameFile) {
                                sprintf(buf, "File '%s' updated.", filePath);
                                printf("%s\n", buf);
                                if (logFile) {
                                    fprintf(logFile, "%s\n", buf);
                                }
                            } else {
                                sprintf(buf, "File '%s' created.", filePath);
                                printf("%s\n", buf);
                                if (logFile) {
                                    fprintf(logFile, "%s\n", buf);
                                }
                            }
                        } else {
                            fprintf(stderr, "File [%s] corrupted!\n", filesToAdd[i].name);
                        }
                        free(fileData);
                    }
                    free(filesToAdd);
                }
            } else if (strcmp(buf, "bye") == 0) {
                if (logFile) {
                    fclose(logFile);
                }
                return 1;
            }
        }
        if (logFile) {
            fclose(logFile);
        }
    }
    return 0;
}

void syncPeerWithMe(int socketFd, const char *dirName, dir_updates_t *dirUpdates, pthread_mutex_t *dirUpdateMutex) {

    pthread_mutex_lock(dirUpdateMutex);
    if (dirUpdates->deletedCount != 0) {
        /* Send "delete" request */
        sendData(socketFd, "delete", strlen("delete"));
        printf("\"delete\" request sent\n");

        /* Send deleted file list to the peer */
        sendData(socketFd, dirUpdates->deletedFileList, dirUpdates->deletedCount * sizeof(file_info_t));
        printf("Deleted files list sent...\n");
    }
    pthread_mutex_unlock(dirUpdateMutex);

    pthread_mutex_lock(dirUpdateMutex);
    if (dirUpdates->addedUpdatedCount != 0) {
        sendData(socketFd, "add", strlen("add"));

        /* Send added/updated file list to the peer */
        sendData(socketFd, dirUpdates->addedUpdatedFileList, dirUpdates->addedUpdatedCount * sizeof(file_info_t));
        printf("addedUpdatedFileList sent\n");

        /* Send added/updated files to the peer */
        for (int i = 0; i < dirUpdates->addedUpdatedCount; ++i) {
            sendFileToPeer(socketFd, dirName, dirUpdates->addedUpdatedFileList[i]);
            printf("File [%s] sent\n", dirUpdates->addedUpdatedFileList[i].name);
        }
    }
    pthread_mutex_unlock(dirUpdateMutex);

}

void *checkFileChanges(void *args) {
    dir_args_t *check_th_args = (dir_args_t *) args;

    const char *dirName = check_th_args->name;
    dir_updates_t *dirUpdates = check_th_args->updates;
    pthread_mutex_t *dirMutex = check_th_args->mutex;
    pthread_mutex_t *dirUpdateMutex = check_th_args->updateMutex;

    file_info_t *oldFileList = NULL, *newFileList = NULL;
    int oldFileCount = 0, newFileCount = 0;

    dirUpdates->addedUpdatedFileList = NULL;
    dirUpdates->deletedFileList = NULL;

    while (!stopFlag && !serverBye) {
        /* Check the changes on the directory periodically */
        pthread_mutex_lock(dirMutex);
        traverseDirectory(dirName, &oldFileList, &oldFileCount);
        pthread_mutex_unlock(dirMutex);

        sleep(SYNC_PERIOD);

        pthread_mutex_lock(dirMutex);
        traverseDirectory(dirName, &newFileList, &newFileCount);
        pthread_mutex_unlock(dirMutex);

        pthread_mutex_lock(dirUpdateMutex);
        /* Find the deleted files from directory */
        if(dirUpdates->deletedFileList) {
            free(dirUpdates->deletedFileList);
        }
        dirUpdates->deletedFileList = findDeletedFiles(oldFileList, oldFileCount, newFileList, newFileCount,
                                                       &dirUpdates->deletedCount);
        for (int i = 0; i < dirUpdates->deletedCount; ++i) {
            printf("File [%s] deleted\n", dirUpdates->deletedFileList[i].name);
        }
        /* Find the newly added or updated files on directory */
        if(dirUpdates->addedUpdatedFileList) {
            free(dirUpdates->addedUpdatedFileList);
        }
        dirUpdates->addedUpdatedFileList = findAddedUpdatedFiles(newFileList, newFileCount, oldFileList, oldFileCount,
                                                                 &dirUpdates->addedUpdatedCount);
        for (int i = 0; i < dirUpdates->addedUpdatedCount; ++i) {
            file_info_t fileInfo = dirUpdates->addedUpdatedFileList[i];
            if (fileInfo.sameFile) {
                if (fileInfo.sameSize) {
                    int j;
                    for (j = i; j < dirUpdates->addedUpdatedCount - 1; ++j) {
                        dirUpdates->addedUpdatedFileList[j] = dirUpdates->addedUpdatedFileList[j + 1];
                    }
                    dirUpdates->addedUpdatedCount--;
                    continue;
                }
                printf("File [%s] updated\n", dirUpdates->addedUpdatedFileList[i].name);
            } else {
                printf("File [%s] added\n", dirUpdates->addedUpdatedFileList[i].name);
            }
        }
        pthread_mutex_unlock(dirUpdateMutex);

        /* Clean up */
        if (oldFileList) {
            free(oldFileList);
            oldFileList = NULL;
        }
        if (newFileList) {
            free(newFileList);
            newFileList = NULL;
        }
    }

    free(dirUpdates->deletedFileList);
    free(dirUpdates->addedUpdatedFileList);
    printf("Exiting check file thread...\n");
    pthread_exit(NULL);
}







#ifndef FINAL_UTILS_H
#define FINAL_UTILS_H

#include <sys/types.h>
#include <unistd.h>

/* Define to print debugging messages */
#define DEBUG_MOD

#define BUFFER_SIZE 8192
#define MAX_PATH_LENGTH 4096
#define MAX_NAME_LENGTH 255
#define SYNC_PERIOD 1 //SEC

typedef struct {
    void *buf;
    ssize_t bufSize;
} msg_buffer_t;

typedef struct {
    char name[MAX_NAME_LENGTH + 1];
    char path[MAX_PATH_LENGTH + 1];
    off_t size;
    time_t modified;
    int sameFile;
    int sameSize;
    int isSubDirFile;
} file_info_t;

typedef struct {
    int addedUpdatedCount;
    int deletedCount;
    file_info_t *addedUpdatedFileList;
    file_info_t *deletedFileList;
} dir_updates_t;

typedef struct {
    const char *name;
    dir_updates_t *updates;
    pthread_mutex_t *mutex;
    pthread_mutex_t *updateMutex;
} dir_args_t;

/* Indicates that SIGINT signal received
 * or program needs to be shutdown in some case */
extern int stopFlag;

/* Indicates that server is going to shut down */
extern int serverBye;

/* Indicates that this is a server process */
extern int isServer;

/* Gets the IP of the host machine */
char *getHostMachineIP();

/* This function recursively traverses the specified directory,
 * and it collects the file information (name, size, and modification timestamp)
 * and adds it to the dynamically growing array of file_info_t. */
void traverseDirectory(const char *dirname, file_info_t **fileInfos, int *count);

/* This function checks if the given file has the same given data
 * Returns 0 if the two data is identical
 * Returns 1 if the two data are different */
int compareFileContent(const char *filePath, const char *data);

/* This function compares dir1 and dir2 and finds created or updated files on dir1 */
file_info_t *
findAddedUpdatedFiles(file_info_t *dir1, size_t count1, file_info_t *dir2, size_t count2, int *resultCount);

/* This function compares dir1 and dir2 and finds deleted files from dir1 */
file_info_t *findDeletedFiles(file_info_t *dir1, size_t count1, file_info_t *dir2, size_t count2, int *resultCount);

/* Send data which has a size of dataSize */
int sendData(int socketFd, const void *data, size_t dataSize);

/* Receive data using socket file descriptor */
msg_buffer_t *receiveData(int socketFd);

/* This function searches for the given filename in the directory,
 * when it finds opens it for reading and return its file descriptor.
 * If the file is not found, returns -1 */
int openFileInDirectory(const char *directory, const char *filename);

/* This function reads the given file into an array and returns it */
char *readFileContent(int fd);

/* This function creates a new file using the data stored in the buffer */
int createFile(const char *filePath, const char *buffer, size_t bufferSize);

/* This function creates all the parent directories in the path, if not exists */
void createDirectories(const char* filePath);

/* This two function make synchronization between client-server
 * These two should be used together like if the client calls one of these,
 * the server should call the other function at the same time
 *
 * syncPeerWithMeOnConnect: This function sends the requested files to its peer,
 * syncMeWithPeerOnConnect: This function requests added/updated files from its peer.
 * */
int syncPeerWithMeOnConnect(int socketFd, const char *dirName, pthread_mutex_t *dirMutex);

int syncMeWithPeerOnConnect(int socketFd, const char *dirName, pthread_mutex_t *dirMutex);

/* These two function sync with the peer,
 * They should be used after the first sync, in a loop */
int syncMeWithPeer(int socketFd, const char *dirName, pthread_mutex_t *dirMutex);

void syncPeerWithMe(int socketFd, const char *dirName, dir_updates_t *dirUpdates, pthread_mutex_t *dirUpdateMutex);

void *checkFileChanges(void *args);

/* This function send the requested file to the peer */
void sendFileToPeer(int socketFd, const char *dirName, file_info_t requestedFile);

/* This functions searches for the given filename in the directory,
 * when it finds it, deletes it */
void deleteFile(const char *directory, const char *filename);


#endif //FINAL_UTILS_H

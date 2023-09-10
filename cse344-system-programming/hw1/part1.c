/*
 * Gebze Technical University
 * CSE344 - System Programming (Spring 2023)
 * Homework 1 - Part 1
 * */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
    char *endPtr;
    int omitAppendFlag = 0;
    int fd;

    /* Check command-line arguments */
    if(argc < 3 || argc > 4) {
        fflush(stdout);
        fprintf(stderr,
                "Usage: ./appendMeMore filename num-bytes [x]\n");
        fflush(stderr);
        exit(1);
    }

    /* Take the command-line arguments */
    char *filePathName = argv[1];
    long numBytes = strtol(argv[2], &endPtr, 10);
    if (argc == 4 && strcmp(argv[3], "x") == 0)
        omitAppendFlag = 1;

    /* Open file */
    if(omitAppendFlag) {
        fd = open(filePathName, O_WRONLY | O_CREAT);
        if (fd == -1) {
            perror("open");
            return 1;
        }
        /* Append num bytes */
        for (int i = 0; i < numBytes; ++i) {
            if(lseek(fd, 0 , SEEK_END) == -1) {
                perror("lseek");
                close(fd);
                return 1;
            }
            if (write(fd, "b", 1) == -1) {
                perror("write");
                close(fd);
                return 1;
            }
        }
    }
    else {
        fd = open(filePathName, O_WRONLY | O_CREAT | O_APPEND);
        if (fd == -1) {
            perror("open");
            return 1;
        }
        /* Append num bytes */
        for (int i = 0; i < numBytes; ++i) {
            if (write(fd, "b", 1) == -1) {
                perror("write");
                close(fd);
                return 1;
            }
        }
    }

    if(close(fd) == -1) {
        perror("close");
        return 1;
    }

    return 0;
}

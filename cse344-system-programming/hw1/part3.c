/*
 * Gebze Technical University
 * CSE344 - System Programming (Spring 2023)
 * Homework 1 - Part 3
 * */

#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

#define BUFFER_SIZE 50

#define ERROR_2FD(err_str, fd1, fd2) perror(err_str);   \
close(fd1);                                             \
close(fd2);                                             \
return 1

int main() {
    /* Open a file and duplicate it's descriptor */
    int fd1 = open("test_offset_file.txt", O_RDWR | O_CREAT);
    if (fd1 == -1) {
        perror("open test_offset_file.txt");
        return 1;
    }
    int fd2 = dup(fd1);
    if (fd2 == -1) {
        perror("dup fd1");
        close(fd1);
        return 1;
    }
    /* Write something to the file using fd1 */
    char buf[BUFFER_SIZE];
    strcpy(buf, "Check this out!");
    size_t n = strlen(buf);
    if(write(fd1, buf, n) == -1) {
        ERROR_2FD("write from buf to fd1", fd1, fd2);
    }
    /* Set duplicated fd2's offset 4 before end of file */
    if(lseek(fd2, n-4, SEEK_SET) == -1) {
        ERROR_2FD("lseek fd2, 4 before end of file", fd1, fd2);
    }
    /* Write "in!" instead of "out!" using fd1 */
    char buf2[BUFFER_SIZE];
    strcpy(buf2, "in!\n");
    size_t n2 = strlen(buf2);
    if(write(fd1, buf2, n2) == -1) {
        ERROR_2FD("write from buf2 to fd1", fd1, fd2);
    }
    /* Set the offset start of file using fd2 */
    if(lseek(fd2, 0, SEEK_SET) == -1) {
        ERROR_2FD("lseek fd2, start of file", fd1, fd2);
    }
    /* Read and write the resulting text to stdout */
    char buf3[BUFFER_SIZE];
    if(read(fd1, buf3, n) == -1) {
        ERROR_2FD("read from fd2 to buf3", fd1, fd2);
    }
    if(write(1, buf3, n) == -1) {
        ERROR_2FD("write from buf3 to stdout", fd1, fd2);
    }
    close(fd1);
    close(fd2);
    return 0;
}
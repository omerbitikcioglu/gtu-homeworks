/*
 * Gebze Technical University
 * CSE344 - System Programming (Spring 2023)
 * Homework 1 - Part 2
 * */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#define ERROR_FD(err_str, fd) perror(err_str);          \
close(fd);                                              \
return 1

#define ERROR_2FD(err_str, fd1, fd2) perror(err_str);   \
close(fd1);                                             \
close(fd2);                                             \
return 1

#define TEST_MY_DUP 0
#define TEST_MY_DUP2 1

#define OLD_FD old_fd
#define NEW_FD 5

/*
 * Duplicates the file descriptor using the lowest-numbered
 * available file descriptor.
 * */
int my_dup(int oldfd) {
    /* Check if the oldfd is a valid descriptor */
    if (fcntl(oldfd, F_GETFL) == -1) {
        errno = EBADF;
        return -1;
    }
    /* Duplicate the oldfd, newfd >= 0 */
    int newfd = fcntl(oldfd, F_DUPFD, 0);

    return newfd;
}

/*
 * Duplicates the file descriptor using the newfd.
 * If newfd == oldfd, then does nothing.
 * If newfd is open, closes it.
 * */
int my_dup2(int oldfd, int newfd) {
    /* Check if the oldfd is a valid descriptor */
    if (fcntl(oldfd, F_GETFL) == -1) {
        errno = EBADF;
        return -1;
    }
    if(newfd == oldfd)
        return newfd;
    /* Check if newfd is already open */
    if (fcntl(newfd, F_GETFL) != -1)
        close(newfd);
    /* Duplicate the oldfd, result should equal to newfd */
    if (fcntl(oldfd, F_DUPFD, newfd) != newfd) {
        return -1;
    }
    return newfd;
}

/* Test */
int main() {
    int old_fd = open("file.txt", O_RDWR | O_CREAT | O_APPEND);
    if (old_fd == -1) {
        perror("open old_fd");
        return 1;
    }
    /* Write something to the file.txt with old_fd */
    char buf[50];
    strcpy(buf, "Hello from old_fd!\n");
    size_t n = strlen(buf);
    if(write(old_fd, buf, n) == -1) {
        ERROR_FD("write from buf to old_fd", old_fd);
    }
#if TEST_MY_DUP
    int new_fd = my_dup(old_fd);
    if(new_fd == -1) {
        ERROR_FD("my_dup old_fd to new_fd", old_fd);
    }
#endif
#if TEST_MY_DUP2
    int new_fd = my_dup2(OLD_FD, NEW_FD);
    if(new_fd == -1) {
        ERROR_FD("my_dup2 old_fd to new_fd", old_fd);
    }
#endif
    /* Append string to file.txt using new_fd this time */
    char buf2[50];
    strcpy(buf2, "Hello from new_fd!\n");
    size_t n2 = strlen(buf2);
    if(write(new_fd, buf2, n2) == -1) {
        ERROR_2FD("write from buf2 to new_fd", old_fd, new_fd);
    }
    /* Set the newfd to start of file and print */
    if(lseek(new_fd, 0, SEEK_SET) == -1) {
        ERROR_2FD("lseek new_fd to start", old_fd, new_fd);
    }
    char buf3[100];
    if(read(new_fd, buf3, n+n2) == -1) {
        ERROR_2FD("read from new_fd to buf3", old_fd, new_fd);
    }
    if(write(1, buf3, strlen(buf3)) == -1) {
        ERROR_2FD("write from buf3 to stdout", old_fd, new_fd);
    }
    close(old_fd);
    close(new_fd);
    return 0;
}

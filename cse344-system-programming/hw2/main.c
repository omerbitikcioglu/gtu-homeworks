#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>

#define MAX_COMMANDS 20
#define MAX_COMMAND_LENGTH 100

//#define DEBUG_COMMANDS

#define DEALLOCATE_MEMORY for (int k = 0; k < num_commands; ++k) { \
free(commands[k]); \
}

volatile sig_atomic_t sig_flag = 0;

void signal_handler(int signum) {
    switch (signum) {
        case SIGINT:
            printf("\nReceived SIGINT signal\n");
            break;
        case SIGQUIT:
            printf("\nReceived SIGQUIT signal\n");
            break;
        case SIGTERM:
            printf("\nReceived SIGTERM signal\n");
            break;
        case SIGTSTP:
            printf("\nReceived SIGTSTP signal\n");
            break;
        default:
            printf("\nReceived signal %d\n", signum);
    }
    sig_flag = 1;
}

int main(int argc, char *argv[]) {
    /* Usage information */
    if (argc > 1) {
        fflush(stdout);
        fprintf(stderr,
                "Usage: %s\n", argv[0]);
        fflush(stderr);
        exit(1);
    }

    sigset_t blockMask, origMask;
    struct sigaction saCallIgnore, saOrigQuit, saOrigInt, saOrigTerm, saOrigStp, saDefault;

    /* Block SIGCHLD */
    sigemptyset(&blockMask);
    sigaddset(&blockMask, SIGCHLD);
    sigprocmask(SIG_BLOCK, &blockMask, &origMask);

    /* Call ignoring handler for SIGINT, SIGQUIT, SIGTERM and SIGTSTP */
    saCallIgnore.sa_handler = signal_handler;
    saCallIgnore.sa_flags = 0;
    sigemptyset(&saCallIgnore.sa_mask);
    sigaction(SIGINT, &saCallIgnore, &saOrigInt);
    sigaction(SIGQUIT, &saCallIgnore, &saOrigQuit);
    sigaction(SIGTERM, &saCallIgnore, &saOrigTerm);
    sigaction(SIGTSTP, &saCallIgnore, &saOrigStp);

    int stop = 0;
    while (!stop) {
        char input[MAX_COMMAND_LENGTH * MAX_COMMANDS];
        char *commands[MAX_COMMANDS];
        int num_commands = 0;
        int final = 0;

        /* Get user input */
        printf(">");
        if (fgets(input, sizeof(input), stdin) == NULL) {
            if (sig_flag) {
                final = 1;
                sig_flag = 0;
            } else {
                printf("^D\n");
                exit(EXIT_FAILURE);
            }
        }
        if (strcmp(input, "\n") == 0) {
            final = 1;
        }

        /* Parse commands from the user input */
        while (num_commands < MAX_COMMANDS && !final) {
            char *command = malloc(sizeof(char));
            if (command == NULL) {
                DEALLOCATE_MEMORY
                fprintf(stderr, "Error: malloc");
                exit(EXIT_FAILURE);
            }
            command[0] = '\0';
            char *token = NULL;
            if (num_commands == 0)
                token = strtok(input, "|\n");
            else
                token = strtok(NULL, "|\n");

            if (token != NULL) {
                size_t command_len = strlen(token);
                char *temp = (char *) realloc(command, command_len + 1);
                if (temp != NULL) {
                    command = temp;
                    sprintf(command, "%s", token);
                }
                commands[num_commands++] = command;

                /* Quit command */
                if (strcmp(command, ":q") == 0) {
                    stop = 1;
                    final = 1;
                }
            } else {
                free(command);
                final = 1;
            }
        }

#ifdef DEBUG_COMMANDS
        printf("Commands:\n");
        for (int i = 0; i < num_commands; ++i) {
            printf("%s\n", commands[i]);
        }
#endif
        /* Create a child process for every command */
        int status, i;
        pid_t pid;
        char *token;
        char tokens[4][MAX_COMMAND_LENGTH];
        int pipe_fds[num_commands - 1][2];
        char buf[MAX_COMMAND_LENGTH]; /* Log buffers */

        for (i = 0; i < num_commands && !stop; ++i) {
            /* If there is a pipe | on the right */
            if (i + 1 < num_commands) {
                if (pipe(pipe_fds[i]) == -1) {
                    perror("pipe");
                    DEALLOCATE_MEMORY
                    exit(EXIT_FAILURE);
                }
                pid = fork();
                switch (pid) {
                    case -1: /* Fork failed */
                        perror("fork");
                        DEALLOCATE_MEMORY
                        exit(EXIT_FAILURE);
                    case 0: /* Child process - the command on the left of | */
                        saDefault.sa_handler = SIG_DFL;
                        saDefault.sa_flags = 0;
                        sigemptyset(&saDefault.sa_mask);

                        if (saOrigInt.sa_handler != SIG_IGN)
                            sigaction(SIGINT, &saDefault, NULL);
                        if (saOrigQuit.sa_handler != SIG_IGN)
                            sigaction(SIGQUIT, &saDefault, NULL);
                        if (saOrigTerm.sa_handler != SIG_IGN)
                            sigaction(SIGTERM, &saDefault, NULL);
                        if (saOrigStp.sa_handler != SIG_IGN)
                            sigaction(SIGTSTP, &saDefault, NULL);
                        sigprocmask(SIG_SETMASK, &origMask, NULL);

                        /* If there is a pipe on the left */
                        if (i >= 1) {
                            close(pipe_fds[i - 1][1]); /* Close write part */
                            /* Set stdin to read part of pipe */
                            if (dup2(pipe_fds[i - 1][0], STDIN_FILENO) == -1) {
                                perror("dup2");
                                exit(EXIT_FAILURE);
                            }
                            close(pipe_fds[i - 1][0]); /* Close read part */
                        } else if (strchr(commands[i], '<')) { /* Input redirection */
                            /* Parse the command */
                            char *tmp_cmd = (char *) malloc(strlen(commands[i]) + 1);
                            tmp_cmd[0] = '\0';
                            strcpy(tmp_cmd, commands[i]);
                            token = strtok(commands[i], "<");
                            strcpy(tokens[0], token);
                            token = strtok(NULL, " >\n");
                            strcpy(tokens[1], token);
                            strcpy(commands[i], tmp_cmd);
                            free(tmp_cmd);
                            /* Set stdout to the file on the right */
                            int fd = open(tokens[1], O_RDONLY);
                            if (fd == -1) {
                                perror("open");
                                exit(EXIT_FAILURE);
                            }
                            if (dup2(fd, STDIN_FILENO) == -1) {
                                perror("dup2");
                                exit(EXIT_FAILURE);
                            }
                            close(fd);
                        }
                        close(pipe_fds[i][0]); /* Close read part */
                        /* Set stdout to write part of pipe */
                        if (dup2(pipe_fds[i][1], STDOUT_FILENO) == -1) {
                            perror("dup2");
                            exit(EXIT_FAILURE);
                        }
                        close(pipe_fds[i][1]); /* Close write part */

                        if (strchr(commands[i], '<')) {
                            execl("/bin/sh", "sh", "-c", tokens[0], (char *) NULL);
                        } else {
                            execl("/bin/sh", "sh", "-c", commands[i], (char *) NULL);
                        }
                        /* Failed execl */
                        perror("execl");
                        _exit(127);
                    default: /* Parent process */
                        close(pipe_fds[i][1]); /* Close write part */
                        pid_t wait_pid;
                        do {
                            wait_pid = waitpid(pid, &status, WUNTRACED);
                        } while (wait_pid == -1 && errno == EINTR);
                        if (wait_pid == -1) {
                            perror("wait");
                            status = -1;
                            final = 1;
                        } else {
                            /* Child process exit status */
                            if (WIFEXITED(status)) {
                                printf("Child process [%d] exited with status %d\n", pid, WEXITSTATUS(status));
                                sprintf(buf, "Process [%d] - %s\n", pid, commands[i]);
                            } else if (WIFSIGNALED(status)) {
                                printf("Child process [%d] terminated by signal %d\n", pid, WTERMSIG(status));
                                sprintf(buf, "Process [%d] - %s\n", pid, commands[i]);
                                if (WTERMSIG(status) == SIGKILL) {
                                    stop = 1;
                                    final = 1;
                                }
                            } else if WIFSTOPPED(status)
                            {
                                printf("Child process [%d] stopped by signal %d\n", pid, WSTOPSIG(status));
                                sprintf(buf, "Process [%d] - %s\n", pid, commands[i]);
                            }
                        }
                        break;
                }
            }
            else {
                pid = fork();
                switch (pid) {
                    case -1: /* Fork failed */
                        perror("fork");
                        DEALLOCATE_MEMORY
                        exit(EXIT_FAILURE);
                    case 0: /* Child process */
                        saDefault.sa_handler = SIG_DFL;
                        saDefault.sa_flags = 0;
                        sigemptyset(&saDefault.sa_mask);

                        if (saOrigInt.sa_handler != SIG_IGN)
                            sigaction(SIGINT, &saDefault, NULL);
                        if (saOrigQuit.sa_handler != SIG_IGN)
                            sigaction(SIGQUIT, &saDefault, NULL);
                        if (saOrigTerm.sa_handler != SIG_IGN)
                            sigaction(SIGTERM, &saDefault, NULL);
                        if (saOrigStp.sa_handler != SIG_IGN)
                            sigaction(SIGTSTP, &saDefault, NULL);
                        sigprocmask(SIG_SETMASK, &origMask, NULL);

                        /* If there is a pipe on the left */
                        if (i >= 1) {
                            close(pipe_fds[i - 1][1]); /* Close write part */
                            /* Set stdin to read part of pipe */
                            if (dup2(pipe_fds[i - 1][0], STDIN_FILENO) == -1) {
                                perror("dup2");
                                exit(EXIT_FAILURE);
                            }
                            close(pipe_fds[i - 1][0]); /* Close read part */
                        } else if (strchr(commands[i], '<')) { /* Input redirection */
                            /* Parse the command */
                            char *tmp_cmd = (char *) malloc(strlen(commands[i]) + 1);
                            tmp_cmd[0] = '\0';
                            strcpy(tmp_cmd, commands[i]);
                            token = strtok(commands[i], "<");
                            strcpy(tokens[0], token);
                            token = strtok(NULL, " >\n");
                            strcpy(tokens[1], token);
                            strcpy(commands[i], tmp_cmd);
                            free(tmp_cmd);
                            /* Set stdout to the file on the right */
                            int fd = open(tokens[1], O_RDONLY);
                            if (fd == -1) {
                                perror("open");
                                exit(EXIT_FAILURE);
                            }
                            if (dup2(fd, STDIN_FILENO) == -1) {
                                perror("dup2");
                                exit(EXIT_FAILURE);
                            }
                            close(fd);
                        }
                        if (strchr(commands[i], '>')) { /* Output file redirection */
                            /* Parse the command */
                            char *tmp_cmd = (char *) malloc(strlen(commands[i]) + 1);
                            tmp_cmd[0] = '\0';
                            strcpy(tmp_cmd, commands[i]);
                            token = strtok(commands[i], ">");
                            strcpy(tokens[2], token);
                            token = strtok(NULL, " \n");
                            strcpy(tokens[3], token);
                            strcpy(commands[i], tmp_cmd);
                            free(tmp_cmd);
                            /* Set stdout to the file on the right */
                            int fd = open(tokens[3], O_WRONLY | O_TRUNC | O_CREAT, 0666);
                            if (fd == -1) {
                                perror("open");
                                exit(EXIT_FAILURE);
                            }
                            if (dup2(fd, STDOUT_FILENO) == -1) {
                                perror("dup2");
                                exit(EXIT_FAILURE);
                            }
                            close(fd);
                        }
                        if (strchr(commands[i], '<')) {
                            execl("/bin/sh", "sh", "-c", tokens[0], (char *) NULL);
                        } else if (strchr(commands[i], '>')) {
                            execl("/bin/sh", "sh", "-c", tokens[2], (char *) NULL);
                        } else {
                            execl("/bin/sh", "sh", "-c", commands[i], (char *) NULL);
                        }
                        /* Failed execl */
                        perror("execl");
                        _exit(127);
                    default: /* Parent process */
                        pid_t wait_pid;
                        do {
                            wait_pid = waitpid(pid, &status, WUNTRACED);
                        } while (wait_pid == -1 && errno == EINTR);
                        if (wait_pid == -1) {
                            perror("wait");
                            status = -1;
                            final = 1;
                        } else {
                            /* Child process exit status */
                            if (WIFEXITED(status)) {
                                printf("Child process [%d] exited with status %d\n", pid, WEXITSTATUS(status));
                                sprintf(buf, "Process [%d] - %s\n", pid, commands[i]);
                            } else if (WIFSIGNALED(status)) {
                                printf("Child process [%d] terminated by signal %d\n", pid, WTERMSIG(status));
                                sprintf(buf, "Process [%d] - %s\n", pid, commands[i]);
                                if (WTERMSIG(status) == SIGKILL) {
                                    stop = 1;
                                    final = 1;
                                }
                            } else if WIFSTOPPED(status)
                            {
                                printf("Child process [%d] stopped by signal %d\n", pid, WSTOPSIG(status));
                                sprintf(buf, "Process [%d] - %s\n", pid, commands[i]);
                            }
                        }
                        break;
                }
            }

            /* Get the timestamp and open file with it */
            time_t t = time(NULL);
            struct tm *tm = localtime(&t);
            char timestamp[50];
            strftime(timestamp, sizeof(timestamp), "%Y-%m-%d_%H:%M:%S.txt", tm);

            int fd = open(timestamp, O_WRONLY | O_CREAT | O_APPEND, 0666);
            if (fd == -1) {
                perror("open");
                exit(EXIT_FAILURE);
            }

            /* Write the buffer to the timestamped file */
            if (write(fd, buf, strlen(buf)) == -1) {
                perror("write");
                exit(EXIT_FAILURE);
            }
            close(fd);
        }
        DEALLOCATE_MEMORY
    }

    /* Unblock SIGCHLD, restore dispositions for other signals */
    int savedErrno = errno;
    sigprocmask(SIG_SETMASK, &origMask, NULL);
    sigaction(SIGINT, &saOrigInt, NULL);
    sigaction(SIGQUIT, &saOrigQuit, NULL);
    sigaction(SIGTERM, &saOrigQuit, NULL);
    sigaction(SIGTSTP, &saOrigQuit, NULL);
    errno = savedErrno;

    return 0;
}

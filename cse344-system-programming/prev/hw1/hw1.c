#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#define BYTES_READ 1048576

char* read_input_file(int fd);
char* replace(char *buf, char* str1, char* str2);
char* ins_replace(char *buf, char* str1, char* str2);
int update_file(int fd, char *new_buf);

int main(int argc, char *argv[])
{
	// Our program needs 3 arguments to run
	if (argc != 3)
	{
		fprintf(stderr, "Usage: %s ‘/str1/str2/‘ inputFilePath\n", argv[0]);
		return 1;
	}

	// Given user command to do replacement
	char *command = argv[1];

	// Input file path to do string replacement
	const char *path = argv[2];	

	// Open the file with read and write options
	int fd = open(path, O_RDWR);
	if (fd == -1) {
		perror("Failed to open file");
		return 1;
	}

	// Read from file to buffer
	char *buf = read_input_file(fd);
	if (buf == NULL)
	{
		return 1;
	}

	// Get commands and apply them one by one
	const char cdelim[2] = ";";
	const char sdelim[2] = "/";
	char *saveptr1, *saveptr2, *new_buf;
	char *tmp = buf;
	char *acommand = strtok_r(command, cdelim, &saveptr1);
	do {
		printf("%s\n", acommand);

		// Get str1 and str2 from the acommand argument
		char *str1 = strtok_r(acommand, sdelim, &saveptr2);
		char *str2 = strtok_r(NULL, sdelim, &saveptr2);
		char *insensitive = strtok_r(NULL, sdelim, &saveptr2);

		// Create a new buffer of replaced strings
		
		if (insensitive != NULL) {
			printf("%s\n", insensitive);
			if (strcmp(insensitive, "i") == 0) {
				new_buf = ins_replace(tmp, str1, str2);
			}
		} else {
			new_buf = replace(tmp, str1, str2);
		}

		// Write replaced string new buffer to the input file
		if (update_file(fd, new_buf) == 1) {
			return 1;
		}
		tmp = new_buf;

	} while ((acommand = strtok_r(NULL, cdelim, &saveptr1)) != NULL);

	free(new_buf);
	free(buf);
	close(fd);

	return 0;
}

// Read the input file
char* read_input_file(int fd) {

	char *buf = (char *) calloc(BYTES_READ, sizeof(char)); 
	int bytesread = -1;

	for (int i = 0; bytesread != 0; ++i)
	{
		// Read and escape system interrupts if happens
		while(((bytesread = read(fd, buf, BYTES_READ)) == -1) && 
			(errno == EINTR));
		
		if(bytesread == -1) // Error other than interrupt
		{
			perror("Failed to read file");
			return NULL;
		}
	}

	return buf;
}

// Create a new buffer of replaced strings
char* replace(char *buf, char* str1, char* str2) {

	int strlen1 = strlen(str1);
	char *new_buf = calloc(BYTES_READ, sizeof(char));
	char *cursor = buf;
	char *p;
	do {
		p = strstr(cursor, str1);

		if (p == NULL)
		{
			strcat(new_buf, cursor); // Add the rest
		}
		else
		{
			strncat(new_buf, cursor, p-cursor);
			strcat(new_buf, str2);
			cursor += (p-cursor) + strlen1;
		}

	} while(p != NULL);

	return new_buf;
}

// Create a new buffer of (case insensitive) replaced strings
char* ins_replace(char *buf, char* str1, char* str2) {

	int strlen1 = strlen(str1);
	char *new_buf = calloc(BYTES_READ, sizeof(char));
	char *cursor = buf;
	char *p;
	do {
		p = strcasestr(cursor, str1);

		if (p == NULL)
		{
			strcat(new_buf, cursor); // Add the rest
		}
		else
		{
			strncat(new_buf, cursor, p-cursor);
			strcat(new_buf, str2);
			cursor += (p-cursor) + strlen1;
		}

	} while(p != NULL);

	return new_buf;
}

// Write replaced string in the buffer to the input file
int update_file(int fd, char *new_buf) {

	int byteswritten = 0;
	int newbuf_len = strlen(new_buf);
	
	while(byteswritten < newbuf_len) {

		// Reposition the file offset to the beginning
		if(lseek(fd, 0, SEEK_SET) == -1) {
			perror("Lseek failed");
		}

		// Write and escape system interrupts if happens
		while(((byteswritten = write(fd, new_buf, newbuf_len)) == -1) &&
			(errno == EINTR));

		if (byteswritten == -1) // Error other than interrupt
		{
			perror("Failed to write file");
			return 1;
		}
	}

	return 0;
}
	

	
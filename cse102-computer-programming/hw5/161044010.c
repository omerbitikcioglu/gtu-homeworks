#include<stdio.h>
#include<stdlib.h>
#include<time.h>

#define DICT_SIZE 15
#define WORD_LEN 10
#define LINE_LEN 18

/*Directions*/
enum{E, SE, S, SW, W, NW, N, NE};

void make_upper(char map[DICT_SIZE][DICT_SIZE], int y, int x, int dir, int n)
{
	if(n>0)
	{
		/*If it is a lower case make it upper*/
		if(map[y][x]<='z' && map[y][x]>='a') 
		{
			map[y][x]-='a';
			map[y][x]+='A';	
		}

		switch(dir) /*Go make upper the next char of the word*/
		{
			case E:	make_upper(map, y,   x+1, E,  n-1);	
					break;				
			case SE:make_upper(map, y+1, x+1, SE, n-1);
					break;					
			case S:	make_upper(map, y+1, x,   S,  n-1);
					break;				
			case SW:make_upper(map, y+1, x-1, SW, n-1);
					break;					
			case W:	make_upper(map, y,   x-1, W,  n-1);
					break;		
			case NW:make_upper(map, y-1, x-1, NW, n-1);
					break;		
			case N:	make_upper(map, y-1, x,   N,  n-1);
					break;						
			case NE:make_upper(map, y-1, x+1, NE, n-1);
					break;		
		}
	}
}

/*Checks 8 direction to control if the input word matches with the input coordinate, from end-to-beginning*/
int check_word_end(char map[DICT_SIZE][DICT_SIZE], char word[LINE_LEN], int y, int x, int dir, int n)
{
	if(n<0) /*Matches*/
		return 1;
	else if(y<DICT_SIZE && x<DICT_SIZE && y>=0 && x>=0 && (word[n]==map[y][x] || word[n]==map[y][x]-'A'+'a'))
	{
		switch(dir)
		{
			case E:	return check_word_end(map, word, y,   x+1, E,  n-1);					
			case SE:return check_word_end(map, word, y+1, x+1, SE, n-1);					
			case S:	return check_word_end(map, word, y+1, x,   S,  n-1);				
			case SW:return check_word_end(map, word, y+1, x-1, SW, n-1);					
			case W:	return check_word_end(map, word, y,   x-1, W,  n-1);		
			case NW:return check_word_end(map, word, y-1, x-1, NW, n-1);		
			case N:	return check_word_end(map, word, y-1, x,   N,  n-1);						
			case NE:return check_word_end(map, word, y-1, x+1, NE, n-1);		
		}
	}
	else /*No match*/
		return 0;
}

/*Checks 8 direction to control if the input word matches with the input coordinate, from beginning-to-end */
int check_word_begin(char map[DICT_SIZE][DICT_SIZE], char word[LINE_LEN], int y, int x, int dir)
{
	if(word[0]=='\0') /*Matches*/
		return 1;
	else if(y<DICT_SIZE && x<DICT_SIZE && y>=0 && x>=0 && (word[0]==map[y][x] || word[0]==map[y][x]-'A'+'a'))
	{
		switch(dir)
		{
			case E:	return check_word_begin(map, word+1, y,   x+1, E);					
			case SE:return check_word_begin(map, word+1, y+1, x+1, SE);					
			case S:	return check_word_begin(map, word+1, y+1, x,   S);				
			case SW:return check_word_begin(map, word+1, y+1, x-1, SW);					
			case W:	return check_word_begin(map, word+1, y,   x-1, W);		
			case NW:return check_word_begin(map, word+1, y-1, x-1, NW);		
			case N:	return check_word_begin(map, word+1, y-1, x,   N);						
			case NE:return check_word_begin(map, word+1, y-1, x+1, NE);		
		}
	}
	else /*No match*/
		return 0;
}

/*Prints the puzzle map*/
void print_puzzle(char map[DICT_SIZE][DICT_SIZE])
{
	for(int i=0; i<DICT_SIZE; ++i)
	{
		for(int j=0; j<DICT_SIZE; ++j)
			printf("%c ", map[i][j]);
		printf("\n");
	}
	printf("\n");
}

/*Places the words from the dict on to the map based on coordinates*/
void place_words(char *dict[DICT_SIZE], int coord[DICT_SIZE][4], char map[DICT_SIZE][DICT_SIZE])
{
	int i, j, x1, y1, x2, y2;

	for(i=0; i<DICT_SIZE; ++i)
	{
		/*Coord of the beginning of the word*/
		y1 = coord[i][0];
		x1 = coord[i][1];
		/*Coord of the ending of the word*/
		y2 = coord[i][2];
		x2 = coord[i][3];

		if(x1 == x2 && y1 != y2) /*Vertical*/
		{
			if(y2>y1)
			{
				for(j=0; dict[i][j]!='\0' && y1<=y2; ++j, ++y1)
					map[y1][x1]=dict[i][j];
			}
			else
			{
				for(j=0; dict[i][j]!='\0' && y1>=y2; ++j, --y1)
					map[y1][x1]=dict[i][j];
			}
		}
		else if(x1 != x2 && y1 == y2) /*Horizontal*/
		{
			if(x2>x1)
			{
				for(j=0; dict[i][j]!='\0' && x1<=x2; ++j, ++x1)
					map[y1][x1]=dict[i][j];	
			}
			else
			{
				for(j=0; dict[i][j]!='\0' && x1>=x2; ++j, --x1)
					map[y1][x1]=dict[i][j];	
			}
		}
		else /*Diagonal*/
		{
			if(x2>x1 && y2>y1)
			{
				for(j=0; dict[i][j]!='\0' && x1<=x2 && y1<=y2; ++j, ++x1, ++y1)
					map[y1][x1]=dict[i][j];	
			}
			else if(x2>x1 && y2<y1)
			{
				for(j=0; dict[i][j]!='\0' && x1<=x2 && y1>=y2; ++j, ++x1, --y1)
					map[y1][x1]=dict[i][j];	
			}
			else if(x2<x1 && y2<y1)
			{
				for(j=0; dict[i][j]!='\0' && x1>=x2 && y1>=y2; ++j, --x1, --y1)
					map[y1][x1]=dict[i][j];	
			}
			else
			{
				for(j=0; dict[i][j]!='\0' && x1>=x2 && y1<=y2; ++j, --x1, ++y1)
					map[y1][x1]=dict[i][j];	
			}
		}
	}
}

/*Checks if the two strings are equal*/
int cmp_str(char *str1, char *str2)
{
	int error=0;
	for(int i=0; str1[i]!='\0' && str2[i]!='\0'; ++i)
	{
		if(str1[i]!=str2[i])
			error=1;
		else if((str1[i+1]=='\0' && str2[i+1]!='\0') || (str1[i+1]!='\0' && str2[i+1]=='\0'))
			error=1;
	}

	return error; /*Returns 0 if two strings are equal*/
}

char random_char(void)
{
	char rand_ch;

	/*Get a random character between 'a' and 'z'*/
	do{
		rand_ch = rand()%'z'+1;	
	}while(rand_ch<'a');

	return rand_ch;
}

int get_line_size(char *line) 
{
	char *ch_iter = line; /*so as not to lose beginning of line*/
	int counter = 0;
	/*go until you see new line or null char*/
	while(*ch_iter != '\n' && *ch_iter != '\0') 
	{
		ch_iter++; /* next char */
		counter++; /* increment counter */
	}
	
	return counter;
}

void copy_string(char *source, char *destination) 
{
	/* get iterators over original pointers */
	char *src_iter = source;
	char *dst_iter = destination;
	/* until null char */
	while (*src_iter != '\0') 
	{
		/* copy pointers */
		*dst_iter = *src_iter;
		/* advance to next char */
		src_iter++;
		dst_iter++;
	}
   /* terminate string */
   *dst_iter = '\0';
}

void remove_newline(char *line) 
{
	char *ch_iter = line;
	/* go until you see new line */
	while(*ch_iter != '\n') {
		ch_iter++; /* next char */
	}
	*ch_iter = '\0'; /* overwrite new line */
}

int main(void)
{
	char *dict[DICT_SIZE];
	int coord[DICT_SIZE][4];    
	char line[LINE_LEN];
	char map[DICT_SIZE][DICT_SIZE];
	FILE *fp = fopen("word_hunter.dat", "r");
	int i, j, n, words_found=0, exit=0, guess_y, guess_x, found=0, flag=0;

	srand(time(NULL));
	
	int line_counter = 0;
	int dict_counter = 0;
	while(fgets(line, LINE_LEN, fp) != NULL) {
		if(line_counter%5 == 0) {
			dict[dict_counter] = (char*) malloc(sizeof(char) * get_line_size(line));
			remove_newline(line);
			copy_string(line, dict[dict_counter]);
		} else if (line_counter%5 == 1){
			coord[dict_counter][0] = atoi(line);
		} else if (line_counter%5 == 2){			
			coord[dict_counter][1] = atoi(line);		
		} else if (line_counter%5 == 3){
			coord[dict_counter][2] = atoi(line);
		} else if (line_counter%5 == 4){
			coord[dict_counter][3] = atoi(line);
			dict_counter++;
		}
		line_counter++;
	}
	
	fclose(fp);
	
	/*Fills the map with random chars*/
	for(i=0; i<DICT_SIZE; ++i)
	{
		for(j=0; j<DICT_SIZE; ++j)
			map[i][j]=random_char();
	}

	place_words(dict, coord, map);

	/*Gameplay*/
	print_puzzle(map);
	while(words_found!=DICT_SIZE && !exit)
	{
		printf("Word: ");
		fgets(line, LINE_LEN, stdin);
		remove_newline(line);
		n=get_line_size(line);

		if(!cmp_str(line, "exit game")) /*Exit the game*/
			exit=1;
		else  
		{
			printf("Coordinate: ");
			scanf("%d%d", &guess_y, &guess_x);
			getchar();	

			if(map[guess_y][guess_x]==line[0]) /*Matches with beginning of the word*/
			{
				found = check_word_begin(map, line, guess_y, guess_x, E);
				if(found){
					make_upper(map, guess_y, guess_x, E, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_begin(map, line, guess_y, guess_x, SE);

				if(found && !flag){
					make_upper(map, guess_y, guess_x, SE, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_begin(map, line, guess_y, guess_x, S);

				if(found && !flag){
					make_upper(map, guess_y, guess_x, S, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_begin(map, line, guess_y, guess_x, SW);

				if(found && !flag){
					make_upper(map, guess_y, guess_x, SW, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_begin(map, line, guess_y, guess_x, W);

				if(found && !flag){
					make_upper(map, guess_y, guess_x, W, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_begin(map, line, guess_y, guess_x, NW);

				if(found && !flag){
					make_upper(map, guess_y, guess_x, NW, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_begin(map, line, guess_y, guess_x, N);

				if(found && !flag){
					make_upper(map, guess_y, guess_x, N, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_begin(map, line, guess_y, guess_x, NE);

				if(found && !flag){
					make_upper(map, guess_y, guess_x, NE, n);
					flag=1;
				}
			}
			if(map[guess_y][guess_x]==line[n-1] && !flag) /*Matches with end of the word*/
			{
				found = check_word_end(map, line, guess_y, guess_x, E, n-1);
				if(found){
					make_upper(map, guess_y, guess_x, E, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_end(map, line, guess_y, guess_x, SE, n-1);;

				if(found && !flag){
					make_upper(map, guess_y, guess_x, SE, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_end(map, line, guess_y, guess_x, S,  n-1);

				if(found && !flag){
					make_upper(map, guess_y, guess_x, S, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_end(map, line, guess_y, guess_x, SW, n-1);

				if(found && !flag){
					make_upper(map, guess_y, guess_x, SW, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_end(map, line, guess_y, guess_x, W,  n-1);

				if(found && !flag){
					make_upper(map, guess_y, guess_x, W, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_end(map, line, guess_y, guess_x, NW, n-1);

				if(found && !flag){
					make_upper(map, guess_y, guess_x, NW, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_end(map, line, guess_y, guess_x, N,  n-1);

				if(found && !flag){
					make_upper(map, guess_y, guess_x, N, n);
					flag=1;
				}
				else if(!flag)
					found = check_word_end(map, line, guess_y, guess_x, NE, n-1);

				if(found && !flag){
					make_upper(map, guess_y, guess_x, NE, n);
					flag=1;
				}
			}
			
			print_puzzle(map);

			if(found) /*Word found*/
			{
				printf("%s is found!\n", line);
				words_found++;
				found=0;
				flag=0;
			}
			else
				printf("No match, be careful next time.\n");

			printf("You found %d words, %d left.\n", words_found, DICT_SIZE-words_found);
		}
	}

	if(words_found==DICT_SIZE){
		print_puzzle(map);
		printf("You found all words on the puzzle. Congratz!..\n");
	}

	return 0;
}







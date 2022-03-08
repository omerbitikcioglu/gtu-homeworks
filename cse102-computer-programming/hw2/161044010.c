#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

/*Macros for the lucky number game*/
#define TRIAL 10
#define MIN 1
#define MAX 1024

enum{human_win, comp_win}; /*For lucky number game*/
enum{left, right};	/*To determine mountain's direction*/

void menu (void);
int make_a_guess (int trial, int min, int max);
void show_scores (int score_human, int score_program);
int find_distance (int dif);
int get_input_height (void);
void draw_hourglass (int height);
void draw_mountain_road (int length, int max_radius);

int main(void)
{
	srand(time(NULL));
	menu();
	return (0);
}

void menu(void)
{
	int choice, exit = 0, who_won, score_human=0, score_program=0, height, length, max_radius;
	
	do
	{
		printf("***** MENU *****\n");
		printf("1. Play Lucky Number\n");
		printf("2. Draw Hourglass\n");
		printf("3. Draw Mountain Road\n");
		printf("4. Exit\n");
		
		printf("Choice: ");
		scanf("%d", &choice);
		printf("\n");

		switch(choice)
		{
			case 1:		who_won = make_a_guess (TRIAL, MIN, MAX);
						if(who_won == human_win)
							score_human++;
						else
							score_program++;
						show_scores (score_human, score_program);
						break;
			
			case 2:		height = get_input_height();
						draw_hourglass(height);
						break;
			
			case 3:		printf("Please enter length and max radius values respectively.\n");
						scanf("%d%d", &length, &max_radius);
						draw_mountain_road (length, max_radius);
						break;
			
			case 4:		exit = 1;
						break;
			
			default:	printf("This is an invalid choice. Try again!\n\n");
						break;
		}	
	}
	while(!exit);
}

int make_a_guess (int trial, int min, int max)
{
	int lucky_num, guess, var_trial=0, dif, distance, who_won;

	/*Program picks a random lucky number in the range of [min,max]*/
	for(lucky_num = rand()%(max+1); 
		lucky_num < min; 
		lucky_num = rand()%(max+1));
	do
	{
		do {
			printf("(Trial: %d) Make a guess between %d and %d: ", var_trial+1, min, max);
			scanf("%d", &guess);	
			var_trial++;
		}while(guess<min || guess >max);

		dif = abs(lucky_num-guess);
		distance = find_distance(dif);
		printf("Distance: %d\n", distance);

		/*Rearranging the interval*/
		if(lucky_num > guess)
			min = guess;
		else
			max = guess;		
	}
	while(dif != 0 && var_trial != TRIAL);

	if(dif == 0) 
	{
		printf("Correct guess, you win!\n");
		who_won = human_win;
	}
	else if(var_trial == TRIAL)
	{
		printf("Out of trial limit. Program win...\n");
		who_won = comp_win;
	}

	return who_won;
}

/*It finds the distance between guess number and lucky number as power of 2*/
int find_distance (int dif)
{
	int i, distance=0;

	for(i=0; distance<=dif; ++i)
		distance = pow(2,i);

	return (i-1);
}

/*It prints human score and program score for the lucky number game*/
void show_scores (int score_human, int score_program)
{
	printf("Your Score: %d Program Score: %d\n\n", score_human, score_program);
}

/*It gets an odd height value for the draw hourglass program*/
int get_input_height (void)
{
	int height;

	printf("Height = ");
	/*Get an odd value for height*/
	for(scanf("%d", &height); height % 2 == 0; scanf("%d", &height))
	{
		printf("Invalid input! Please enter a odd number...\n");
		printf("Height = ");
	}
	return height;
}

void draw_hourglass (int height)
{
	int i, j, star, space=0;
	
	/*Drawing top of the hourglass*/
	star = height;
	for(i=0; i<(height/2)+1; ++i)
	{
		/*Printing spaces*/
		for(j=space; j>0; --j)
			printf(" ");
		/*Printing stars*/
		for(j=star; j>0; --j) 
			printf("*");
		printf("\n");
		star-=2;
		space++;
	}
	/*Drawing bottom of the hourglass*/
	star = 3;
	space -= 2;
	for(i=0; i<(height/2); ++i)
	{
		/*Printing spaces*/
		for(j=space; j>0; --j)
			printf(" ");
		/*Printing stars*/
		for(j=star; j>0; --j) 
			printf("*");
		printf("\n");
		star+=2;
		space--;
	}
	printf("\n");
}

void draw_mountain_road (int length, int max_radius)
{
	int i, j, k, direction=left, radius, space;

	for(i=0; i<length; ++i) /*Amount of half circles*/
	{
		radius = rand()%(max_radius+1);

		if(i==0)
			space = max_radius; /*Enough space for all half circles*/

		if(direction == left) 
		{
			for(j=0; j<radius; ++j, space--) /*Top of half circle*/ 
			{
				for(k=space; k>0; --k)
					printf(" ");
				printf("/\n");
			}
			for(k=space; k>0; --k)
				printf(" ");
			printf("|\n"); /*The peak*/
			space++;
			for(j=0; j<radius; ++j, space++) /*Bottom of half circle*/ 
			{
				for(k=space; k>0; --k)
					printf(" ");
				printf("\\\n");
			}
			direction = right; /*Change direction*/
		}
		else if(direction == right)
		{
			for(j=0; j<radius; ++j, space++) /*Top of half circle*/ 
			{
				for(k=space; k>0; --k)
					printf(" ");
				printf("\\\n");
			}
			for(k=space; k>0; --k)
				printf(" ");
			printf("|\n"); /*The peak*/
			space--;
			for(j=0; j<radius; ++j, space--) /*Bottom of half circle*/ 
			{
				for(k=space; k>0; --k)
					printf(" ");
				printf("/\n");
			}
			direction = left; /*Change direction*/
		}
	}
	printf("\n");
}
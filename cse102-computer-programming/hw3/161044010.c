#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define HWS_LABS 10

void menu (void);
void part1_menu (void);
int adds (int number1, int number2);
int subs (int number1, int number2);
int mult (int number1, int number2);
int divs (int number1, int number2);
int pwr (int number1, int number2);
int mods (int number1, int number2);
int doit (int (*f)(int, int), int number1, int number2);
int get_inputs(char *op, char num1[], char num2[], char *c);
int take_grades(int[]);
int take_exam_grades(int[]);
double calculate_homework(int[]);
double calculate_lab(int[]);
double calculate_all(int, int[], int);
void draw_shape(int x);

int main(void)
{
	menu();
	return(0);
}

void menu(void)
{
	int choice, exit = 0, hws[HWS_LABS], labs[HWS_LABS], exams[2], height;
	double mean_hw, mean_lab;
	
	do
	{
		printf("\n***** MENU *****\n");
		printf("1. Part1\n");
		printf("2. Part2\n");
		printf("3. Part3\n");
		printf("4. Exit\n");
		
		printf("Choice: ");
		scanf("%d", &choice);
		printf("\n");

		switch(choice)
		{
			case 1:		part1_menu();
						break;
			
			case 2:		printf("Enter %d homework grades\n", HWS_LABS);
						take_grades(hws);
						printf("Enter %d lab grades\n", HWS_LABS);
						take_grades(labs);
						take_exam_grades(exams);
						mean_hw = calculate_homework(hws);
						mean_lab = calculate_lab(labs);
						printf("Weighted average = %.2lf\n", calculate_all(mean_hw, exams, mean_lab));
						break;
			
			case 3:		printf("Enter height of the shape...\n");
						scanf("%d", &height);
						draw_shape(height);
						break;
			
			case 4:		exit = 1;
						break;
			
			default:	printf("This is an invalid choice. Try again!\n\n");
						break;
		}	
	}
	while(!exit);
}

void part1_menu(void)
{
	char op, ch, num1_s[11], num2_s[11];
	int num1, num2, exit=0, result=0, error=0;

	printf("Welcome to the simple calculator!\n");
	printf("Type +, -, *, /, ** or %%\n");
	printf("and input/inputs respectively\n");
	printf("To exit, type X...\n");

	do
	{
		error = get_inputs(&op, num1_s, num2_s, &ch);

		if(!error)
		{
			num1 = atoi(num1_s);
			num2 = atoi(num2_s);

			if(strcmp(num2_s, "empty")==0) /*If there is just one input number*/
			{
				num2 = num1;
				num1 = result;
			}

			switch(op)
			{
				case '+':	result = doit(adds, num1, num2);
							break;
				
				case '-':	result = doit(subs, num1, num2);
							break;
				
				case '*':	result = doit(mult, num1, num2);
							break;

				case '^':	if(num1 == 0 && num2 == 0)
								error=1;
							else
								result = doit(pwr, num1, num2);
							break;

				case '/':	result = doit(divs, num1, num2);
							break;
				
				case '%':	result = doit(mods, num1, num2);
							break;
				
				default: 	error=1;
							break;
			}
		}

		if(!error)
			printf("%d\n", result);
		else
		{
			if(error == 1)
			{
				printf("Invalid input/ inputs!\n");
				result=0;
			}
			else if(error == -1)
				exit=1;
				
			while(ch != '\n' && ch != EOF)
				ch = getchar();
		}
		error = 0;
	}
	while(!exit);
}

int adds(int number1, int number2)
{
	return number1 + number2;
}

int subs(int number1, int number2)
{
	return number1 - number2;
}

int mult(int number1, int number2)
{
	return number1 * number2;
}

int divs(int number1, int number2)
{
	return number1 / number2;
}

int pwr(int number1, int number2)
{
	int result = 1;

	for(int i = 0; i<number2; ++i)
		result *= number1;

	return result;
}

int mods(int number1, int number2)
{
	return number1 % number2;
}

int doit (int (*f)(int, int), int number1, int number2)
{
	return (*f)(number1, number2);
}

/*Gets valid inputs for the simple calculator*/
int get_inputs(char *op, char num1[], char num2[], char *c)
{
	char nextch;
	int i, error=0, exit=0;

	/*Gets the operation symbol*/
	do {*op = getchar();} 
	while(*op == '\n' || *op == ' ');
	
	nextch = getchar();

	if(*op == 'X' || *op == 'x' && nextch == '\n')
		exit=1; 
	else if(nextch != ' ')
	{
		if(nextch == '*')
		{
			*op = '^';
			nextch = getchar();
			if(nextch != ' ')
				error=1;
		}
		else
			error=1;
	}

	*c = nextch; /*Continue from nextch*/
	if(!error && !exit)
	{
		/*Getting num1, the first input number*/
		for(i=0, *c=getchar(); *c!=' ' && *c!='\n' && *c!= EOF; *c=getchar(), ++i)
		{
			if((*c >= '0' && *c <= '9') || (*c == '-') || (*c == '+'))
				num1[i] = *c;
			else
				error=1;
		}
		num1[i] = '\0';	

		if(*c == '\n')
			strcpy(num2, "empty");
	}
	
	if(!error && !exit && *c==' ')
	{
		/*Getting num2, the second input number*/
		for(i=0, *c=getchar(); *c!='\n' && *c!= EOF; *c=getchar(), ++i)
		{
			if((*c >= '0' && *c <= '9') || (*c == '-') || (*c == '+'))
				num2[i] = *c;
			else
				error=1;
		}
		num2[i] = '\0';
	}

	if(!error && !exit)
	{
		if(*op == '/' || *op == '%')
		{
			/* num/0 now allowed */
			if(strcmp(num2, "empty")==0)
			{
				if(strcmp(num1, "0")==0)
					error=1;
			} 
			else if(strcmp(num2, "0")==0) 
				error=1;
		}

		if(*op == '^')
		{
			if(strcmp(num1, "0")==0 && strcmp(num2, "0")==0) /* 0^0 not allowed */
				error=1;	
		}
	}
	
	if(exit)
		error=-1; /*Return value is -1 when op is x or X*/
	
	return error;
}

int take_grades(int grades[])
{
	int scan=0, error=0;

	for(int i=0; i<HWS_LABS; ++i)
		scan += scanf("%d", &grades[i]);

	if(scan != HWS_LABS) /*Is array valid*/
		error = 1;

	return error;
}

int take_exam_grades(int exams[])
{
	int scan, error=0;

	printf("Enter midterm and final grades respectively...\n");
	scan = scanf("%d %d", &exams[0], &exams[1]);

	if(scan != 2) /*Is array valid*/
		error = 1;

	return error;
}

double calculate_homework(int hws[])
{
	double mean=0;

	for(int i=0; i<HWS_LABS; ++i)
		mean += hws[i];

	mean /= HWS_LABS;

	return mean;
}

double calculate_lab(int labs[])
{
	double mean=0;

	for(int i=0; i<HWS_LABS; ++i)
		mean += labs[i];

	mean /= HWS_LABS;

	return mean;
}

/*Calculates and returns weighted average of all grades*/
double calculate_all(int mean_lab, int grades[], int mean_hw)
{
	return mean_hw*0.1 + mean_lab*0.2 + grades[0]*0.3 + grades[1]*0.4;
}

/*Draws the shape according to the x value for part3*/
void draw_shape(int x)
{
	int i, j, stars, space=x-1;

	/*Top of the shape*/
	for(i=0; i<x; ++i, --space) /*Rows*/
	{
		for(j=0; j<space; ++j) /*Spaces*/
			printf(" ");
		printf("/");
		stars = 2*i;
		for(j=0; j<stars; ++j) /*Stars*/
			printf("*");
		printf("\\");
		printf("\n");
	}
	++space;
	/*Bottom of the shape*/
	for(i=0; i<x; ++i, stars-=2, ++space) /*Rows*/
	{
		for(j=0; j<space; ++j) /*Spaces*/
			printf(" ");
		printf("\\");
		for(j=0; j<stars; ++j) /*Stars*/
			printf("*");
		printf("/\n");
	}
}
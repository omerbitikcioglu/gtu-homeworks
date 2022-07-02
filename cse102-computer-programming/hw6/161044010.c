/*
		Gebze Technical University
	Introduction to Computer Programming
				Homework-6

		Author:Ömer Faruk BİTİKÇİOĞLU
											*/

#include <stdio.h>
#include <ctype.h>

#define STR_SIZ 100

void menu(void);

/*Part1 functions*/
int common(int num1, int num2);
int check_prime(int num);

/*Part2 functions*/
void part2(void);
void mergeSort(int list[], int first, int last);
void merge(int list[], int first, int mid, int last);

/*Part3 functions*/
void part3(int input);

/*Part4 functions*/
int part4(int input, int n, int sum);
int power(int x, int n);
int num_of_digits(int input);

/*Part5 functions*/
char find_first_upper(char *str);

int main(void)
{
	menu();
	return 0;
}

void menu(void)
{
	int select=0, num1, num2, input, n;	
	char str[STR_SIZ], first_upper;

	while(select != 6)
	{
		printf("\t~ MENU ~\n");
		printf("\t1-Part1\n");
		printf("\t2-Part2\n");
		printf("\t3-Part3\n");
		printf("\t4-Part4\n");
		printf("\t5-Part5\n");
		printf("\t6-Exit\n");
		
		printf("Selection: ");
		scanf("%d", &select);
		printf("\n");

		switch(select)
		{
			case 1	:	/*Part1*/
				
				printf("Enter two numbers to find their gcd: ");
				scanf("%d%d", &num1, &num2);
				printf("Result = %d\n\n", common(num1, num2));
				break;
			case 2	:	/*Part2*/
				
				part2();
				break;
			case 3	:	/*Part3*/

				printf("Input: ");
				scanf("%d", &input);
				printf("Output: ");
				part3(input);
				break;
			case 4	:	/*Part4*/

				printf("Input: ");
				scanf("%d", &input);
				n = num_of_digits(input);
				if(part4(input, n, 0)==input)
					printf("Output: Equal\n\n");
				else
					printf("Output: Not Equal\n\n");
				break;
			case 5	:	/*Part5*/

				printf("Input: ");
				scanf("%s", str);
				first_upper = find_first_upper(str);
				if(first_upper!=EOF)
					printf("Output: %c\n\n", first_upper);
				else
					printf("There is no upper letter in this string!\n\n");
				break;
			case 6	:	/*Exit*/
				
				break;
			default :
				printf("Enter a valid choice!..\n\n");
		}
	}
	
}

/*Finds the greatest common divisor of the two number*/
int common(int num1, int num2)
{
	int mult=2;

	/*Finds a common multiplier*/
	while((num1%mult!=0 || num2%mult!=0) && num1>=mult && num2>=mult)
	{
		do{
			++mult;
		}while(!check_prime(mult));
	}

	if(num1<mult || num2<mult)
		return 1;
	else
		return mult*common(num1/mult, num2/mult);
}

/*Checks if a number is a prime number or not*/
int check_prime(int num)
{
	int is_prime=1, flag=0;

	for(int i=2; i<num/2 && !flag; ++i)
	{
		if(num%i==0)
		{
			flag=1;
			is_prime=0;
		}
	}

	return is_prime;
}

void part2(void)
{
	int length;

	/*Taking the length of the list from the user*/
	printf("Enter the length of the list: ");
	scanf("%d", &length);
	int list[length];
	
	/*Taking the list elements from the user*/
	printf("Enter the elements of list:\n");
	for(int i=0; i<length; ++i)
		scanf("%d", &list[i]);
	
	/*Go fuction to sort the array in increasing order*/
	mergeSort(list, 0, length-1);
	
	printf("Sorted array is: ");
	for(int i=0; i<length; ++i)
		printf("%d ", list[i]);
	printf("\n\n");
}

/*Sorts the array in increasing order*/
void mergeSort(int list[], int first, int last)
{
	int mid;

	if(first < last)
	{
		mid = (first + last) / 2;
		mergeSort(list, first, mid);
		mergeSort(list, mid+1, last);
		merge(list, first, mid, last);
	}
}

void merge(int list[], int first, int mid, int last)
{
	/*Array indices*/
	int i, j, k=first; 

	/*Sizes of the sub arrays*/
	int n1 = mid-first+1;
	int n2 = last-mid;

	/*Create left and right sub arrays*/
	int left_sub[n1];
	int right_sub[n2];

	/*Fill the sub arrays*/
	for(i=0; i<n1; ++i)
		left_sub[i]=list[first++];
	for(i=0; i<n2; ++i)
		right_sub[i]=list[1+mid++];

	/*Merge two sub arrays with increasing order*/
	i=0; j=0;
	while(i<n1 && j<n2) /*While still there are elements in sub1 and sub2*/
	{
		/*Smaller value precedes*/
		if(left_sub[i]<right_sub[j])
			list[k++]=left_sub[i++];
		else
			list[k++]=right_sub[j++];
	}

	/*Rest of the remaining sub array copied to merged array*/
	while(i<n1)
		list[k++]=left_sub[i++];
	while(j<n2)
		list[k++]=right_sub[j++];	
}

/*	Prints n/2 	if n%2 = 0,
	Prints 3n+1 if n%2 = 1... until reaches 1*/
void part3(int input)
{
	printf("%d ", input); /*Print current value*/

	if(input==1) /*End of the sequence*/
	{
		printf("\n\n");
		return;
	} 
		
	if(input%2 == 0)
		part3(input/2);
	else
		part3(3*input+1);
}

/*Finds and returns the sum for such input abc = a^3 + b^3 + c^3...*/ 
int part4(int input, int n, int sum)
{
	int digit = input%10; /*Take one digit at each time*/

	if(input==0)
		return sum;
	else
		return part4(input/10, n, sum+power(digit,n));
}

/*Finds and returns x^n*/
int power(int x, int n)
{
	int result=1;
	for(int i=0; i<n; ++i)
		result *= x;

	return result;
}

/*Finds and returns the number of digits of any given number*/
int num_of_digits(int input)
{
	int digits=0;

	if(input==0)
		digits=1;

	while(input!=0)
	{
		input/=10;
		digits++;
	}

	return digits;
}

/*Finds and returns the first upper letter in any given string,
	if there is no upper letter in the string, returns EOF.*/
char find_first_upper(char *str)
{
	if(isupper(*str))
		return *str;
	else if(*(str+1)!='\0')
		find_first_upper(str+1);
	else
		return EOF;
}

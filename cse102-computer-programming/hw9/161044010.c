/*
		Gebze Technical University
	Introduction to Computer Programming
				Homework-9

		Author:Ömer Faruk BİTİKÇİOĞLU
											*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STR_SIZ 15

struct person{
	char name[STR_SIZ];
	char surname[STR_SIZ];
	char musical_work[STR_SIZ];
	int age;
	struct person *next;
} *top = NULL;

void addNode(char name[], char surname[], char creation[], int age);
void deleteNode();
void pushStack(struct person **inpStack, struct person *new_node);
struct person* popStack(struct person **inpStack);
struct person* sort_alphabetically();
struct person* sort_increasingly();
void print_stack(struct person *inpStack);

int main(void)
{
	int choice=0, inp; // For menu
	char c;
	char tmp_name[STR_SIZ], tmp_surname[STR_SIZ], tmp_mwork[STR_SIZ];	/* Musician	*/ 
	int tmp_age;														/*	Infos	*/
	
	// MENU
	while(choice != 5) // Shows menu until user selects exit
	{
		printf("****MENU****\n");
		printf(" 1- Add a Person to the Stack\n");
		printf(" 2- Pop a Person from the Stack\n");
		printf(" 3- Sort Alphabetical Order\n");
		printf(" 4- Sort Age in Increasing Order\n");
		printf(" 5- Exit menu\n");
		printf("*************\n");
		
		printf("Select your Choice: ");
		inp = scanf("%d", &choice);
		if(inp)
		{
			switch(choice)
			{
				case 1: // 1- Add a Person to the Stack
					
					// Take inputs from the user
					printf("Name: ");
					scanf(" %15[^\n]s", tmp_name);
					printf("Surname: ");
					scanf(" %15[^\n]s", tmp_surname);
					printf("Creation: ");
					scanf(" %15[^\n]s", tmp_mwork);
					printf("Age: ");
					scanf(" %d", &tmp_age);

					// Adds those infos to the stack
					addNode(tmp_name, tmp_surname, tmp_mwork, tmp_age); 
					break;
				case 2:
					// 2- Pop a person from the Stack
					deleteNode();
					break;
				case 3:
					// 3- Sort Alphabetical Order
					top = sort_alphabetically();
					print_stack(top);
					break;
				case 4:
					// 4- Sort Age in Increasing Order
					top = sort_increasingly();
					print_stack(top);
					break;
				case 5:
					// 5- Exit menu
					break;
				default: 
					printf("Invalid choice!\n");
			}	
		}
		else // Garbage input
		{
			printf("Invalid choice!\n");
			do{c=getchar();}while(c!='\n'); // Ignore garbage input
		}	
	}

	// Free allocated memory
	struct person *temp;
	while(top)
	{
		temp = top;
		top = top -> next;
		free(temp);
	}
	
	return 0;
}

void addNode(char name[], char surname[], char creation[], int age)
{
	// Create a temp node
	struct person *temp = (struct person*)malloc(sizeof(struct person));
	strcpy(temp->name, name);
	strcpy(temp->surname, surname);
	strcpy(temp->musical_work, creation);
	temp->age = age;

	if(!top) // If the stack is empty
	{
		top = temp; // First node is the top node
		top -> next = NULL;
	}
	else
	{
		temp -> next = top; // New node added in front of the top node
		top = temp; // Top node is the new node now
	}
	print_stack(top);
}

void deleteNode()
{
	struct person *temp = NULL;

	if(!top) // If the stack is empty
		printf("Stack is empty!\n");
	else // If the stack has an element/ elements
	{
		temp = top;
		top = top->next; // Top element is the next element now
		free(temp);
	}
	print_stack(top);
}

// Adds a given node to the stack
void pushStack(struct person **inpStack, struct person *new_node)
{
	struct person *temp = *inpStack;
	*inpStack = new_node;
	(*inpStack) -> next = temp;	
}

// Pops top node of the stack and returns it
struct person* popStack(struct person **inpStack)
{
	struct person *temp = NULL;
	if(inpStack) // If the stack has an element/ elements
	{
		temp = *inpStack;
		*inpStack = (*inpStack)->next;	
	}
	else
		printf("Stack is empty!\n");
	return temp;
}

struct person* sort_alphabetically()
{
	struct person *temp1, *temp2;
	struct person *sorted_top = NULL;

	while(top) // Sort until stack has no element left
	{
		temp1 = popStack(&top); // Pops one element from the main stack
		while(sorted_top && (strcmp(sorted_top->name, temp1->name) < 0)) // Deletes preceding elements from the sorted stack
		{
			temp2 = popStack(&sorted_top); // Pops the preceding element from the sorted stack
			pushStack(&top, temp2); // Pushes back the preceding element to the main stack
		}
		pushStack(&sorted_top, temp1);	// Pushes the current element to the sorted stack
										// Because no preceding element left, current element is
										// the subsequent element of the sorted stack
	}
	return sorted_top;
}

struct person* sort_increasingly()
{
	struct person *temp1, *temp2;
	struct person *sorted_top = NULL;

	while(top) // Sort until stack has no element left
	{
		temp1 = popStack(&top); // Pops one element from the main stack
		while(sorted_top && sorted_top->age < temp1->age) // Deletes preceding elements from the sorted stack
		{
			temp2 = popStack(&sorted_top); // Pops the preceding element from the sorted stack
			pushStack(&top, temp2); // Pushes back the preceding element to the main stack
		}
		pushStack(&sorted_top, temp1); 	// Pushes the current element to the sorted stack
										// Because no preceding element left, current element is
										// the subsequent element of the sorted stack
	}
	return sorted_top;
}

void print_stack(struct person *inpStack)
{
	struct person *current = inpStack;

	printf("-----------------\n");
	for(int i=1; current; ++i) // Traverses until the end of the stack 
	{
		printf("%d)%s\n", i, current->name);
		printf("%s\n", current->surname);
		printf("%s\n", current->musical_work);
		printf("%d\n", current->age);
		current = current->next; // Get the next node of the stack
	}
}
/*
		Gebze Technical University
	Introduction to Computer Programming
				Homework-10

		Author:Ömer Faruk BİTİKÇİOĞLU
											*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define ARR_SIZ 20

typedef struct bst_s
{
	int value;
	struct bst_s *leftp;
	struct bst_s *rightp;
} bst;

typedef struct queue_node_s
{
	int value;
	struct queue_node_s *nextp;
} queue_node_t;

typedef struct
{
	queue_node_t 	*headp, 
					*rearp;
	int size;
} queue;

typedef struct stack_node_s
{
	int value;
	struct stack_node_s *nextp;
} stack_node_t;

typedef struct
{
	stack_node_t *headp;
	int size;
} stack;

void fill_structures(stack **stack_, queue **queue_, bst **bst_, int data[ARR_SIZ]);
void ordered_print(stack *stack_, queue *queue_, bst *bst_);
void sort_stack(stack *stack_);
void sort_queue(queue *queue_);
void print_bst_inorder(bst *bst_);
void search(stack *stack_, queue *queue_, bst *bst_, int val_to_search);
void special_traverse(stack *stack_, queue *queue_, bst *bst_);

int main()
{
	int data[ARR_SIZ]={5, 2, 7, 8, 11, 3, 21, 7, 6, 15, 19, 35, 24, 1, 8, 12, 17, 8, 23, 4};
	bst *bst_;
	queue *queue_;
	stack *stack_;
	fill_structures(&stack_, &queue_, &bst_, data);
	ordered_print(stack_, queue_, bst_);
	search(stack_, queue_, bst_, 5);
	special_traverse(stack_, queue_, bst_);
	return 0;
}

// Fills the data structures with data array
void fill_structures(stack **stack_, queue **queue_, bst **bst_, int data[ARR_SIZ])
{
	int i;
	float	start_t_s, end_t_s,
			start_t_q, end_t_q,
			start_t_b, end_t_b;
	printf("Structures are initilizing...\n");
	// Fills the stack
	start_t_s = (float)clock();
	stack_node_t *temp_node;
	stack *temp_s = (stack*)malloc(sizeof(stack));
	temp_s->headp = NULL;
	temp_s->size = 0;
	for(i=0; i<ARR_SIZ; ++i)
	{
		temp_node = (stack_node_t*)malloc(sizeof(stack_node_t));
		temp_node -> value = data[i];
		if(temp_s->headp) // If stack has an element
			temp_node->nextp = temp_s->headp;
		else
			temp_node->nextp = NULL;
		temp_s->headp = temp_node;
		temp_s->size++;
	}
	*stack_ = temp_s;
	end_t_s = (float)clock();

	// Fills the queue
	start_t_q = (float)clock();
	queue_node_t *temp_q_node;
	queue *temp_q = (queue*)malloc(sizeof(queue));
	for(i=0; i<ARR_SIZ; ++i)
	{
		temp_q_node = (queue_node_t*)malloc(sizeof(queue_node_t));
		temp_q_node -> value = data[i];
		temp_q_node -> nextp = NULL;
		if(i==0) // First entry
		{
			temp_q -> headp = temp_q_node;
			temp_q -> rearp = temp_q_node;
			temp_q -> size++;	
		}
		else
		{
			temp_q -> rearp -> nextp = temp_q_node; // Add new node to the tail
			temp_q -> rearp = temp_q_node;
			temp_q -> size++;	
		}
	}
	*queue_ = temp_q;
	end_t_q = (float)clock();

	// Fills the binary search tree
	start_t_b = (float)clock();
	bst *temp_bst, *temp_root;
	int error, found;
	for(i=0; i<ARR_SIZ; ++i)
	{
		if(i==0) // First entry
		{
			temp_bst = (bst*)malloc(sizeof(bst));
			temp_root = temp_bst;
			temp_bst -> value = data[i];
			temp_bst -> leftp = NULL;
			temp_bst -> rightp = NULL;
		}
		else
		{
			// Finds the proper leaf to insert new value
			temp_bst = temp_root;
			found=0; error=0;
			while(!found && !error) 
			{
				if(data[i] == temp_bst->value) 
				{
					// No insertion, duplicate value
					error = 1;
				}
				else if(data[i] < temp_bst->value)
				{
					if(temp_bst -> leftp)
						temp_bst = temp_bst -> leftp;
					else
						found = 1;
				}
				else
				{
					if(temp_bst -> rightp)
						temp_bst = temp_bst -> rightp;
					else
						found = 1;
				}
			}

			if(found)
			{
				if(data[i] < temp_bst->value) // Insert in the left leaf
				{
					temp_bst->leftp = (bst*)malloc(sizeof(bst));
					temp_bst = temp_bst->leftp;
				}
				else // Insert in the right leaf
				{
					temp_bst->rightp = (bst*)malloc(sizeof(bst));
					temp_bst = temp_bst->rightp;
				}	
				temp_bst -> value = data[i];
				temp_bst -> leftp = NULL;
				temp_bst -> rightp = NULL;		
			}
		}
	}
	*bst_ = temp_root;
	end_t_b = (float)clock();

	printf("Structures\tStack\t\tQueue\t\tBST\n");
	printf("Exec. Time\t%.3fms\t\t%.3fms\t\t%.3fms\n\n"	,(end_t_s-start_t_s)/CLOCKS_PER_SEC*1000
									 					,(end_t_q-start_t_q)/CLOCKS_PER_SEC*1000
									 					,(end_t_b-start_t_b)/CLOCKS_PER_SEC*1000);
}

// Sorts and prints the data structures in descending order
void ordered_print(stack *stack_, queue *queue_, bst *bst_)
{
	float	start_t_s, end_t_s,
			start_t_q, end_t_q,
			start_t_b, end_t_b;
	
	printf("Structures are ordering and printing...\n");
		
			// STACK
	start_t_s = clock();
	// Sorts the stack
	sort_stack(stack_);
	//Prints the sorted stack
	printf("Stack: ");
	stack_node_t *current_s;
	current_s = stack_->headp;
	while(current_s)
	{
		printf("%d ", current_s->value);
		current_s = current_s->nextp;
	}
	printf("\n");
	end_t_s = clock();

			// QUEUE
	start_t_q = clock();
	// Sorts the queue
	sort_queue(queue_);
	//Prints the sorted queue
	printf("Queue: ");
	queue_node_t *current_q;
	current_q = queue_->headp;
	while(current_q)
	{
		printf("%d ", current_q->value);
		current_q = current_q->nextp;
	}
	printf("\n");
	end_t_q = clock();

			// BST
	start_t_b = clock();
	printf("BST: ");
	print_bst_inorder(bst_);
	printf("\n");
	end_t_b = clock();

	printf("Structures\tStack\t\tQueue\t\tBST\n");
	printf("Exec. Time\t%.3fms\t\t%.3fms\t\t%.3fms\n\n"	,(end_t_s-start_t_s)/CLOCKS_PER_SEC*1000
									 					,(end_t_q-start_t_q)/CLOCKS_PER_SEC*1000
														,(end_t_b-start_t_b)/CLOCKS_PER_SEC*1000);
}

// Sorts the stack in descending order
void sort_stack(stack *stack_)
{
	int i,j;
	stack_node_t *current_s;
	stack_node_t *temp_s, *tocompare_s, *prev_s;
	for(i=1; i < stack_->size; ++i)
	{
		current_s = stack_->headp;
		tocompare_s = current_s->nextp;
		for(j=0; j < stack_->size-i; ++j)
		{
			if(current_s->value < tocompare_s->value)
			{
				// Swap
				temp_s = tocompare_s->nextp;
				tocompare_s->nextp = current_s;
				current_s->nextp = temp_s;

				if(j==0)
					stack_->headp = tocompare_s; // Change head
				else
					prev_s->nextp = tocompare_s;

				prev_s = tocompare_s;
				tocompare_s = current_s->nextp;
			}
			else
			{
				prev_s = current_s;
				current_s = current_s->nextp;
				tocompare_s = tocompare_s->nextp;
			}
		}
	}
}

// Sorts the queue in descending order
void sort_queue(queue *queue_)
{
	int i,j;
	queue_node_t *current_q;
	queue_node_t *temp_q, *tocompare_q, *prev_q;
	for(i=1; i < queue_->size; ++i)
	{
		current_q = queue_->headp;
		tocompare_q = current_q->nextp;
		for(j=0; j < queue_->size-i; ++j)
		{
			if(current_q->value < tocompare_q->value)
			{
				// Swap
				temp_q = tocompare_q->nextp;
				tocompare_q->nextp = current_q;
				current_q->nextp = temp_q;

				if(j==0)
					queue_->headp = tocompare_q; // Change head
				else
					prev_q->nextp = tocompare_q;

				prev_q = tocompare_q;
				tocompare_q = current_q->nextp;
			}
			else
			{
				prev_q = current_q;
				current_q = current_q->nextp;
				tocompare_q = tocompare_q->nextp;
			}
		}
	}
}

// Prints bst in descending order
void print_bst_inorder(bst *bst_)
{
	if(bst_->rightp)
		print_bst_inorder(bst_->rightp);
	printf("%d ", bst_->value);	
	if(bst_->leftp)
		print_bst_inorder(bst_->leftp);
}

// Searches for the desired value in the data structures
void search(stack *stack_, queue *queue_, bst *bst_, int val_to_search)
{
	int index;
	float	start_t_s, end_t_s,
			start_t_q, end_t_q,
			start_t_b, end_t_b;
	printf("Searching the value %d in data structures...\n", val_to_search);
		// STACK
	start_t_s = clock();
	printf("In Stack:\n");
	stack_node_t *current_s;
	for(index=1, current_s=stack_->headp; current_s; ++index)
	{
		if(current_s->value == val_to_search)
			printf("Founded on %d.step\n", index);
		current_s=current_s->nextp;
	}
	end_t_s = clock();

		// QUEUE
	start_t_q = clock();
	printf("In Queue:\n");
	queue_node_t *current_q;
	for(index=1, current_q=queue_->headp; current_q; ++index)
	{
		if(current_q->value == val_to_search)
			printf("Founded on %d.step\n", index);
		current_q=current_q->nextp;
	}
	end_t_q = clock();

		// BST
	start_t_b = clock();
	printf("In BST:\n");
	bst *current_b;
	for(index=1, current_b=bst_; current_b; ++index)
	{
		if(current_b->value == val_to_search)
		{
			printf("Founded on %d.step\n", index);
			current_b = NULL;
		}
		else if(val_to_search > current_b->value)
			current_b = current_b->rightp; 	
		else
			current_b = current_b->leftp;
	}
	end_t_b = clock();

	printf("Structures\tStack\t\tQueue\t\tBST\n");
	printf("Exec. Time\t%.3fms\t\t%.3fms\t\t%.3fms\n\n"	,(end_t_s-start_t_s)/CLOCKS_PER_SEC*1000
									 					,(end_t_q-start_t_q)/CLOCKS_PER_SEC*1000
														,(end_t_b-start_t_b)/CLOCKS_PER_SEC*1000);
}

// Traverses the data structures and prints one max and one min value consecutively
void special_traverse(stack *stack_, queue *queue_, bst *bst_)
{
	int i, j, flag=0;
	float	start_t_s, end_t_s,
			start_t_q, end_t_q,
			start_t_b, end_t_b;
	printf("Structures are traversed in a special way...\n");
			// STACK
	start_t_s = clock();
	printf("Stack: ");
	stack_node_t *max_s = stack_->headp; // Max value is the first node
	stack_node_t *min_s;
	for(i=0; i < stack_->size; i+=2) 
	{
		for(j=0, min_s=max_s; j < stack_->size-i-1; ++j) // Finds min value
		{
			min_s = min_s->nextp;
			flag=1;
		}
		printf("%d ", max_s->value);
		if(flag)
			printf("%d ", min_s->value); // If min exists then print it
		flag=0;
		max_s = max_s->nextp;
	}
	printf("\n");
	end_t_s = clock();

			// QUEUE
	start_t_q = clock();
	printf("Queue: ");
	queue_node_t *max_q = queue_->headp; // Max value is the first node
	queue_node_t *min_q;
	for(i=0; i < queue_->size; i+=2) 
	{
		for(j=0, min_q=max_q; j < queue_->size-i-1; ++j) // Finds min value
		{
			min_q = min_q->nextp;
			flag=1;
		}
		printf("%d ", max_q->value);
		if(flag)
			printf("%d ", min_q->value); // If min exists then print it
		flag=0;
		max_q = max_q->nextp;
	}
	printf("\n");
	end_t_q = clock();

			// BST
	start_t_b = clock();
	printf("BST: ");
	bst *max_b, *min_b, *prev_max, *prev_min;
	bst *root = bst_;
	while(root->rightp || root->leftp)
	{
		if(root->rightp)
		{
			for(max_b=root->rightp, prev_max=root; max_b->rightp; max_b=max_b->rightp) // Finds max value
				prev_max = max_b;
			printf("%d ", max_b->value); // Print the max value
			
			// Removes printed node from the tree
			if(max_b->leftp)
				prev_max->rightp = max_b->leftp;
			else
				prev_max->rightp = NULL;
		}
		else
		{
			printf("%d ", root->value); // If there is no right node then root has the max value
			if(root->leftp)
				root = root->leftp; // Change root
			else
				flag=1; // Root has no child and already printed
		}

		if(root->leftp)
		{
			for(min_b=root->leftp, prev_min=root; min_b->leftp; min_b=min_b->leftp) // Finds min value
				prev_min = min_b;
			printf("%d ", min_b->value); // Print the min value

			// Removes printed node from the tree
			if(min_b->rightp)
				prev_min->leftp = min_b->rightp;
			else
				prev_min->leftp = NULL;
		}
		else
		{
			printf("%d ", root->value); // If there is no left node then root has the min value
			if(root->rightp)
				root = root->rightp; // Change root
			else
				flag=1; // Root has no child and already printed
		}
	}
	if(!flag) // If root is not printed yet
		printf("%d\n", root->value); // Print the last existing node
	else
		printf("\n");
	end_t_b = clock();

	printf("Structures\tStack\t\tQueue\t\tBST\n");
	printf("Exec. Time\t%.3fms\t\t%.3fms\t\t%.3fms\n\n"	,(end_t_s-start_t_s)/CLOCKS_PER_SEC*1000
									 					,(end_t_q-start_t_q)/CLOCKS_PER_SEC*1000
														,(end_t_b-start_t_b)/CLOCKS_PER_SEC*1000);
}
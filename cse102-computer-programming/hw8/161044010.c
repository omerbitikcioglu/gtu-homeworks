/*
		Gebze Technical University
	Introduction to Computer Programming
				Homework-8

		Author:Ömer Faruk BİTİKÇİOĞLU
											*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#define NAM_SIZ 15
#define BLK_NUM 24
#define BLK_WID 22
#define CRD_NUM 5

typedef enum {
	noone, cap, car
} player_type;

typedef enum {
	start, property, tax, punish, fortune
} block_type;

typedef enum {
	human = 1, computer = 2
} turn_type;

typedef enum {
	free_house, time_travel, garnishment, generosity, treasure
} card_type;

typedef struct player_s {
	player_type type;
	int current_block_id;
	int owned_block_ids[12];
	int account;
	int turn_to_wait;
	char *name;
} player_t;

typedef struct block_s {
	int block_id;
	char *name;
	int price;
	int rent, rent_1, rent_2, rent_3;
	int house_price;
	int house_count;
	player_t owner;
	block_type type;
	struct block_s *next;
	struct block_s *prev;
} block_t;

typedef struct fortune_card_s {
	int card_no;
	char *name;
	card_type type;
} fortune_card_t;

char block_names[BLK_NUM][NAM_SIZ]= {"Start", "Esenyurt", "Car Park", "Fortune Card", "Tuzla", "Arnavutkoy", 
"Wait 2 Turn", "Catalca", "Beykoz", "Fortune Card", "Car Fix", "Maltepe", "Bills", "Sisli", "Oil", "Fortune Card", 
"Atasehir", "Sariyer", "Wait 1 Turn", "Kadikoy", "Besiktas", "Fortune Card", "Vocation", "Bebek"};

void init_the_board(block_t **head);
void init_the_players(player_t* player, player_t* comp);
void init_the_cards(fortune_card_t cards[CRD_NUM]);
void show_board(block_t *block, player_t player, player_t comp);
void print_dashes(int num_of_dashes);
void show_rent_or_tax(block_t *block);
void show_players_on_board(player_t player, player_t comp, int block_id);
void show_properties(block_t *block);
void buy_property(block_t* current_block, player_t* current_player);
void buy_houses(block_t* current_block, player_t* current_player);
void sell_property(block_t *block, player_t* current_player);
double avg_of_props(block_t* head);
void gameplay (block_t *block, player_t player, player_t comp);
int check_properties(player_t player);
int check_empty_props(block_t *head, player_t player);
void show_menu(void);

int main(void)
{
	block_t *head = NULL;
	player_t player;
	player_t comp;

	srand(time(NULL));

	printf("\n\tM O N O P O L Y - 2\n\n");
	
	init_the_board(&head);
	init_the_players(&player, &comp);
	show_board(head, player, comp);
	gameplay(head, player, comp);

	return 0;
}

void init_the_board(block_t **head)
{
	int i = 0;	
	block_t *temp, *headtemp, *prevtemp;

	temp = (block_t*)malloc(sizeof(block_t));
	headtemp = temp;
	
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 0;
	temp -> rent = 0;
	temp -> rent_1 = 0;
	temp -> rent_2 = 0;
	temp -> rent_3 = 0;
	temp -> house_price = 0;
	temp -> house_count = 0;
	temp -> owner.type = noone; 
	temp -> type = start;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 16000;
	temp -> rent = 800;
	temp -> rent_1 = 4000;
	temp -> rent_2 = 9000;
	temp -> rent_3 = 25000;
	temp -> house_price = 10000;
	temp -> house_count = 0;
	temp -> owner.type = noone; 
	temp -> type = property;
	prevtemp = temp;
	
	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 0;
	temp -> rent = 1500;
	temp -> rent_1 = 0;
	temp -> rent_2 = 0;
	temp -> rent_3 = 0;
	temp -> house_price = 0;
	temp -> house_count = 0;
	temp -> owner.type = noone; 
	temp -> type = tax;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 0;
	temp -> rent = 0;
	temp -> rent_1 = 0;
	temp -> rent_2 = 0;
	temp -> rent_3 = 0;
	temp -> house_price = 0;
	temp -> house_count = 0;
	temp -> owner.type = noone; 
	temp -> type = fortune;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 16500;
	temp -> rent = 850;
	temp -> rent_1 = 4250;
	temp -> rent_2 = 9500;
	temp -> rent_3 = 26000;
	temp -> house_price = 12000;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = property;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 17000;
	temp -> rent = 875;
	temp -> rent_1 = 4500;
	temp -> rent_2 = 10000;
	temp -> rent_3 = 28000;
	temp -> house_price = 12000;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = property;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 0;
	temp -> rent = 2;
	temp -> rent_1 = 0;
	temp -> rent_2 = 0;
	temp -> rent_3 = 0;
	temp -> house_price = 0;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = punish;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 20000;
	temp -> rent = 1000;
	temp -> rent_1 = 5000;
	temp -> rent_2 = 12000;
	temp -> rent_3 = 30000;
	temp -> house_price = 13000;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = property;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 23000;
	temp -> rent = 1100;
	temp -> rent_1 = 5500;
	temp -> rent_2 = 12500;
	temp -> rent_3 = 33000;
	temp -> house_price = 13000;
	temp -> house_count = 0;
	temp -> owner.type = noone; 
	temp -> type = property;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 0;
	temp -> rent = 0;
	temp -> rent_1 = 0;
	temp -> rent_2 = 0;
	temp -> rent_3 = 0;
	temp -> house_price = 0;
	temp -> house_count = 0;
	temp -> owner.type = noone; 
	temp -> type = fortune;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 0;
	temp -> rent = 1750;
	temp -> rent_1 = 0;
	temp -> rent_2 = 0;
	temp -> rent_3 = 0;
	temp -> house_price = 0;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = tax;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 30000;
	temp -> rent = 1350;
	temp -> rent_1 = 7000;
	temp -> rent_2 = 15000;
	temp -> rent_3 = 40000;
	temp -> house_price = 15000;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = property;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 0;
	temp -> rent = 2000;
	temp -> rent_1 = 0;
	temp -> rent_2 = 0;
	temp -> rent_3 = 0;
	temp -> house_price = 0;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = tax;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 33000;
	temp -> rent = 1500;
	temp -> rent_1 = 8000;
	temp -> rent_2 = 16000;
	temp -> rent_3 = 42000;
	temp -> house_price = 16000;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = property;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 0;
	temp -> rent = 2250;
	temp -> rent_1 = 0;
	temp -> rent_2 = 0;
	temp -> rent_3 = 0;
	temp -> house_price = 0;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = tax;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 0;
	temp -> rent = 0;
	temp -> rent_1 = 0;
	temp -> rent_2 = 0;
	temp -> rent_3 = 0;
	temp -> house_price = 0;
	temp -> house_count = 0;
	temp -> owner.type = noone; 
	temp -> type = fortune;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 35000;
	temp -> rent = 1600;
	temp -> rent_1 = 8500;
	temp -> rent_2 = 17000;
	temp -> rent_3 = 45000;
	temp -> house_price = 17000;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = property;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 40000;
	temp -> rent = 1750;
	temp -> rent_1 = 9500;
	temp -> rent_2 = 19000;
	temp -> rent_3 = 48000;
	temp -> house_price = 19000;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = property;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 0;
	temp -> rent = 1;
	temp -> rent_1 = 0;
	temp -> rent_2 = 0;
	temp -> rent_3 = 0;
	temp -> house_price = 0;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = punish;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 43000;
	temp -> rent = 1900;
	temp -> rent_1 = 11000;
	temp -> rent_2 = 21500;
	temp -> rent_3 = 55000;
	temp -> house_price = 23000;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = property;	
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 60000;
	temp -> rent = 2500;
	temp -> rent_1 = 15000;
	temp -> rent_2 = 28000;
	temp -> rent_3 = 60000;
	temp -> house_price = 30000;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = property;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 0;
	temp -> rent = 0;
	temp -> rent_1 = 0;
	temp -> rent_2 = 0;
	temp -> rent_3 = 0;
	temp -> house_price = 0;
	temp -> house_count = 0;
	temp -> owner.type = noone; 
	temp -> type = fortune;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 0;
	temp -> rent = 5000;
	temp -> rent_1 = 0;
	temp -> rent_2 = 0;
	temp -> rent_3 = 0;
	temp -> house_price = 0;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = tax;
	prevtemp = temp;

	temp -> next = (block_t*)malloc(sizeof(block_t));
	temp = temp -> next;
	temp -> prev = prevtemp;
	temp -> block_id = i;
	temp -> name = block_names[i++];
	temp -> price = 70000;
	temp -> rent = 3500;
	temp -> rent_1 = 20000;
	temp -> rent_2 = 35500;
	temp -> rent_3 = 65000;
	temp -> house_price = 35000;
	temp -> house_count = 0;
	temp -> owner.type = noone;  
	temp -> type = property;
	
	temp -> next = headtemp; // Circular list
	*head = headtemp;
}

void init_the_players(player_t* player, player_t* comp)
{
	char c;
	int pick=0, i;

	//Get player's specifications
	player->type = car;
	player->name = "Car";
	printf("Player has Car\n");
	player->current_block_id = 0;
	for(i=0; i<12; ++i)
		player->owned_block_ids[i]=0;
	player->account = 100000;
	player->turn_to_wait = 1;

	//Get computer's specifications		
	comp->type = cap;
	comp->name = "Cap";
	printf("Computer has Cap\n");
	comp->current_block_id = 0;
	for(i=0; i<12; ++i)
		comp->owned_block_ids[i]=0;
	comp->account = 100000;
	comp->turn_to_wait = 1;	
}

void init_the_cards(fortune_card_t cards[CRD_NUM])
{
	for(int i=0; i<CRD_NUM; ++i)
	{
		switch(i+1)
		{
			case 1: //Free House
				cards[i].card_no=i+1;
				cards[i].name = "Free House";
				cards[i].type = free_house;
				break;
			case 2: //Time Travel
				cards[i].card_no=i+1;
				cards[i].name = "Time Travel";
				cards[i].type = time_travel;
				break;
			case 3:	//Garnishment
				cards[i].card_no=i+1;
				cards[i].name = "Garnishment";
				cards[i].type = garnishment;
				break;
			case 4:	//Generosity
				cards[i].card_no=i+1;
				cards[i].name = "Generosity";
				cards[i].type = generosity;
				break;
			case 5:	//Treasure Hunter
				cards[i].card_no=i+1;
				cards[i].name = "Treasure Hunter";
				cards[i].type = treasure;
				break;
		}
	}
}

void show_board(block_t *block, player_t player, player_t comp)
{ 
	int i, j, k, t; // Loop indices 
	int space, count, side_len = (BLK_NUM/4+1); 
	block_t *head, *head2;
	block_t *current, *current2;

	printf("\n"); print_dashes(BLK_WID*(side_len)+1); printf("\n");		// Dashes
	// Prints top of the board
	head = block;
	for(i=0; i<3; ++i) // Three row per block
	{
		current = head;
		printf("|");
		for(j=0; j<side_len; ++j) // First line of blocks
		{
			if(i==0) // First row : name of the block
			{
				space = (BLK_WID/2+4);
				printf("%*s", space, current->name); printf("%*s", BLK_WID-space, "|");
				current = current->next;
			}
			else if(i==1) // Second row : price of the property or rent of the tax
			{
				show_rent_or_tax(current);
				current = current->next;
			}
			else // Third row : Shows players if they are on the current block
			{
				show_players_on_board(player, comp, j);
				current = current->next;
			}
		}
		printf("\n");
	}
	print_dashes(BLK_WID*(side_len)+1); printf("\n"); 					// Dashes
	
	// Finds the last block of the table
	current2 = current; // To save time a bit
	for(i=0; i<3*side_len-5; ++i)
		current2 = current2->next;
	head2 = current2;
	head = current;	
	// Prints middle of the board
	for(j=BLK_NUM-1, k=side_len, count=0; j>BLK_NUM-side_len+1 && k<2*(side_len-1); --j, ++k)
	{
		printf("|");
		for(i=0; i<3; ++i) //Three row per block
		{
			if(i==0) //First row : name of the block
			{
				space = (BLK_WID/2+4);
				printf("%*s", space, current2->name);
				printf("%*s", BLK_WID-space, "|");
				
				for(t=0; t<(side_len-2)*BLK_WID-1; ++t)
					printf(" ");
				
				printf("|"); 
				printf("%*s", space, current->name);
				printf("%*s\n", BLK_WID-space, "|");
			}
			else if(i==1) //Second row : price of the property or rent of the tax
			{
				printf("|");
				show_rent_or_tax(current2);
				
				for(t=0; t<(side_len-2)*BLK_WID-1; ++t)
					printf(" ");	

				printf("|");
				show_rent_or_tax(current);
				printf("\n");
			}
			else //Third row : Shows players if they are on the block
			{
				printf("|");
				show_players_on_board(player, comp, j);

				for(t=0; t<(side_len-2)*BLK_WID-1; ++t)
					printf(" ");

				printf("|");
				show_players_on_board(player, comp, k);
				printf("\n");
			}
		}
		count++;
		if(count != (side_len-2)) // We did not reach the bottom side yet
		{
			print_dashes(BLK_WID+1);				// Dashes
			for(t=0; t<(side_len-2)*BLK_WID-1; ++t)
				printf(" ");	
			print_dashes(BLK_WID+1); printf("\n");	// Dashes
		}
		current = current -> next;
		current2 = current2 -> prev;
	}
	
	// Finds the head of the last side of blocks
	for(i=0; i<side_len-1; ++i)
		current = current->next;
	head = current;
	print_dashes(BLK_WID*(side_len)+1); printf("\n");		// Dashes
	// Prints bottom of the board
	for(i=0; i<3; ++i) // Three row per block
	{
		printf("|");
		for(j=BLK_NUM-side_len+1; j>=2*(side_len-1); --j) // Last side of blocks
		{
			if(i==0) // First row : name of the block
			{
				space = (BLK_WID/2+4);
				printf("%*s", space, current->name);
				printf("%*s", BLK_WID-space, "|");
				current = current -> prev;
			}
			else if(i==1) // Second row : price of the property or rent of the tax
			{
				show_rent_or_tax(current);
				current = current -> prev;
			}
			else // Third row : Shows players if they are on the block
			{
				show_players_on_board(player, comp, j);
				current = current -> prev;
			}
		}
		printf("\n");
		current = head;
	}
	print_dashes(BLK_WID*(side_len)+1); printf("\n\n");		// Dashes
}

void print_dashes(int num_of_dashes)
{
	for(int i=0; i<num_of_dashes; ++i)
		printf("-");
}

//Shows the rent of the properties or taxes
void show_rent_or_tax(block_t *block)
{
	int space = (BLK_WID/2+4);
	
	if(block->type == property) //If it is a property
		printf("%*d$", space-1, block->price);
	else if(block->type == tax) //If it is a tax
		printf("%*d$", space-1, block->rent);
	else
	{
		for(int i=0; i<space; ++i)
			printf(" ");			
	}
	printf("%*s", BLK_WID-space, "|");	
}

//Prints the vehicle name of the player if he/she on the given block id  
void show_players_on_board(player_t player, player_t comp, int block_id)
{
	int space = (BLK_WID/2);

	if(player.current_block_id == block_id && comp.current_block_id == block_id) //If both player on the block
		printf("%*s, %s%*s", space, player.name, comp.name, BLK_WID-space-5, "|");
	else if(player.current_block_id == block_id) //If just the player on the block
		printf("%*s%*s", space, player.name, BLK_WID-space, "|");
	else if(comp.current_block_id == block_id) //If just computer on the block
		printf("%*s%*s", space, comp.name, BLK_WID-space, "|");
	else //No one on the block
	{
		for(int i=0; i<space; ++i)
			printf(" ");
		printf("%*s", BLK_WID-space, "|");
	}
}

void show_properties(block_t *block)
{
	int choice, exit=0, i;
	char c;
	block_t *head = block;
	block_t *current;

	while(!exit)
	{
		printf("\n\nPlease select a property to see details:\n");
		current = head;
		for(i=0; i<BLK_NUM; ++i)
		{
			if(current->type == property)
				printf("%d - %s\n", current->block_id, current->name);
			current = current->next;
		}
		printf("0 - Exit\n");
		scanf("%d", &choice);

		if(choice==0)
			exit = 1;
		else if(choice<BLK_NUM)
		{
			// Seek for the choice
			current = head->next; // Skip the 'Start' block
			for(i=1; i<BLK_NUM; ++i)
			{
				if(current->type == property && current->block_id == choice) // Choice found
					i = BLK_NUM; // To terminate
				else
					current = current->next;
			}
			
			if(current!=NULL)
			{
				printf("\n\n\n"); print_dashes(35); printf("\n"); 					// Dashes		
				printf("|%22s", current->name);	printf("%12s\n", "|");
				print_dashes(35); printf("\n"); 									// Dashes			
				printf("|%13s%15d$", "Rent", current->rent); printf("%5s\n", "|");
				printf("|%13s%15d$", "Rent 1 H", current->rent_1); printf("%5s\n", "|");
				printf("|%13s%15d$", "Rent 2 H", current->rent_2); printf("%5s\n", "|");
				printf("|%13s%15d$", "Rent 3 H", current->rent_3); printf("%5s\n", "|");
				print_dashes(35); printf("\n"); 									// Dashes
				printf("|%13s%15d$", "House Price", current->house_price); printf("%5s\n", "|");
				print_dashes(35); printf("\n\n\n");									// Dashes	
			}
			else
				printf("Your choice is not on the list!\n");
		}
		else
		{
			printf("\n\n\nInvalid choice!..\n\n\n");	
			do{c= getchar();}while(c!='\n');	
		}
	}
}

void buy_property(block_t* current_block, player_t* current_player)
{
	char c;
	int i, dice;

	if(	current_block->owner.type == noone 							&& 	//Property has no owner
		current_player->current_block_id == current_block->block_id &&	//Player is on the property
		current_player->account >= current_block->price 			)	//Player can afford the property
	{
		(current_player->account) -= (current_block->price); //Player pays the price to the bank
		(current_block->owner) = (*current_player); //Player owns the property

		for(i=0; i<12; ++i) // Searches through owned_block_ids array
		{
			if(current_player->owned_block_ids[i] == 0) // Find empty space in the array
			{
				current_player->owned_block_ids[i] = current_player->current_block_id; // Added to owned properties
				i=12; //To terminate
			}
		}
		if(current_player->type == cap) // If it is computer
			printf("Computer bought %s\n", current_block->name);

		if(current_player->account >= current_block->house_price) //If player has money to build houses
		{
			if(current_player->type == car) // If it is human
			{
				printf("Do you want to build a house/ houses on your property?\n");
				printf("In %s there is/are %d house/houses\n", current_block->name, current_block->house_count);
				printf("Price per house is %d$ for %s\n", current_block->house_price, current_block->name);
				printf("You have %d$ in your account\n", current_player->account);
				printf("Type (Y) or (N)\n");
				do{c= getchar();}while(c!='\n'); //To get rid of \n char from last printf
				scanf("%c", &c);
				if(c == 'y' || c == 'Y')
					buy_houses(current_block, current_player);
				else
					do{c= getchar();}while(c!='\n'); //Get rid of the rest				
			}
			else // If it is computer
			{
				int count=0;
				for(i=0; i<12; ++i) // Counts how many properties computer has
				{
					if(current_player->owned_block_ids[i]!=0)
						++count;
				}
				if(count >= 4) // If computer owns the 1/3 of all properties
				{
					do{dice = rand()%7;}while(dice==0); // Computer rolls the dice 1-6
					if(dice < 4) // If dice is between 1-3
						buy_houses(current_block, current_player);
					else
						printf("Computer skipped its turn!\n");
				}
				else
					printf("Computer skipped its turn!\n");
			}
		}
		else
			printf("%s has no money to build houses here!\n", current_player->name);	
	}
	else if(current_block->owner.type != noone) // If property has an owner
	{
		if(current_block->owner.type == current_player->type)
			printf("This is already your property.\n");
		else
			printf("This property belongs to the computer!\n");
	}
	else if(current_player->account < current_block->price)
		printf("You don't have enough money to buy this property\n");
	else 
		printf("You are not on the block\n");
}

void buy_houses(block_t* current_block, player_t* current_player)
{
	int inp, houses, temp, i, j;
	char c;

	if(current_block->type != fortune)
	{
		if(!strcmp(current_player->name, "Car")) // If it is human
		{
			printf("How many houses do you want to build?\n");
			inp = scanf("%d", &houses);

			if(	houses > 0 && houses <= 3 && // You can build [1-3] houses
				current_player->account >= houses*(current_block->house_price) && // Does player have enough money 
				current_block->house_count + houses <=3) // You can't build more than 3 houses on a property
			{
				temp = current_player -> account;
				current_player->account -= (houses*(current_block->house_price)); //Player pays the price of houses
				printf("You had %d$, after buying you have %d$\n", temp, current_player->account);
				current_block->house_count += houses; // House count increased
			}
			else if(houses > 3 || current_block->house_count + houses > 3)
				printf("You can only build up to 3 houses!\n");
			else if(current_player->account < houses*(current_block->house_price))
				printf("You don't have enough money for this!\n");	
			else if(inp != 1 || houses < 0)
			{
				printf("Invalid input!\n");
				do{c= getchar();}while(c!='\n');
			}		
		}
		else // If it is computer
		{
			temp = current_player -> account;
			current_player->account -= current_block->house_price; //Computer pays the price of houses
			printf("Computer had %d$, after buying a house it has %d$\n", temp, current_player->account);
			current_block->house_count += 1; // House count increased
		}	
	}
	else // If it's free house fortune card
	{
		if(current_player->type == car && check_empty_props(current_block, *current_player)) // If it is human and has empty props
		{
			block_t *current = current_block;
			int choice;

			for(i=0; i<12; ++i)
			{
				if(current_player->owned_block_ids[i] != 0)
				{
					while(current->block_id != current_player->owned_block_ids[i])
						current = current->next;
					printf("ID:%d Name:%s (%d H)\n", current->block_id, current->name, current->house_count);
				}
			}	
			printf("Type the id of the property you want to build a free house:\n");
			printf("(Warning: You can not build more than 3 houses on a property!)\n");
			inp = scanf("%d", &choice);

			if(inp != 1 || choice < 0 || choice >= BLK_NUM) // Invalid input
			{
				printf("Invalid choice. You lost your chance!\n");
				do{c= getchar();}while(c!='\n');
			}
			else
			{
				while(current->block_id != choice) // Find the property
					current = current -> next;
				
				if(	current->type == property &&
					current->owner.type == current_player->type &&
				  	current->house_count<3) {
					current->house_count += 1; // Build a free house 
				} 	
				else if(current->house_count == 3)
				{
					printf("There are already 3 houses in this property!\n");
					printf("You lost your chance!\n");
				}
				else
					printf("Invalid choice. You lost your chance!\n");
			}	
		}
		else if(current_player->type == cap && check_empty_props(current_block, *current_player))// If it is computer and has empty props
		{
			block_t *current = current_block;
			int error=0;

			// Pick a random property computer has
			do{i = rand()%12;}while(current_player->owned_block_ids[i] == 0);

			while(current->block_id != current_player->owned_block_ids[i]) // Find the property
				current = current -> next;	
			current->house_count += 1; // Build a free house 	
			printf("Computer built a free house on %s\n", current->name);	
		}
		else
		{
			printf("%s has no property or empty property to build houses.\n", current_player->name);
			printf("Turn skipped!\n");
		}
	}
}

void sell_property(block_t *block, player_t* current_player) 
{
	int choice=1, price, price_most, inp;
	int i, j;
	char c;
	block_t *head = block;
	block_t *current;
	block_t *most_expensive;

	if(!strcmp(current_player->name, "Car")) // If it is human
	{
		while(choice && check_properties(*current_player)) // Does the player has properties to sell?
		{
			current = head -> next; // Skip the 'Start' block
			for(i=1; i<BLK_NUM; ++i) // Seek for the owned properties
			{
				for(j=0; j<12; ++j)
				{
					if(current_player->owned_block_ids[j] == i) // Property found
					{
						price = ((current->price) + (current->house_count)*(current->house_price))/2;
						printf("%d - %s (%d H) Worth:%d\n", i, current->name, current->house_count, price);
					}				
				}
				current = current->next;
			}	
			printf("0 - Exit\n");
			printf("Which one do you want to sell?:\n");	
			inp = scanf("%d", &choice);
			
			if(inp != 1 || choice >= BLK_NUM || choice < 0)
			{
				printf("Invalid choice!..\n");
				do{c= getchar();}while(c!='\n');
			}
			else
			{
				// Seek for the choice
				current = head->next; // Skip the 'Start' block
				for(i=1; i<BLK_NUM; ++i)
				{
					if(current -> block_id == choice) // Choice found
						i = BLK_NUM; // To terminate
					else
						current = current->next;
				}

				if(current!=NULL)
				{
					price = ((current->price) + (current->house_count)*(current->house_price))/2;
					current->house_count = 0; // Demolish the houses
					current->owner.type = noone; // The property is no longer owned by someone 
					int temp = current_player -> account;
					(current_player->account) += price; // The playe gets the money from selling
					printf("You had %d$, after selling you have %d$\nn", temp, current_player->account);
					// Seek for the property element in player's inventory to delete it
					for(int k=0; k<12; ++k)
					{
						if(current_player->owned_block_ids[k]==choice)
						{
							current_player->owned_block_ids[k]=0; // Delete from inventory
							k=12; // To terminate
						}
					}
				}
				else
					printf("Your choice is not on the list!\n");	
			}
		}	
	}
	else // If it is computer
	{
		current = head -> next; // Skip the 'Start' block
		most_expensive = current;
		for(i=1; i<BLK_NUM; ++i) // Seek for the most expensive owned property
		{
			for(j=0; j<12; ++j)
			{
				if(current_player->owned_block_ids[j] == i) // Property found
				{
					price_most = ((most_expensive->price) + (most_expensive->house_count)*(most_expensive->house_price))/2; 
					price = ((current->price) + (current->house_count)*(current->house_price))/2;
					if(price > price_most)
						most_expensive = current;
				}				
			}
			current = current->next;
		}	
		most_expensive->house_count = 0; // Demolish the houses
		most_expensive->owner.type = noone; // The property is no longer owned by someone 
		int temp = current_player -> account;
		(current_player->account) += price_most; // The playe gets the money from selling
		printf("Computer sold %s and gained %d$!\n", most_expensive->name, price_most);
		// Seek for the property element in player's inventory to delete it
		for(int k=0; k<12; ++k)
		{
			if(current_player->owned_block_ids[k]==most_expensive->block_id)
			{
				current_player->owned_block_ids[k]=0; // Delete from inventory
				k=12; // To terminate
			}
		}
	}
	
}

double avg_of_props(block_t* head)
{
	block_t *current = head;
	double avg=0; int count=0;

	for(int i=0; i<BLK_NUM; ++i)
	{
		if(current -> type == property)
		{
			avg += current -> price;
			++count;
		}
		current = current -> next;
	}
	avg /= count;
	return avg;
}

void gameplay (block_t *block, player_t player, player_t comp)
{
	int bankrupt=0, choice, dice, house, dept, i, inp, temp;
	turn_type turn = human;
	player_t *whose_turn, *who_waits;
	block_t *head = block, *current;
	fortune_card_t cards[CRD_NUM];
	char c;
	init_the_cards(cards);

	while(!bankrupt) //Game goes on until someone bankrupt
	{
		if(turn == human)
		{
			whose_turn = &player;
			who_waits = &comp;
		}
		else
		{
			whose_turn = &comp;
			who_waits = &player;
		}
		
		printf("\t_________________\n");
		printf("\t|               |\n");
		printf("\t| %s's Turn!.. |\n", whose_turn->name);
		printf("\t|_______________|\n");
		printf("     (%s must wait %d turn.)\n", who_waits->name, who_waits->turn_to_wait);
		
		while(who_waits->turn_to_wait != 0) // Play until opponent's punishment is over
		{
			if(turn == human)
			{
				printf("\n"); show_menu();
				for(inp = scanf("%d", &choice); inp!=1; inp = scanf("%d", &choice)) // Player selects an operation from the menu
				{	
					printf("Invalid choice. Select between 1-7!\n");
					do{c= getchar();}while(c!='\n');
					printf("\n"); show_menu();
				}
				switch(choice)
				{
					case 1: //Roll the dice
						//Decrease punish of the opponent
						who_waits->turn_to_wait -=1;

						do{dice = rand()%7;}while(dice==0); // Roll the dice (Value must be between 1-6)
						// Move according to the dice
						current = head;
						whose_turn->current_block_id += dice;
						if(whose_turn->current_block_id >= BLK_NUM) //Passes the starting point
						{
							whose_turn->current_block_id %= BLK_NUM;
							whose_turn->account += 10000; //Starting point prize
						}
						for(i=0; i<(whose_turn->current_block_id); ++i) // Go to the current block
							current = current -> next;

						show_board(head, player, comp);
						printf("You rolled the dice: %d\n", dice);
						printf("You arrived in %s\n", current->name);
						if(current->type == property) //If it is a property
						{
							if(current->owner.type == noone) //Unowned property
							{
								if(current->price <= whose_turn->account)
								{
									printf("%s is Unowned. Do you want to buy it?\n", current->name);
									printf("You have: %d$, Property price: %d$\n", whose_turn->account, current->price);
									printf("Type (Y) or (N)\n");
									do{c= getchar();}while(c!='\n');
									scanf("%c", &c);
									if(c == 'y' || c == 'Y')
										buy_property(current, whose_turn);
									else
										do{c= getchar();}while(c!='\n');	
								}
								else
									printf("You don't have enough money to buy this property!\n");
							}
							else if(current->owner.name == whose_turn->name) //Our property
							{
								if(current->house_count<3 && whose_turn->account >= current->house_price)
								{
									printf("This is your property.\n");
									printf("Do you want to build a house/ houses on your property?\n");
									printf("In %s there is/are %d house/houses and you can build %d more\n",	current->name, 
																												current->house_count, 
																												3-current->house_count);
									printf("Price per house is %d$ for %s\n", current->house_price, current->name);
									printf("You have %d$ in your account\n", whose_turn->account);
									printf("Type (Y) or (N)\n");
									do{c= getchar();}while(c!='\n');
									scanf("%c", &c);
									if(c == 'y' || c == 'Y')
										buy_houses(current, whose_turn);
									else
										do{c= getchar();}while(c!='\n');	
								}
								else if(current->house_count == 3)
									printf("This is your property and,\n there are already 3 houses on it.\n You can not build more!\n");
								else
									printf("This is your property but you don't have enough money to build houses!\n");
							}
							else //Opponent's property
							{
								printf("%s is opponent's property!\n", current->name);
								//How much do we have to pay?
								house = current->house_count;
								if(house == 0)
									dept = current->rent;
								else if(house == 1)
									dept = current->rent_1;
								else if(house == 2)
									dept = current->rent_2;
								else
									dept = current->rent_3;

								if(whose_turn->account >= dept) // We have enough money to pay
								{
									temp = whose_turn -> account;
									(whose_turn->account) -= dept; // Player pays the dept
									printf("You paid %d$ to %s\n", dept, current->owner.name);
									printf("You had %d$, after payment you have %d$\n", temp, whose_turn -> account);
									temp = who_waits -> account;
									who_waits->account += dept; // Owner of the property gets the money
									printf("Opponent's money before: %d, After your pay: %d\n", temp, who_waits->account);
								}
								else // We don't have enough money to pay
								{
									printf("You don't have enough money to pay.\n");
									printf("You have %d$ and your dept is %d$\n", whose_turn->account, dept);
									while(!bankrupt && whose_turn->account < dept)
									{
										if(check_properties(*whose_turn)) // If the player has properties to sell
										{
											printf("You have to sell one or more properties you have!\n");
											sell_property(head, whose_turn);
										}
										else
										{
											printf("%s is bankrupt!\n", whose_turn->name);
											bankrupt = turn; // The player is bankrupt
										}
									}
									if(!bankrupt) // After selling properties we finally have enough money to pay
									{
										temp = whose_turn -> account;
										whose_turn->account -= dept; // Player pays the dept
										printf("You paid %d$ to %s\n", dept, current->owner.name);
										printf("You had %d$, after payment you have %d$\n", temp, whose_turn -> account);
										temp = who_waits -> account;
										who_waits->account += dept; // Owner of the property gets the money
										printf("Opponent's money before: %d, After your pay: %d\n", temp, who_waits->account);
									}
									else // If we bankrupt
									{
										(who_waits->account) += (whose_turn->account); //We pay what we can to the opponent 
										whose_turn->account = 0; //Bankrupt
									}
								}
							}
						}
						else if(current->type == tax) //If it is a tax
						{
							temp = whose_turn -> account;
							dept = current->rent;
							if(whose_turn->account >= dept) // We have enough money to pay
							{
								(whose_turn->account) -= dept; // Player pays the dept
								printf("You had %d$, after payment you have %d$\n", temp, whose_turn -> account);
								printf("You paid %d$ to the bank\n", dept);
							}
							else // We don't have enough money to pay
							{
								printf("You don't have enough money to pay.\n");
								while(!bankrupt && whose_turn->account < dept)
								{
									if(check_properties(*whose_turn)) // If the player has properties to sell
									{
										printf("You have to sell one or more properties you have\n");
										sell_property(head, whose_turn);
										temp = whose_turn -> account;
									}
									else
									{
										printf("%s is bankrupt\n", whose_turn->name);
										bankrupt = turn; // The player is bankrupt
									}
								}
								if(!bankrupt) // After selling properties we finally have enough money to pay
								{
									(whose_turn->account) -= dept; // Player pays the dept
									printf("You had %d$, after payment you have %d$\n", temp, whose_turn -> account);
									printf("You paid %d$ to the bank\n", dept);
								}
								else
									whose_turn->account = 0; // All money we have goes to the bank, we bankrupt
							}					
						}
						else if(current->type == punish) //If it is a punish
							(whose_turn->turn_to_wait) += current->rent;
						else if(current->type == fortune) // If it is a fortune card
						{
							dice = rand()%5; // Randomly pick a fortune card
							switch(cards[dice].type)
							{
								case free_house:
									printf("%s !..\n", cards[dice].name);
									buy_houses(current, whose_turn);
									break;
								case time_travel:
									printf("%s !..\n", cards[dice].name);
									do{dice = rand()%7;}while(dice==0); // Roll a dice
									if(dice<4) // If dice is [1-3]
									{
										printf("%s is moving forward by 2 blocks!\n", whose_turn->name);
										whose_turn->current_block_id += 2; // Two blocks forward
										if(whose_turn->current_block_id >= BLK_NUM) // Passes the starting point
										{
											whose_turn->current_block_id %= BLK_NUM;
											whose_turn->account += 10000; //Starting point prize
										}
										current = head;
										for(i=0; i<(whose_turn->current_block_id); ++i) // Go to the current block
											current = current -> next;	
									}
									else
									{
										printf("%s is moving backward by 2 blocks!\n", whose_turn->name);
										whose_turn->current_block_id -= 2; // Two blocks forward
										if(whose_turn->current_block_id < 0) // Going back through the start
											whose_turn->current_block_id %= BLK_NUM;
										current = head;
										for(i=0; i<(whose_turn->current_block_id); ++i) // Go to the current block
											current = current -> next;		
									}
									break;
								case garnishment:
									printf("%s !..\n", cards[dice].name);

									temp = whose_turn->account;  
									if(whose_turn->account >= 5000)
									{
										printf("You paid 5000$ to the bank!\n");
										whose_turn->account -= 5000;	
									}
									else
									{
										printf("You don't have enough money to pay.\n");
										while(!bankrupt && whose_turn->account < dept)
										{
											if(check_properties(*whose_turn)) // If the player has properties to sell
											{
												printf("You have to sell one or more properties you have\n");
												sell_property(head, whose_turn);
												temp = whose_turn -> account;
											}
											else
											{
												printf("You're bankrupt\n");
												bankrupt = turn; // The player is bankrupt
											}
										}
										if(!bankrupt) // After selling properties we finally have enough money to pay
										{
											(whose_turn->account) -= dept; // Player pays the dept
											printf("You had %d$, after payment you have %d$\n", temp, whose_turn -> account);
											printf("You paid %d$ to the bank\n", dept);
										}
										else
											whose_turn->account = 0; // All money we have goes to the bank, we bankrupt										
									}
									break;
								case generosity:
									printf("%s !..\n", cards[dice].name);
									dept = 10000;
									if(whose_turn->account >= dept) // We have enough money to pay
									{
										temp = whose_turn -> account;
										(whose_turn->account) -= dept; // Player pays the dept
										printf("You paid %d$ to computer\n", dept);
										printf("You had %d$, after payment you have %d$\n", temp, whose_turn -> account);
										temp = who_waits -> account;
										who_waits->account += dept; // Owner of the property gets the money
										printf("Opponent's money before: %d, After your pay: %d\n", temp, who_waits->account);
									}
									else // We don't have enough money to pay
									{
										printf("You don't have enough money to pay.\n");
										printf("You have %d$ and your dept is %d$\n", whose_turn->account, dept);
										while(!bankrupt && whose_turn->account < dept)
										{
											if(check_properties(*whose_turn)) // If the player has properties to sell
											{
												printf("You have to sell one or more properties you have!\n");
												sell_property(head, whose_turn);
											}
											else
											{
												printf("You're bankrupt!\n");
												bankrupt = turn; // The player is bankrupt
											}
										}
										if(!bankrupt) // After selling properties we finally have enough money to pay
										{
											temp = whose_turn -> account;
											whose_turn->account -= dept; // Player pays the dept
											printf("You paid %d$ to the computer\n", dept);
											printf("You had %d$, after payment you have %d$\n", temp, whose_turn -> account);
											temp = who_waits -> account;
											who_waits->account += dept; // Owner of the property gets the money
											printf("Opponent's money before: %d, After your pay: %d\n", temp, who_waits->account);
										}
										else // If we bankrupt
										{
											(who_waits->account) += (whose_turn->account); //We pay what we can to the opponent 
											whose_turn->account = 0; //Bankrupt
										}
									}
									break;
								case treasure:
									printf("%s !..\n", cards[dice].name);
									temp = whose_turn->account;
									whose_turn->account += 20000;
									printf("You get 20000$ from the bank!\n");
									printf("You had %d$ after gain you have %d$\n", temp, whose_turn->account);
									break;
							}
						}
						break;
					case 2: //Show my account
						printf("My account: %d$\n", whose_turn->account);
						break;
					case 3: //Show my properties
						if(check_properties(*whose_turn))
						{
							printf("My properties:\n");
							for(i=0; i<12; ++i)
							{
								current = head;
								if(whose_turn->owned_block_ids[i] != 0)
								{
									for(int j=0; j<(whose_turn->owned_block_ids[i]); ++j)
										current = current->next;
									printf("%d - %s (%d H)\n", i+1, current->name, current->house_count);
								}
							}		
						}
						else
							printf("You don't have any properties!\n");
						break;
					case 4: //Show property deeds
						show_properties(head);
						break;
					case 5: //Buy property
						current = head;
						for(i=0; i<whose_turn->current_block_id; ++i)
							current = current->next;
						if(current->type == property)
							buy_property(current, whose_turn);
						else
							printf("This is not a property!\n");
						break;
					case 6: //Buy houses
						current = head;
						for(i=0; i<whose_turn->current_block_id; ++i)
							current = current->next;
						if(current->owner.type == whose_turn->type && current->type == property)
						{
							printf("In %s there is/are %d house/houses\n", current->name, current->house_count);
							printf("Price per house is %d$ for %s\n", current->house_price, current->name);
							printf("You have %d$ in your account\n", whose_turn->account);
							buy_houses(current, whose_turn);
						}
						else if(current->type != property)
							printf("This is not a property and not available to build houses!\n");
						else
							printf("This is not your property!\n");
						break;
					case 7: //Sell property
						if(check_properties(*whose_turn)) // If player has properties to sell
							sell_property(head, whose_turn);
						else
							printf("You don't have any properties to sell!\n");
						break;
					default:
						printf("Invalid choice. Select between 1-7!\n");		
						do{c= getchar();}while(c!='\n');
						break;
				}					
			}
			else // Computer's turn
			{
				who_waits->turn_to_wait -=1; // Decrease punish of the opponent

				do{dice = rand()%7;}while(dice==0); // Computer rolls the dice 1-6
				// Move according to the dice
				whose_turn->current_block_id += dice;
				if(whose_turn->current_block_id >= BLK_NUM) //Passes the starting point
				{
					whose_turn->current_block_id %= BLK_NUM;
					whose_turn->account += 10000; //Starting point prize
				}
				// Go to the current block
				current = head;
				for(i=0; i<(whose_turn->current_block_id); ++i)
					current = current -> next;

				show_board(head, player, comp);
				printf("Computer rolled the dice: %d\n", dice);
				printf("It arrived in %s\n", current->name);
				if(current->type == property) //If it is a property
				{
					if(current->owner.type == noone) //Unowned property
					{
						if(current->price <= whose_turn->account)
						{
							if(current->price < avg_of_props(head)) 
								buy_property(current, whose_turn);	
							else
							{
								do{dice = rand()%7;}while(dice==0); // Computer rolls the dice
								if(dice < 4) // If dice is between 1-3
									buy_property(current, whose_turn);
								else
									printf("Computer skipped its turn!\n");
							}
						}	
						else
							printf("Computer doesn't have enough money to buy this property!\n");
					}
					else if(current->owner.name == whose_turn->name) //Our property
					{
						if(current->house_count<3 && whose_turn->account >= current->house_price)
						{
							int count=0;
							for(i=0; i<12; ++i) // Counts how many properties computer has
							{
								if(whose_turn->owned_block_ids[i]!=0)
									++count;
							}
							if(count >= 4) // If computer owns the 1/3 of all properties
							{
								do{dice = rand()%7;}while(dice==0); // Computer rolls the dice 1-6
								if(dice < 4) // If dice is between 1-3
									buy_houses(current, whose_turn);
								else
									printf("Computer skipped its turn!\n");
							}
							else
								printf("Computer skipped its turn!\n");
						}
						else if(current->house_count==3)
							printf("Computer arrived in its property with max count of houses.\n");
						else
							printf("Computer arrived in its property but has no money to build houses!\n");
					}
					else //Opponent's property
					{
						printf("%s is your property!\n", current->name);
						//How much does it has to pay?
						house = current->house_count;
						if(house == 0)
							dept = current->rent;
						else if(house == 1)
							dept = current->rent_1;
						else if(house == 2)
							dept = current->rent_2;
						else
							dept = current->rent_3;

						if(whose_turn->account >= dept) // Computer has enough money to pay
						{
							temp = whose_turn -> account;
							(whose_turn->account) -= dept; // Player pays the dept
							printf("Computer paid %d$ to you!\n", dept);
							printf("Computer had %d$, after payment it has %d$\n", temp, whose_turn -> account);
							temp = who_waits -> account;
							who_waits->account += dept; // Owner of the property gets the money
							printf("Your money before: %d$, After your gain: %d\n", temp, who_waits->account);
						}
						else // Computer doesn't have enough money to pay
						{
							printf("Computer doesn't have enough money to pay!\n");
							printf("Computer has %d$ and its dept is %d$\n", whose_turn->account, dept);
							while(!bankrupt && whose_turn->account < dept)
							{
								if(check_properties(*whose_turn)) // If the player has properties to sell
								{
									printf("Computer is going to sell its properties.\n");
									sell_property(head, whose_turn);
								}
								else
								{
									printf("Computer is bankrupt!\n");
									bankrupt = turn; // The player is bankrupt
								}
							}
							if(!bankrupt) // After selling properties computer finally has enough money to pay
							{
								temp = whose_turn -> account;
								whose_turn->account -= dept; // Computer pays the dept
								printf("Computer paid %d$ to you!\n", dept);
								printf("Computer had %d$, after payment it has %d$\n", temp, whose_turn -> account);
								temp = who_waits -> account;
								who_waits->account += dept; // Owner of the property gets the money
								printf("You had %d$, after your gain you have %d$\n", temp, who_waits->account);
							}
							else // If we bankrupt
							{
								(who_waits->account) += (whose_turn->account); //Computer pays what it can to us 
								whose_turn->account = 0; //Bankrupt
							}
						}
					}
				}
				else if(current->type == tax) //If it is a tax
				{
					temp = whose_turn -> account;
					dept = current->rent;
					if(whose_turn->account >= dept) // Computer has enough money to pay
					{
						(whose_turn->account) -= dept; // Computer pays the dept
						printf("Computer had %d$, after payment Computer have %d$\n", temp, whose_turn -> account);
						printf("Computer paid %d$ to the bank\n", dept);
					}
					else // Computer doesn't have enough money to pay
					{
						printf("Computer does't have enough money to pay.\n");
						while(!bankrupt && whose_turn->account < dept)
						{
							if(check_properties(*whose_turn)) // If the computer has properties to sell
							{
								printf("Computer is going to sell its properties.\n");
								sell_property(head, whose_turn);
								temp = whose_turn -> account;
							}
							else
							{
								printf("Computer is bankrupt!\n");
								bankrupt = turn; // The player is bankrupt
							}
						}
						if(!bankrupt) // After selling properties computer finally has enough money to pay
						{
							(whose_turn->account) -= dept; // Computer pays the dept
							printf("Computer had %d$, after payment Computer have %d$\n", temp, whose_turn -> account);
							printf("Computer paid %d$ to the bank\n", dept);
						}
						else
							whose_turn->account = 0; // All money computer has goes to the bank, it bankrupt
					}					
				}
				else if(current->type == punish) // If it is a punish
					(whose_turn->turn_to_wait) += current->rent;
				else if(current->type == fortune) // If it is a fortune card
				{
					dice = rand()%5; // Randomly pick a fortune card
					switch(cards[dice].type)
					{
						case free_house:
							printf("%s !..\n", cards[dice].name);
							buy_houses(current, whose_turn);
							break;
						case time_travel:
							printf("%s !..\n", cards[dice].name);
							do{dice = rand()%7;}while(dice==0); // Roll a dice
							if(dice<4) // If dice is [1-3]
							{
								printf("%s is moving forward by 2 blocks!\n", whose_turn->name);
								whose_turn->current_block_id += 2; // Two blocks forward
								if(whose_turn->current_block_id >= BLK_NUM) // Passes the starting point
								{
									whose_turn->current_block_id %= BLK_NUM;
									whose_turn->account += 10000; //Starting point prize
								}
								current = head;
								for(i=0; i<(whose_turn->current_block_id); ++i) // Go to the current block
									current = current -> next;	
							}
							else
							{
								printf("%s is moving backward by 2 blocks!\n", whose_turn->name);
								whose_turn->current_block_id -= 2; // Two blocks forward
								if(whose_turn->current_block_id < 0) // Going back through the start
									whose_turn->current_block_id %= BLK_NUM;
								current = head;
								for(i=0; i<(whose_turn->current_block_id); ++i) // Go to the current block
									current = current -> next;		
							}
							break;
						case garnishment:
							printf("%s !..\n", cards[dice].name);

							dept = 5000;
							if(whose_turn->account >= dept)
							{
								printf("Computer paid 5000$ to the bank!\n");
								whose_turn->account -= dept;	
							}
							else
							{
								printf("Computer does't have enough money to pay.\n");
								while(!bankrupt && whose_turn->account < dept)
								{
									if(check_properties(*whose_turn)) // If the computer has properties to sell
									{
										printf("Computer is going to sell its properties.\n");
										sell_property(head, whose_turn);
										temp = whose_turn -> account;
									}
									else
									{
										printf("Computer is bankrupt!\n");
										bankrupt = turn; // The player is bankrupt
									}
								}
								if(!bankrupt) // After selling properties computer finally has enough money to pay
								{
									(whose_turn->account) -= dept; // Computer pays the dept
									printf("Computer had %d$, after payment Computer have %d$\n", temp, whose_turn -> account);
									printf("Computer paid %d$ to the bank\n", dept);
								}
								else
									whose_turn->account = 0; // All money computer has goes to the bank, it bankrupt									
							}
							break;
						case generosity:
							printf("%s !..\n", cards[dice].name);
							dept = 10000;
							if(whose_turn->account >= dept) // We have enough money to pay
							{
								temp = whose_turn -> account;
								(whose_turn->account) -= dept; // Player pays the dept
								printf("Computer paid %d$ to you\n", dept);
								printf("Computer had %d$, after payment Computer has %d$\n", temp, whose_turn -> account);
								temp = who_waits -> account;
								who_waits->account += dept; // Owner of the property gets the money
								printf("You had %d$, After your gain you have %d$\n", temp, who_waits->account);
							}
							else // We don't have enough money to pay
							{
								printf("Computer does't have enough money to pay.\n");
								printf("Computer have %d$ and its dept is %d$\n", whose_turn->account, dept);
								while(!bankrupt && whose_turn->account < dept)
								{
									if(check_properties(*whose_turn)) // If the player has properties to sell
									{
										printf("Computer have to sell one or more properties it has!\n");
										sell_property(head, whose_turn);
									}
									else
									{
										printf("Computer's bankrupt!\n");
										bankrupt = turn; // The player is bankrupt
									}
								}
								if(!bankrupt) // After selling properties computer finally has enough money to pay
								{
									temp = whose_turn -> account;
									whose_turn->account -= dept; // Player pays the dept
									printf("Computer paid %d$ to you\n", dept);
									printf("Computer had %d$, after payment Computer have %d$\n", temp, whose_turn -> account);
									temp = who_waits -> account;
									who_waits->account += dept; // Owner of the property gets the money
									printf("Your money before: %d, After your gain: %d\n", temp, who_waits->account);
								}
								else // If computer bankrupt
								{
									(who_waits->account) += (whose_turn->account); //We pay what we can to the opponent 
									whose_turn->account = 0; //Bankrupt
								}
							}
							break;
						case treasure:
							printf("%s !..\n", cards[dice].name);
							temp = whose_turn->account;
							whose_turn->account += 20000;
							printf("Computer get 20000$ from the bank!\n");
							printf("Computer had %d$ after gain it has %d$\n", temp, whose_turn->account);
							break;
					}
				}
			}
		}
		who_waits->turn_to_wait = 1;
		
		if(turn == human)
			turn = computer;
		else
			turn = human;
	}	

	printf("\n");
	if(bankrupt == human)
	{
		printf("Computer (%s) Won!\n", comp.name);
		printf("%s's money : %d$\n", comp.name, comp.account);
	}
	else
	{
		printf("Player (%s) Won!\n", player.name);
		printf("%s's money : %d$\n", player.name, player.account);
	}
}

void show_menu(void)
{
	printf("1 - Roll the dice\n");
	printf("2 - Show my account\n");
	printf("3 - Show my properties\n");
	printf("4 - Show property deeds\n");
	printf("5 - Buy property\n");
	printf("6 - Buy house\n");
	printf("7 - Sell property\n");
	printf("Please select an option to continue:\n");
}

//Checks if the player has properties
int check_properties(player_t player)
{
	int check=0; 
	for(int i=0; i<12; ++i)
	{
		if(player.owned_block_ids[i]!=0)
		{
			check=1; // Yes, the player has properties
			i=12; // To terminate
		}
	}
	return check;
}

//Checks if the player has a property that has less than 3 houses
int check_empty_props(block_t *head, player_t player)
{
	int check=0; 
	block_t *current = head;
	for(int i=0; i<12; ++i)
	{
		if(player.owned_block_ids[i]!=0)
		{
			while(current->block_id != player.owned_block_ids[i]) // Find the prop
				current = current->next;

			if(current->house_count < 3) //If the prop empty
			{
				check = 1;
				i = 12; // To terminate
			}
		}
	}
	return check;
}

/*
		Gebze Technical University
	Introduction to Computer Programming
				Homework-7

		Author:Ömer Faruk BİTİKÇİOĞLU
											*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#define NAM_SIZ 20
#define BLK_NUM 20
#define BLK_WID 26

typedef enum {
	noone, cap, car
} player_type;

typedef enum {
	start, property, tax, punish
} block_type;

typedef enum {
	player1 = 1, player2 = 2
} turn_type;

typedef struct player {
	player_type type;
	int current_block_id;
	int owned_block_ids[12];
	int account;
	int turn_to_wait;
	char *name;
} player_t;

typedef struct block {
	int block_id;
	char *name;
	int price;
	int rent, rent_1, rent_2, rent_3;
	int house_price;
	int house_count;
	player_t owner;
	block_type type;
} block_t;

char block_names[BLK_NUM][NAM_SIZ]= {"Start", "Esenyurt", "Car Park", "Tuzla", "Arnavutkoy", "Wait 2 Turn", "Catalca", "Beykoz", 
"Car Fix", "Maltepe", "Bills", "Sisli", "Oil", "Atasehir", "Sariyer", "Wait 1 Turn", "Kadikoy", "Besiktas", "Vocation", "Bebek"};
char player_names[2][4]={"Car", "Cap"};

void init_the_board(block_t board[BLK_NUM]);
void init_the_players(player_t* player_one, player_t* player_two);
void show_board(block_t board[BLK_NUM], player_t player_one, player_t player_two);
void print_dashes(int num_of_dashes);
void show_rent_or_tax(block_t board[BLK_NUM], int block_id);
void show_players_on_board(player_t player_one, player_t player_two, int block_id);
void show_properties(block_t board[BLK_NUM]);
void buy_property(block_t* current_block, player_t* current_player);
void buy_houses(block_t* current_block, player_t* current_player);
void sell_property(block_t board[BLK_NUM], player_t* current_player);
void gameplay (block_t board[BLK_NUM], player_t player_one, player_t player_two);
int check_properties(player_t player);
void show_menu(void);

int main(void)
{
	block_t board[BLK_NUM];	
	char names[BLK_NUM][NAM_SIZ];
	player_t player1;
	player_t player2;

	srand(time(NULL));

	printf("\n\tM O N O P O L Y\n");
	
	init_the_board(board);
	init_the_players(&player1, &player2);
	show_board(board, player1, player2);
	gameplay(board, player1, player2);

	return 0;
}

void init_the_board(block_t board[BLK_NUM])
{
	int i=0;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 0;
	board[i].rent = 0;
	board[i].rent_1 = 0;
	board[i].rent_2 = 0;
	board[i].rent_3 = 0;
	board[i].house_price = 0;
	board[i].house_count = 0;
	board[i].owner.type = noone; 
	board[i++].type = start;
	
	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 16000;
	board[i].rent = 800;
	board[i].rent_1 = 4000;
	board[i].rent_2 = 9000;
	board[i].rent_3 = 25000;
	board[i].house_price = 10000;
	board[i].house_count = 0;
	board[i].owner.type = noone; 
	board[i++].type = property;
	
	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 0;
	board[i].rent = 1500;
	board[i].rent_1 = 0;
	board[i].rent_2 = 0;
	board[i].rent_3 = 0;
	board[i].house_price = 0;
	board[i].house_count = 0;
	board[i].owner.type = noone; 
	board[i++].type = tax;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 16500;
	board[i].rent = 850;
	board[i].rent_1 = 4250;
	board[i].rent_2 = 9500;
	board[i].rent_3 = 26000;
	board[i].house_price = 12000;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = property;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 17000;
	board[i].rent = 875;
	board[i].rent_1 = 4500;
	board[i].rent_2 = 10000;
	board[i].rent_3 = 28000;
	board[i].house_price = 12000;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = property;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 0;
	board[i].rent = 2;
	board[i].rent_1 = 0;
	board[i].rent_2 = 0;
	board[i].rent_3 = 0;
	board[i].house_price = 0;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = punish;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 20000;
	board[i].rent = 1000;
	board[i].rent_1 = 5000;
	board[i].rent_2 = 12000;
	board[i].rent_3 = 30000;
	board[i].house_price = 13000;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = property;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 23000;
	board[i].rent = 1100;
	board[i].rent_1 = 5500;
	board[i].rent_2 = 12500;
	board[i].rent_3 = 33000;
	board[i].house_price = 13000;
	board[i].house_count = 0;
	board[i].owner.type = noone; 
	board[i++].type = property;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 0;
	board[i].rent = 1750;
	board[i].rent_1 = 0;
	board[i].rent_2 = 0;
	board[i].rent_3 = 0;
	board[i].house_price = 0;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = tax;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 30000;
	board[i].rent = 1350;
	board[i].rent_1 = 7000;
	board[i].rent_2 = 15000;
	board[i].rent_3 = 40000;
	board[i].house_price = 15000;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = property;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 0;
	board[i].rent = 2000;
	board[i].rent_1 = 0;
	board[i].rent_2 = 0;
	board[i].rent_3 = 0;
	board[i].house_price = 0;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = tax;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 33000;
	board[i].rent = 1500;
	board[i].rent_1 = 8000;
	board[i].rent_2 = 16000;
	board[i].rent_3 = 42000;
	board[i].house_price = 16000;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = property;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 0;
	board[i].rent = 2250;
	board[i].rent_1 = 0;
	board[i].rent_2 = 0;
	board[i].rent_3 = 0;
	board[i].house_price = 0;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = tax;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 35000;
	board[i].rent = 1600;
	board[i].rent_1 = 8500;
	board[i].rent_2 = 17000;
	board[i].rent_3 = 45000;
	board[i].house_price = 17000;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = property;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 40000;
	board[i].rent = 1750;
	board[i].rent_1 = 9500;
	board[i].rent_2 = 19000;
	board[i].rent_3 = 48000;
	board[i].house_price = 19000;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = property;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 0;
	board[i].rent = 1;
	board[i].rent_1 = 0;
	board[i].rent_2 = 0;
	board[i].rent_3 = 0;
	board[i].house_price = 0;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = punish;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 43000;
	board[i].rent = 1900;
	board[i].rent_1 = 11000;
	board[i].rent_2 = 21500;
	board[i].rent_3 = 55000;
	board[i].house_price = 23000;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = property;	

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 60000;
	board[i].rent = 2500;
	board[i].rent_1 = 15000;
	board[i].rent_2 = 28000;
	board[i].rent_3 = 60000;
	board[i].house_price = 30000;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = property;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 0;
	board[i].rent = 5000;
	board[i].rent_1 = 0;
	board[i].rent_2 = 0;
	board[i].rent_3 = 0;
	board[i].house_price = 0;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i++].type = tax;

	board[i].block_id = i;
	board[i].name = block_names[i];
	board[i].price = 70000;
	board[i].rent = 3500;
	board[i].rent_1 = 20000;
	board[i].rent_2 = 35500;
	board[i].rent_3 = 65000;
	board[i].house_price = 35000;
	board[i].house_count = 0;
	board[i].owner.type = noone;  
	board[i].type = property;
}

void init_the_players(player_t* player_one, player_t* player_two)
{
	char c;
	int pick=0, i;

	//Get player one's specifications
	printf("\n\tPlayer 1...\n");		
	while(pick != 1 && pick != 2)
	{
		printf("Pick your type: 1)Car 2)Cap\n");
		scanf("%d", &pick);
		if(pick == 1)
		{
			player_one->type = car;
			player_one->name = player_names[0];
			printf("Player1 has Car\n");
		}
		else if(pick == 2)
		{
			player_one->type = cap;
			player_one->name = player_names[1];
			printf("Player1 has Cap\n");
		}
		else
		{
			printf("Invalid choice, try again(type 1 or 2)\n");		
			do{c= getchar();}while(c!='\n');
		}
	}
	player_one->current_block_id = 0;
	for(i=0; i<12; ++i)
		player_one->owned_block_ids[i]=0;
	player_one->account = 100000;
	player_one->turn_to_wait = 1;

	//Get player one's specifications
	printf("\n\tPlayer 2...\n");		
	if(player_one->type == car)
	{
		player_two->type = cap;
		player_two->name = player_names[1];
		printf("Player2 has Cap\n");
	}
	else
	{
		player_two->type = car;
		strcpy(player_two->name, player_names[0]);
		printf("Player1 has Car\n");
	}
	player_two->current_block_id = 0;
	for(i=0; i<12; ++i)
		player_two->owned_block_ids[i]=0;
	player_two->account = 100000;
	player_two->turn_to_wait = 1;	
}

void show_board(block_t board[BLK_NUM], player_t player_one, player_t player_two)
{ 
	int i, j, k, t, space; //Loop indices

	printf("\n"); print_dashes(BLK_WID*(BLK_NUM/4+1)+1); printf("\n");
	//Prints top of the board
	for(i=0; i<3; ++i) //First 3 Row
	{
		printf("|");
		for(j=0; j<6; ++j) //First 6 block
		{
			if(i==0) //First row : name of the block
			{
				space = (BLK_WID/2+4);
				printf("%*s", space, board[j].name);
				printf("%*s", BLK_WID-space, "|");
			}
			else if(i==1) //Second row : price of the property or rent of the tax
				show_rent_or_tax(board, j);
			else //Third row : Shows players if they are on the block
				show_players_on_board(player_one, player_two, j);
		}
		printf("\n");
	}
	print_dashes(BLK_WID*(BLK_NUM/4+1)+1); printf("\n");
	//Prints middle of the board
	int count=0;
	for(j=BLK_NUM-1, k=BLK_NUM/4+1; j>15 && k<10; --j, ++k)
	{
		printf("|");
		for(i=0; i<3; ++i) //Three row per block
		{
			if(i==0) //First row : name of the block
			{
				space = (BLK_WID/2+4);
				printf("%*s", space, board[j].name);
				printf("%*s", BLK_WID-space, "|");
				
				for(t=0; t<(BLK_NUM/4-1)*BLK_WID-1; ++t)
					printf(" ");
				
				printf("|"); 
				printf("%*s", space, board[k].name);
				printf("%*s\n", BLK_WID-space, "|");
			}
			else if(i==1) //Second row : price of the property or rent of the tax
			{
				printf("|");
				show_rent_or_tax(board, j);
				
				for(t=0; t<(BLK_NUM/4-1)*BLK_WID-1; ++t)
					printf(" ");	

				printf("|");
				show_rent_or_tax(board, k);
				printf("\n");
			}
			else //Third row : Shows players if they are on the block
			{
				printf("|");
				show_players_on_board(player_one, player_two, j);

				for(t=0; t<(BLK_NUM/4-1)*BLK_WID-1; ++t)
					printf(" ");

				printf("|");
				show_players_on_board(player_one, player_two, k);
				printf("\n");
			}
		}
		count++;
		if(count!=4)
		{
			print_dashes(BLK_WID+1);
			for(t=0; t<(BLK_NUM/4-1)*BLK_WID-1; ++t)
				printf(" ");	
			print_dashes(BLK_WID+1); printf("\n");
		}
	}
	print_dashes(BLK_WID*(BLK_NUM/4+1)+1); printf("\n");
	//Prints bottom of the board
	for(i=0; i<3; ++i) //Last 3 Row
	{
		printf("|");
		for(j=15; j>9; --j) //Last 6 block (15th to 9th)
		{
			if(i==0) //First row : name of the block
			{
				space = (BLK_WID/2+4);
				printf("%*s", space, board[j].name);
				printf("%*s", BLK_WID-space, "|");
			}
			else if(i==1) //Second row : price of the property or rent of the tax
				show_rent_or_tax(board, j);
			else //Third row : Shows players if they are on the block
				show_players_on_board(player_one, player_two, j);
		}
		printf("\n");
	}
	print_dashes(BLK_WID*(BLK_NUM/4+1)+1); printf("\n\n");
}

void print_dashes(int num_of_dashes)
{
	for(int i=0; i<num_of_dashes; ++i)
		printf("-");
}

//Shows the rent of the properties or taxes
void show_rent_or_tax(block_t board[BLK_NUM], int block_id)
{
	int space = (BLK_WID/2+4);
	
	if(board[block_id].type == property) //If it is a property
		printf("%*d$", space-1, board[block_id].price);
	else if(board[block_id].type == tax) //If it is a tax
		printf("%*d$", space-1, board[block_id].rent);
	else
	{
		for(int i=0; i<space; ++i)
			printf(" ");			
	}
	printf("%*s", BLK_WID-space, "|");	
}

//Prints the vehicle name of the player if he/she on the given block id  
void show_players_on_board(player_t player_one, player_t player_two, int block_id)
{
	int space = (BLK_WID/2+4);

	if(player_one.current_block_id == block_id && player_two.current_block_id == block_id) //If both player on the block
		printf("%*s", space, "Car, Cap");
	else if(player_one.current_block_id == block_id) //If just player one on the block
	{
		switch(player_one.type)
		{ 
			case car: printf("%*s", space, "Car"); break;
			case cap: printf("%*s", space, "Cap"); break;
		}
	}
	else if(player_two.current_block_id == block_id) //If just player two on the block
	{
		switch(player_two.type)
		{
			case car: printf("%*s", space, "Car"); break;
			case cap: printf("%*s", space, "Cap"); break;
		}
	}
	else //No one on the block
		for(int i=0; i<space; ++i)
			printf(" ");
	printf("%*s", BLK_WID-space, "|");
}

void show_properties(block_t board[BLK_NUM])
{
	int choice, exit=0, i;
	char c;

	while(!exit)
	{
		printf("\n\nPlease select a property to see details:\n");
		for(int i=0; i<20; ++i)
		{
			if(board[i].type == property)
				printf("%d - %s\n", board[i].block_id, board[i].name);
		}
		printf("0 - Exit\n");
		scanf("%d", &choice);

		if(choice<20 && board[choice].type==property)
		{
			printf("\n\n\n");
			print_dashes(35); printf("\n");
			
			printf("|%22s", board[choice].name);
			printf("%12s\n", "|");
			
			print_dashes(35); printf("\n");
			
			printf("|%13s%15d$", "Rent", board[choice].rent);
			printf("%5s\n", "|");
			printf("|%13s%15d$", "Rent 1 H", board[choice].rent_1);
			printf("%5s\n", "|");
			printf("|%13s%15d$", "Rent 2 H", board[choice].rent_2);
			printf("%5s\n", "|");
			printf("|%13s%15d$", "Rent 3 H", board[choice].rent_3);
			printf("%5s\n", "|");

			print_dashes(35); printf("\n");

			printf("|%13s%15d$", "House Price", board[choice].house_price);
			printf("%5s\n", "|");

			print_dashes(35); printf("\n\n\n");
		}
		else if(choice==0)
			exit = 1;
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

	if(	current_block->owner.type == noone 							&& 	//Property has no owner
		current_player->current_block_id == current_block->block_id &&	//Player is on the property
		current_player->account >= current_block->price 			)	//Player can afford the property
	{
		(current_player->account) -= (current_block->price); //Player pays the price
		(current_block->owner) = (*current_player); //Player owns the property

		for(int i=0; i<12; ++i) // Searches through owned_block_ids array
		{
			if(current_player->owned_block_ids[i] == 0) // Find empty space in the array
			{
				current_player->owned_block_ids[i] = current_player->current_block_id; // Added to owned properties
				i=12; //To terminate
			}
		}

		if(current_player->account >= current_block->house_price) //If player has money to build houses
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
		else
			printf("This is your property now but you don't have enough money to build houses!\n");
		
	}
	else if(current_block->owner.type != noone) // If property has an owner
	{
		if(current_block->owner.type == current_player->type)
			printf("This is already your property.\n");
		else
			printf("This property belongs to someone else!\n");
	}
	else if(current_player->account < current_block->price)
		printf("You don't have enough money to buy this property\n");
	else 
		printf("You are not on the block\n");
}

void buy_houses(block_t* current_block, player_t* current_player)
{
	int inp, houses;
	char c;

	printf("How many houses do you want to build?\n");
	inp = scanf("%d", &houses);

	if(	houses > 0 && houses <= 3 && // You can build [1-3] houses
		current_player->account >= houses*(current_block->house_price) && // Does player have enough money 
		current_block->house_count + houses <=3) // House count can't be bigger than 3
	{
		int temp = current_player -> account;
		current_player->account -= houses*(current_block->house_price); //Player pays the price of houses
		printf("Current money: %d After buying: %d\n", temp, current_player->account);
		current_block->house_count += houses; // House count increased
	}
	else if(houses < 0 || houses > 3 || current_block->house_count + houses > 3)
		printf("You can only build up to 3 houses!\n");
	else if(inp != 1)
	{
		printf("Invalid input!\n");
		do{c= getchar();}while(c!='\n');
	}
	else if( current_player->account < houses*(current_block->house_price) )
		printf("You don't have enough money!\n");	
}

void sell_property(block_t board[BLK_NUM], player_t* current_player) 
{
	int choice=1, price, inp;
	char c;

	while(choice && check_properties(*current_player))
	{
		for(int i=1; i<BLK_NUM; ++i)
			for(int j=0; j<12; ++j)
			{
				if(current_player->owned_block_ids[j] == i)
				{
					price = ((board[i].price) + (board[i].house_count)*(board[i].house_price))/2;
					printf("%d - %s (%d H) Worth:%d\n", i, board[i].name, board[i].house_count, price);
				}
			}
		printf("0 - Exit\n");
		printf("Which one do you want to sell?:\n");
		inp = scanf("%d", &choice);
		if(inp != 1)
		{
			printf("Invalid choice!..\n");
			do{c= getchar();}while(c!='\n');
		}
		else if(choice > 0 && choice < BLK_NUM && board[choice].owner.type == current_player->type)
		{
			price = ((board[choice].price) + (board[choice].house_count)*(board[choice].house_price))/2;
			board[choice].house_count = 0; //Delete houses
			board[choice].owner.type = noone; //Property belongs to noone now
			int temp = current_player -> account;
			(current_player->account) += price; //Gets the money from selling
			printf("Current money: %d After selling: %d\n", temp, current_player->account);
			for(int k=0; k<12; ++k)
			{
				if(current_player->owned_block_ids[k]==choice)
				{
					current_player->owned_block_ids[k]=0; // Delete from inventory
					k=12; // To terminate
				}
			}
		}
		else if(choice)
		{
			printf("Invalid choice!..\n");
			do{c= getchar();}while(c!='\n');
		}	
	}
}

void gameplay (block_t board[BLK_NUM], player_t player_one, player_t player_two)
{
	int bankrupt=0, choice, dice, house, dept, i, inp;
	turn_type turn = player1;
	player_t *whose_turn, *who_waits;
	char c;

	while(!bankrupt) //Game goes on until someone bankrupt
	{
		if(turn == player1)
		{
			whose_turn = &player_one;
			who_waits = &player_two;
		}
		else
		{
			whose_turn = &player_two;
			who_waits = &player_one;
		}
		
		printf("\t_________________\n");
		printf("\t|               |\n");
		printf("\t| %s's Turn!.. |\n", whose_turn->name);
		printf("\t|_______________|\n");
		printf("     (%s must wait %d turn.)\n", who_waits->name, who_waits->turn_to_wait);
		while(who_waits->turn_to_wait != 0)
		{
			printf("\n"); show_menu();
			for(inp = scanf("%d", &choice); inp!=1; inp = scanf("%d", &choice))
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

					do{dice = rand()%7;}while(dice==0); // Roll the dice
					// Move according to the dice
					whose_turn->current_block_id += dice;
					if(whose_turn->current_block_id >= BLK_NUM) //Passes the starting point
					{
						whose_turn->current_block_id %= BLK_NUM;
						whose_turn->account += 10000; //Starting point prize
					}
					show_board(board, player_one, player_two);
					printf("Your dice: %d\n", dice);
					printf("You arrived in %s\n", board[whose_turn->current_block_id].name);
					if(board[whose_turn->current_block_id].type == property) //If it is a property
					{
						if(board[whose_turn->current_block_id].owner.type == noone) //Unowned property
						{
							if(board[whose_turn->current_block_id].price <= whose_turn->account)
							{
								printf("%s is Unowned. Do you want to buy it?\n", board[whose_turn->current_block_id].name);
								printf("You have: %d$, Property price: %d$\n", whose_turn->account, board[whose_turn->current_block_id].price);
								printf("Type (Y) or (N)\n");
								do{c= getchar();}while(c!='\n');
								scanf("%c", &c);
								if(c == 'y' || c == 'Y')
									buy_property(&board[whose_turn->current_block_id], whose_turn);
								else
									do{c= getchar();}while(c!='\n');	
							}
							else
								printf("You don't have enough money to buy this property!\n");
						}
						else if(board[whose_turn->current_block_id].owner.name == whose_turn->name) //Our property
						{
							if(board[whose_turn->current_block_id].house_count<3 && whose_turn->account >= board[whose_turn->current_block_id].house_price)
							{
								printf("This is your property.\n");
								printf("Do you want to build a house/ houses on your property?\n");
								printf("In %s there is/are %d house/houses and you can build %d more\n",	board[whose_turn->current_block_id].name, 
																											board[whose_turn->current_block_id].house_count, 
																											3-board[whose_turn->current_block_id].house_count);
								printf("Price per house is %d$ for %s\n", board[whose_turn->current_block_id].house_price, board[whose_turn->current_block_id].name);
								printf("You have %d$ in your account\n", whose_turn->account);
								printf("Type (Y) or (N)\n");
								do{c= getchar();}while(c!='\n');
								scanf("%c", &c);
								if(c == 'y' || c == 'Y')
									buy_houses(&board[whose_turn->current_block_id], whose_turn);
								else
									do{c= getchar();}while(c!='\n');	
							}
							else if(board[whose_turn->current_block_id].house_count == 3)
								printf("This is your property and,\n there are already 3 houses on it.\n You can not build more!\n");
							else
								printf("This is your property but you don't have enough money to build houses!\n");
						}
						else //Opponent's property
						{
							printf("%s is opponent's property!\n", board[whose_turn->current_block_id].name);
							//How much do we have to pay?
							house = board[whose_turn->current_block_id].house_count;
							if(house == 0)
								dept = board[whose_turn->current_block_id].rent;
							else if(house == 1)
								dept = board[whose_turn->current_block_id].rent_1;
							else if(house == 2)
								dept = board[whose_turn->current_block_id].rent_2;
							else
								dept = board[whose_turn->current_block_id].rent_3;

							if(whose_turn->account >= dept) // We have enough money to pay
							{
								int temp = whose_turn -> account;
								(whose_turn->account) -= dept; // Player pays the dept
								printf("You paid %d$ to %s\n", dept, board[whose_turn->current_block_id].owner.name);
								printf("Current money: %d After paying: %d\n", temp, whose_turn -> account);
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
										sell_property(board, whose_turn);
									}
									else
									{
										printf("%s is bankrupt!\n", whose_turn->name);
										bankrupt = turn; // The player is bankrupt
									}
								}
								if(!bankrupt) // After selling properties we finally have enough money to pay
								{
									int temp = whose_turn -> account;
									whose_turn->account -= dept; // Player pays the dept
									printf("You paid %d$ to %s\n", dept, board[whose_turn->current_block_id].owner.name);
									printf("Current money: %d After paying: %d\n", temp, whose_turn -> account);
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
					else if(board[whose_turn->current_block_id].type == tax) //If it is a tax
					{
						int temp = whose_turn -> account;
						dept = board[whose_turn->current_block_id].rent;
						if(whose_turn->account >= dept) // We have enough money to pay
						{
							(whose_turn->account) -= dept; // Player pays the dept
							printf("Current money: %d After paying: %d\n", temp, whose_turn -> account);
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
									sell_property(board, whose_turn);
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
								printf("Current money: %d After paying: %d\n", temp, whose_turn -> account);
								printf("You paid %d$ to the bank\n", dept);
							}
							else
								whose_turn->account = 0; // All money we have goes to the bank, we bankrupt
						}					
					}
					else if(board[whose_turn->current_block_id].type == punish) //If it is a punish
						(whose_turn->turn_to_wait) += board[whose_turn->current_block_id].rent;
					printf("My account: %d$\n", whose_turn->account);
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
							if(whose_turn->owned_block_ids[i] != 0)
								printf("%d - %s (%d H)\n", i+1, board[whose_turn->owned_block_ids[i]].name, board[whose_turn->owned_block_ids[i]].house_count);
						}		
					}
					else
						printf("You don't have any properties!\n");
					break;
				case 4: //Show property deeds
					show_properties(board);
					break;
				case 5: //Buy property
					if(board[whose_turn->current_block_id].type == property)
						buy_property(&board[whose_turn->current_block_id], whose_turn);
					else
						printf("This is not a property!\n");
					break;
				case 6: //Buy houses
					if(	board[whose_turn->current_block_id].owner.type == whose_turn->type &&
						board[whose_turn->current_block_id].type == property)
					{
						printf("In %s there is/are %d house/houses\n", board[whose_turn->current_block_id].name, board[whose_turn->current_block_id].house_count);
						printf("Price per house is %d$ for %s\n", board[whose_turn->current_block_id].house_price, board[whose_turn->current_block_id].name);
						printf("You have %d$ in your account\n", whose_turn->account);
						buy_houses(&board[whose_turn->current_block_id], whose_turn);
					}
					else if(board[whose_turn->current_block_id].type != property)
						printf("This is not a property and not available to build houses!\n");
					else
						printf("This is not your property and not available to build houses!\n");
					break;
				case 7: //Sell property
					if(check_properties(*whose_turn)) // If player has properties to sell
						sell_property(board, whose_turn);
					else
						printf("You don't have any properties to sell!\n");
					break;
				default:
					printf("Invalid choice. Select between 1-7!\n");		
					do{c= getchar();}while(c!='\n');
					break;
			}			
		}
		who_waits->turn_to_wait = 1;
		
		if(turn == player1)
			turn = player2;
		else
			turn = player1;
	}	

	printf("\n");
	if(bankrupt == player1)
	{
		printf("Player2 (%s) Won!\n", player_two.name);
		printf("%s's money : %d$\n", player_two.name, player_two.account);
	}
	else
	{
		printf("Player1 (%s) Won!\n", player_one.name);
		printf("%s's money : %d$\n", player_one.name, player_one.account);
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

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define POKE_CH 11
#define POKE_SIZE 10
#define USER_POK_LIM 4
#define EMPTY -1
#define FULL -2
#define AREA_SIZE 8

typedef enum{Charmander, Pikachu, Squirtle, Bulbasaur, 
	Pidgeotto, Ratata, Mug, Caterpie, Zubat, Krabby}pokemon;
typedef enum{linear, quadratic}attack_type;
enum{user_turn, ai_turn};

void menu(void);
void pokedex(char Pokemon_name[][POKE_CH], int range[], attack_type type[], 
	int attack_power[], int stamina[]);
void oaks_laboratory(char Pokemon_name[][POKE_CH], pokemon Pokemons[],
	pokemon *my_Pokemons);
void show_Pokemons(char Pokemon_name[][POKE_CH], pokemon Pokemons[],
	int pokemon_count);
void catch_a_pokemon(char Pokemon_name[][POKE_CH], pokemon Pokemons[],
	pokemon *my_pocket);
int check_pokemon_catch(pokemon *my_pocket, int n, pokemon desired_one);
void release_a_pokemon(char Pokemon_name[][POKE_CH], pokemon Pokemons[],
	pokemon *my_pocket);
int check_pokemon_release(pokemon *my_pocket, int n, pokemon desired_one, int *i);
int empty_slots(pokemon *my_pocket);
void battle(char Pokemon_name[][POKE_CH], int range[], attack_type type[], 
	int attack_power[], int stamina[], pokemon user_Pokemons[]);
int check_pokemon(pokemon *my_pocket, int n, pokemon desired_one);
void show_area (char Pokemon_name[][POKE_CH], int area[][AREA_SIZE], 
	int pokemon_staminas_view[][AREA_SIZE]);
int is_game_over(pokemon user_Pokemons[], pokemon ai_Pokemons[]);
int check_move(int area[][AREA_SIZE], int x, int y, int new_x, int new_y);
int check_in_range_user(int area[][AREA_SIZE], pokemon tra_poke, pokemon targ_poke[],
	int range[], pokemon ai_Pokemons[], int poke_x, int poke_y, attack_type type);
int check_in_range_ai(int area[][AREA_SIZE], pokemon tra_poke, pokemon targ_poke[],
	int range[], pokemon user_Pokemons[], int poke_x, int poke_y, attack_type type);
void find_pokemon(int area[][AREA_SIZE], pokemon desired_one, int *targ_x, int *targ_y);
int find_picked_poke(char pick[], pokemon user_Pokemons[], char Pokemon_name[][POKE_CH]);

int main(void)
{
	srand(time(NULL));
	menu();
	return (0);
}

void menu(void)
{
	int exit=0, selection;
	char c, Pokemon_name[][POKE_CH]={"Charmander", "Pikachu", "Squirtle", "Bulbasaur", 
		"Pidgeotto", "Ratata", "Mug", "Caterpie", "Zubat", "Krabby"};
	int range[]={1,3,4,3,2,2,1,2,3,2};
	int attack_power[]={500,350,300,400,250,250,350,200,350,300};
	int stamina[]={2200,1500,1700,2500,1900,2500,3000,1200,1250,2600};
	attack_type type[]={quadratic,linear,linear,linear,quadratic,linear,quadratic,
		quadratic,linear,linear};
	pokemon Pokemons[]={Charmander, Pikachu, Squirtle, Bulbasaur, Pidgeotto, 
		Ratata, Mug, Caterpie, Zubat, Krabby}, *my_Pokemons, user_Pokemons[USER_POK_LIM];

	for(int i=0; i<USER_POK_LIM; ++i)
		user_Pokemons[i]=EMPTY; /*All elements initialized as -1 to avoid match with 0,
									because 0 is Charmander in enum type*/

	my_Pokemons = user_Pokemons;

	while(!exit)
	{
		printf("\nPlease select an option to continue:\n");
		printf("1) Open Pokedex\n");
		printf("2) Go to Oak’s Laboratory\n");
		printf("3) Enter the tournament\n");
		printf("4) Exit\n");
		
		printf("Your selection: ");
		scanf("%d", &selection);

		switch(selection)
		{
			case 1: pokedex(Pokemon_name, range, type, attack_power, stamina);
					break;
			case 2: oaks_laboratory(Pokemon_name, Pokemons, my_Pokemons);
					break;
			case 3: if(user_Pokemons[0]!=EMPTY)
						battle(Pokemon_name, range, type, attack_power, stamina, user_Pokemons);
					else
						printf("\nYou don't have any pokémon to enter the tournament!\n");
					break;
			case 4: exit=1;
					break;
			default:printf("\nPlease select a valid choice!\n");
					do{c=getchar();} /*Skips all garbage inputs*/
					while(c!='\n');
					break;
		}
	}
}

void pokedex(char Pokemon_name[][POKE_CH], int range[], attack_type type[], 
	int attack_power[], int stamina[])
{
	char pokedex_inp[POKE_CH];
	char *strex = "exit";
	int i, j, match=1, exit=0;
	
	while(!exit)
	{
		printf("\nPlease type name of the Pokémon (type exit to close Pokedex):\n");
		scanf("%s", pokedex_inp);

		for(i=0; i<POKE_SIZE; ++i)
		{
			match=1;
			for(j=0; Pokemon_name[i][j]!='\0' && match; ++j)
			{
				if(pokedex_inp[j]!=Pokemon_name[i][j])
				{
					match=0;
					j=POKE_CH; /*No match, terminate subloop*/
				}
			}
			
			if(match) /*Writes specifications of the desired pokemon*/
			{
				printf("\nName : %s\n", &Pokemon_name[i][0]);
				printf("A. Type: ");
				if(type[i]==linear)
					printf("Linear\n");
				else if(type[i]==quadratic)
					printf("Quadratic\n");
				printf("Range: %d block\n", range[i]);
				printf("A. Power : %d\n", attack_power[i]);
				printf("Stamina : %d\n", stamina[i]);

				i=POKE_SIZE; /* To terminate the loop */
			}
		}
		if(!match) /* Checks if the input is "exit" */
		{
			match=1;
			for(i=0; i<5 && match; ++i)
			{
				if(pokedex_inp[i]!=strex[i])
					match=0; /*No match*/
			}
			if(!match)
				printf("\nNo match!\n");
			else
				exit=1;
		}		
	}
}

void oaks_laboratory(char Pokemon_name[][POKE_CH], pokemon Pokemons[],
	pokemon *my_Pokemons)
{
	int exit=0, selection;
	char c;

	while(!exit)
	{
		printf("\nWelcome to Laboratory of Professor Oak. How can I help you?\n");
		printf("\t1) Show Pokémons\n");
		printf("\t2) Catch a Pokémon\n");
		printf("\t3) Release a Pokémon\n");
		printf("\t4) Show my pocket\n");
		printf("\t5) Back\n");
		
		printf("Your selection: ");
		scanf("%d", &selection);

		switch(selection)
		{
			case 1:	show_Pokemons(Pokemon_name, Pokemons, POKE_SIZE);
					break;
			case 2: if(empty_slots(my_Pokemons)!=FULL)
						catch_a_pokemon(Pokemon_name, Pokemons, my_Pokemons);
					else
						printf("\nYou already catched %d pokémon, your pocket is full!\n", USER_POK_LIM);
					break;
			case 3:	if(empty_slots(my_Pokemons))
						release_a_pokemon(Pokemon_name, Pokemons, my_Pokemons);
					else
						printf("\nYou don't have any pokémon to release!\n");
					break;
			case 4:	if(my_Pokemons[0]!=EMPTY)
					{
						printf("\nYour pocket:");
						show_Pokemons(Pokemon_name, my_Pokemons, USER_POK_LIM);
					}
					else
						printf("\nYour pocket is empty!\n");
					break;
			case 5:	exit=1;
					break;
			default:printf("\nPlease select a valid choice!\n");
					do{c=getchar();} /*Skips all garbage inputs*/
					while(c!='\n');
					break;
		}
	}
}

void show_Pokemons(char Pokemon_name[][POKE_CH], pokemon Pokemons[],
	int pokemon_count)
{
	printf("\n");
	for(int i=0; i<pokemon_count && Pokemons[i]!=EMPTY; ++i)
	{
		printf("%d - %s\n", Pokemons[i], Pokemon_name[Pokemons[i]]);
	}
}

void catch_a_pokemon(char Pokemon_name[][POKE_CH], pokemon Pokemons[],
	pokemon *my_pocket)
{
	int index, selection;
	char c;

	printf("\nPlease select a pokémon below to catch!");
	show_Pokemons(Pokemon_name, Pokemons, POKE_SIZE);
	printf("Your selection: ");

	if(scanf("%d", &selection)==1 && selection>=0 && selection<POKE_SIZE)
	{
		index = empty_slots(my_pocket);
		if(!check_pokemon_catch(my_pocket, USER_POK_LIM, selection))
			my_pocket[index]=selection; /*Catch the selected pokémon*/
		else
			printf("\nYou already catched that pokémon!\n");		
	}
	else
	{
		printf("\nInvalid input!\n");
		do{c=getchar();} /*Skips all garbage inputs*/
		while(c!='\n');
	}
}

/*Checks if the desired pokemon already catched or not*/
int check_pokemon_catch(pokemon *my_pocket, int n, pokemon desired_one)
{
	int i, error=0;

	for(i=0; i<n && my_pocket[i]!=EMPTY && !error; ++i)
	{
		if(my_pocket[i]==desired_one) /*If already catched*/
			error=1;
	}

	return error;
}

void release_a_pokemon(char Pokemon_name[][POKE_CH], pokemon Pokemons[],
	pokemon *my_pocket)
{
	int index, selection;
	char c;

	printf("\nPlease select a pokémon below to release:");
	show_Pokemons(Pokemon_name, my_pocket, USER_POK_LIM);
	printf("Your selection: ");
	
	if(scanf("%d", &selection)==1 && selection>=0 && selection<POKE_SIZE)
	{	
		if(!check_pokemon_release(my_pocket, USER_POK_LIM, selection, &index))
		{
			if(index<USER_POK_LIM-1) /*Not at last slot of the pocket*/
			{
				/*Release the pokémon and shift the rest in the pocket*/
				while(index<USER_POK_LIM-1 && my_pocket[index]!=EMPTY)
				{
					my_pocket[index]=my_pocket[index+1];
					index++;	
				}
				my_pocket[index]=EMPTY;
			}
			else /*Desired pokémon is at last slot of the pocket*/
				my_pocket[index]=EMPTY;
		}
		else
			printf("\nYou don't have that pokémon!\n");
	}
	else
	{
		printf("\nInvalid input!\n");
		/*Skips all garbage inputs*/
		do{c=getchar();} 
		while(c!='\n');
	}
}

/*Checks if the user has the pokémon that he/she wants to release and finds its index*/
int check_pokemon_release(pokemon *my_pocket, int n, pokemon desired_one, int *index)
{
	int i, error=1;

	for(i=0; i<n && my_pocket[i]!=EMPTY && error; ++i)
	{
		if(my_pocket[i]==desired_one) /*User has that pokémon*/
		{
			error=0;
			--i; /*To avoid increment before terminating the loop*/
		}	 
	}
	*index=i; /*Where is that pokémon in the pocket*/

	return error;
}

/*Returns from which index pocket has empty slots*/
int empty_slots(pokemon *my_pocket)
{
	int empty_index;
	for(int i=0; i<USER_POK_LIM; ++i)
	{
		if(my_pocket[i]==EMPTY)
		{
			empty_index=i;
			i=USER_POK_LIM; /*Termiante loop*/
		}
		else if(i==USER_POK_LIM-1)
			empty_index=FULL;
	}

	return empty_index;
}

void battle(char Pokemon_name[][POKE_CH], int range[], attack_type type[], 
	int attack_power[], int stamina[], pokemon user_Pokemons[])
{
	int area[AREA_SIZE][AREA_SIZE], pokemon_staminas_view[AREA_SIZE][AREA_SIZE], i, j, control=0;
	int x_coord, y_coord, ai_x, ai_y, new_x, new_y, targ_x, targ_y, turn=user_turn, index, aindex, r;
	pokemon ai_Pokemons[USER_POK_LIM], rand_poke, targ_poke[20];
	char pick[4], c;

	/*Initialize the area as empty*/
	for(i=0; i<AREA_SIZE; ++i)
	{
		for(j=0; j<AREA_SIZE; ++j)
			area[i][j]=EMPTY;
	}

	/*Initialize target array as empty*/
	for(i=0; i<20; ++i)
		targ_poke[i]=EMPTY;

	printf("\nPrepare for battle!\n");
	show_area(Pokemon_name, area, pokemon_staminas_view);
	printf("Locate your pokémons on first two row.\n");
	printf("(Type coordinates x,y respectively");
	printf(", 0<=x<%d, 0<=y<2)\n", AREA_SIZE);

	/*User locates his/her pokémons*/
	for(i=0; i<USER_POK_LIM && user_Pokemons[i]!=EMPTY; ++i)
	{
		printf("\nLocate %s: ", Pokemon_name[user_Pokemons[i]]);
		scanf("%d%d", &x_coord, &y_coord);
		while(x_coord<0 || x_coord>=AREA_SIZE || y_coord<0 || y_coord>=2)
		{
			printf("Please locate your pokémon on first two row!");
			printf(" (0<=x<%d, 0<=y<2)\n", AREA_SIZE);
			printf("\nLocate %s: ", Pokemon_name[user_Pokemons[i]]);
			scanf("%d%d", &x_coord, &y_coord);
		}
		area[y_coord][x_coord]=user_Pokemons[i];
		pokemon_staminas_view[y_coord][x_coord]=stamina[user_Pokemons[i]];
		show_area(Pokemon_name, area, pokemon_staminas_view);
	}

	/*AI picks and locates its pokémons randomly*/
	for(i=0; i<USER_POK_LIM; ++i)
	{
		/*Picks a random pokémon*/
		do{
			rand_poke=rand()%POKE_SIZE;
		}
		while(check_pokemon(ai_Pokemons, USER_POK_LIM, rand_poke));
		ai_Pokemons[i]=rand_poke;

		/*Picks a random location*/
		do{
			ai_x=rand()%AREA_SIZE;
			ai_y=rand()%AREA_SIZE;	
		}
		while(ai_y<6 || area[ai_y][ai_x]!=EMPTY); /*It must be on the last two row*/
		
		area[ai_y][ai_x]=rand_poke;
		pokemon_staminas_view[ai_y][ai_x]=stamina[ai_Pokemons[i]];
	}

	printf("\n///////////////////////  BATTLE BEGINS  //////////////////////////\n");
	printf("Note: To pick a pokémon, just type the first three characters of their names.\n");
	show_area(Pokemon_name, area, pokemon_staminas_view);

	/*The battle*/
	while(!is_game_over(user_Pokemons, ai_Pokemons))
	{
		if(turn==user_turn) /*User plays*/
		{
			do{
				printf("Pick Pokemon: ");
				scanf("%s", pick);
				index = find_picked_poke(pick, user_Pokemons, Pokemon_name);
				if(index==EMPTY)
				{
					do{c=getchar();} /*Skips all garbage inputs*/
					while(c!='\n');
					printf("Invalid choice, please try again!\n");	
				}
			}while(index==EMPTY);

			do{
				printf("Where to go? (type coord x,y): ");
				if(scanf("%d%d", &new_x, &new_y)!=2)
				{
					do{c=getchar();} /*Skips all garbage inputs*/
					while(c!='\n');
					control=1;
				}
				else
				{
					control=check_move(area, x_coord, y_coord, new_x, new_y);
					if(control)
						printf("Invalid coordinates, please try again!\n");
				}
			}while(control);
			
			area[y_coord][x_coord]=EMPTY;
			area[new_y][new_x]=user_Pokemons[index];
			pokemon_staminas_view[new_y][new_x]=stamina[user_Pokemons[index]];
			x_coord = new_x;
			y_coord = new_y;

			if(type[user_Pokemons[index]]==linear) /*Linear pokemon*/
			{
				if(check_in_range_user(area, user_Pokemons[index], targ_poke, range, ai_Pokemons, x_coord, y_coord, linear))
				{
					for(i=0; targ_poke[i]!=EMPTY; ++i)
					{
						stamina[ai_Pokemons[targ_poke[i]]-=attack_power[user_Pokemons[index]]];
						//printf("Stamina[%d]=%d\n", ai_Pokemons[targ_poke[i]], stamina[ai_Pokemons[targ_poke[i]]]);
						find_pokemon(area, ai_Pokemons[targ_poke[i]], &targ_x, &targ_y);
						if(stamina[ai_Pokemons[targ_poke[i]]]<=0)
						{
							area[targ_y][targ_x]=EMPTY;
							check_pokemon_release(ai_Pokemons, USER_POK_LIM, targ_poke[i], &index);
							ai_Pokemons[index]=EMPTY;
						}
						else
							pokemon_staminas_view[targ_y][targ_x]=stamina[ai_Pokemons[targ_poke[i]]];
					}
				}
			}
			else if(check_in_range_user(area, user_Pokemons[index], targ_poke, range, ai_Pokemons, x_coord, y_coord, quadratic))
			{	
				for(i=0; targ_poke[i]!=EMPTY; ++i)
				{
					stamina[ai_Pokemons[targ_poke[i]]-=attack_power[user_Pokemons[index]]];
					find_pokemon(area, targ_poke[i], &targ_x, &targ_y);
					if(stamina[ai_Pokemons[targ_poke[i]]]<=0)
					{
						area[targ_y][targ_x]=EMPTY;
						check_pokemon_release(ai_Pokemons, USER_POK_LIM, targ_poke[i], &index);
						ai_Pokemons[index]=EMPTY;
					}
					else
						pokemon_staminas_view[targ_y][targ_x]=stamina[ai_Pokemons[targ_poke[i]]];
				}	
			}

			show_area(Pokemon_name, area, pokemon_staminas_view);
			turn=ai_turn;
		}
		else /*AI plays*/
		{
			printf("\nAI's turn...");

			r = rand()%USER_POK_LIM;
			index = ai_Pokemons[r];
			find_pokemon(area, index, &ai_x, &ai_y);

			do{
				new_x = rand()%AREA_SIZE;
				new_y = rand()%AREA_SIZE;	
			}while(check_move(area, ai_x, ai_y, new_x, new_y));
			
			area[ai_y][ai_x]=EMPTY;
			check_pokemon_release(ai_Pokemons, USER_POK_LIM, index, &aindex);
			area[new_y][new_x]=ai_Pokemons[aindex];
			pokemon_staminas_view[new_y][new_x]=stamina[index];
			ai_x = new_x;
			ai_y = new_y;

			if(type[index]==linear) /*Linear pokemon*/
			{
				if(check_in_range_ai(area, index, targ_poke, range, user_Pokemons, ai_x, ai_y, linear))
				{
					for(i=0; targ_poke[i]!=EMPTY; ++i)
					{
						stamina[targ_poke[i]]-=attack_power[index];
						//printf("Stamina[%d]=%d\n", ai_Pokemons[targ_poke[i]], stamina[ai_Pokemons[targ_poke[i]]]);
						find_pokemon(area, user_Pokemons[targ_poke[i]], &targ_x, &targ_y);
						if(stamina[user_Pokemons[targ_poke[i]]]<=0)
						{
							area[targ_y][targ_x]=EMPTY;
							check_pokemon_release(user_Pokemons, USER_POK_LIM, targ_poke[i], &index);
							user_Pokemons[index]=EMPTY;
						}
						else
							pokemon_staminas_view[targ_y][targ_x]=stamina[targ_poke[i]];
					}
				}
			}
			else if(check_in_range_ai(area, index, targ_poke, range, user_Pokemons, ai_x, ai_y, quadratic))
			{	
				for(i=0; targ_poke[i]!=EMPTY; ++i)
				{
					stamina[targ_poke[i]]-=attack_power[index];
					find_pokemon(area, targ_poke[i], &targ_x, &targ_y);
					if(stamina[targ_poke[i]]<=0)
					{
						area[targ_y][targ_x]=EMPTY;
						check_pokemon_release(user_Pokemons, USER_POK_LIM, targ_poke[i], &index);
						user_Pokemons[index]=EMPTY;
					}
					else
						pokemon_staminas_view[targ_y][targ_x]=stamina[targ_poke[i]];
				}	
			}
			show_area(Pokemon_name, area, pokemon_staminas_view);
			turn=user_turn;
		}
	}

	if(is_game_over(user_Pokemons, ai_Pokemons)==1)
		printf("You won, Congratz!!!\n");
	else
		printf("AI won, what a loser!\n");

}

/*Just checks if the desired pokemon is in the pocket or not*/
int check_pokemon(pokemon *my_pocket, int n, pokemon desired_one)
{
	int i, error=0;

	for(i=0; i<n && my_pocket[i]!=-1 && !error; ++i)
	{
		if(my_pocket[i]==desired_one) /*User has that pokémon*/
			error=1; 
	}

	return error;
}

void show_area (char Pokemon_name[][POKE_CH], int area[][AREA_SIZE], 
	int pokemon_staminas_view[][AREA_SIZE])
{
	int i, j, k, t, w, row;

	/*Draw the area*/
	printf("\n");
	for(i=0, k=0, row=0; i<=AREA_SIZE*3; ++i, ++row) /*Rows*/
	{
		if(row==3)
			row=0;
		
		if(row==0) /*Draw a horizontal line at each 3 rows*/
		{
			for(w=0; w<=AREA_SIZE*7; ++w)
				printf("-");
			if(i)
				k++; /*Next row in 8x8 area*/
		}
		else
		{
			printf("|");
			for(j=0, t=0; j<AREA_SIZE*7; j+=7, t++) /*Columns*/
			{
				if(area[k][t]!=EMPTY)
				{
					if(row==1)
						printf("%2c%c%c  ", Pokemon_name[area[k][t]][0], Pokemon_name[area[k][t]][1], Pokemon_name[area[k][t]][2]);
					else if(row==2)
						printf("(%4d)", pokemon_staminas_view[k][t]);
				}
				else
					printf("      ");
				printf("|");
			}	
		}
		printf("\n");	
	}
}

int is_game_over(pokemon user_Pokemons[], pokemon ai_Pokemons[])
{
	int i, game_over, user=0, ai=0;

	/*Is user lose?*/
	for(i=0; i<USER_POK_LIM; ++i)
	{
		if(user_Pokemons[i]!=EMPTY)
		{
			user=1;
			i=USER_POK_LIM;
		}
	}
	/*Is ai lose?*/
	for(i=0; i<USER_POK_LIM; ++i)
	{
		if(ai_Pokemons[i]!=EMPTY)
		{
			ai=1;
			i=USER_POK_LIM;
		}
	}

	if(user && ai)
		game_over=0; /*Game goes on*/
	else if(user && !ai)
		game_over=1; /*User wins*/
	else 
		game_over=2; /*AI wins*/

	return game_over;
}

/*Just finds whick pokemon user picked*/
int find_picked_poke(char pick[], pokemon user_Pokemons[], char Pokemon_name[][POKE_CH])
{
	int index, found=0, i;
	for(i=0; i<USER_POK_LIM && !found; ++i)
	{
		/*Checks if the first two letter matches*/
		if(Pokemon_name[user_Pokemons[i]][0]==pick[0] && Pokemon_name[user_Pokemons[i]][1]==pick[1] && Pokemon_name[user_Pokemons[i]][2]==pick[2])
			found=1;
	}

	if(!found)
		index=EMPTY;
	else
		index=i-1;
	
	return index;
}

int check_move(int area[][AREA_SIZE], int x, int y, int new_x, int new_y)
{
	int error=0;

	if(area[new_x][new_y]!=EMPTY)
		error=1;
	else
	{
		if(new_x==x && (new_y-y>2 || new_y-y<-2)) /*Vertical limit*/
			error=1;
		else if(new_y==y && (new_x-x>2 || new_x-x<-2)) /*Horizontal limit*/
			error=1;
		else if(new_x!=x && new_y!=y) /*Can't move diagonal*/
			error=1;
	}
	return error;
}

int check_in_range_user(int area[][AREA_SIZE], pokemon tra_poke, pokemon targ_poke[],
	int range[], pokemon ai_Pokemons[], int poke_x, int poke_y, attack_type type)
{
	int attack=0;
	for(int i=0; i<USER_POK_LIM; ++i)
	{
		for(int j=1; j<=range[tra_poke]; ++j)
		{
			if(type=linear && !attack)
			{
				if(area[poke_y+j][poke_x]==ai_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y+j][poke_x];
					attack+=1;
				}
				if(area[poke_y-j][poke_x]==ai_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y-j][poke_x];
					attack+=1;
				}
				if(area[poke_y][poke_x+j]==ai_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y][poke_x+j];
					attack+=1;
				}
				if(area[poke_y][poke_x-j]==ai_Pokemons[i])
					targ_poke[attack]=area[poke_y][poke_x-j];
			}
			else if(type==quadratic)
			{
				if(area[poke_y+j][poke_x]==ai_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y+j][poke_x];
					attack+=1;
				}
				if(area[poke_y-j][poke_x]==ai_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y-j][poke_x];
					attack+=1;
				}
				if(area[poke_y][poke_x+j]==ai_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y][poke_x+j];
					attack+=1;
				}
				if(area[poke_y][poke_x-j]==ai_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y][poke_x-j];
					attack+=1;
				}
				if(area[poke_y+j][poke_x+j]==ai_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y+j][poke_x+j];
					attack+=1;
				}
				if(area[poke_y+j][poke_x-j]==ai_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y+j][poke_x-j];
					attack+=1;
				}
				if(area[poke_y-j][poke_x+j]==ai_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y-j][poke_x+j];
					attack+=1;
				}
				if(area[poke_y-j][poke_x-j]==ai_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y-j][poke_x-j];
					attack+=1;
				}
			}
		}
	}
	return attack;
}

int check_in_range_ai(int area[][AREA_SIZE], pokemon tra_poke, pokemon targ_poke[],
	int range[], pokemon user_Pokemons[], int poke_x, int poke_y, attack_type type)
{
	int attack=0;
	for(int i=0; i<USER_POK_LIM; ++i)
	{
		for(int j=1; j<=range[tra_poke]; ++j)
		{
			if(type=linear && !attack)
			{
				if(area[poke_y+j][poke_x]==user_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y+j][poke_x];
					attack+=1;
				}
				if(area[poke_y-j][poke_x]==user_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y-j][poke_x];
					attack+=1;
				}
				if(area[poke_y][poke_x+j]==user_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y][poke_x+j];
					attack+=1;
				}
				if(area[poke_y][poke_x-j]==user_Pokemons[i])
					targ_poke[attack]=area[poke_y][poke_x-j];
			}
			else if(type==quadratic)
			{
				if(area[poke_y+j][poke_x]==user_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y+j][poke_x];
					attack+=1;
				}
				if(area[poke_y-j][poke_x]==user_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y-j][poke_x];
					attack+=1;
				}
				if(area[poke_y][poke_x+j]==user_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y][poke_x+j];
					attack+=1;
				}
				if(area[poke_y][poke_x-j]==user_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y][poke_x-j];
					attack+=1;
				}
				if(area[poke_y+j][poke_x+j]==user_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y+j][poke_x+j];
					attack+=1;
				}
				if(area[poke_y+j][poke_x-j]==user_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y+j][poke_x-j];
					attack+=1;
				}
				if(area[poke_y-j][poke_x+j]==user_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y-j][poke_x+j];
					attack+=1;
				}
				if(area[poke_y-j][poke_x-j]==user_Pokemons[i])
				{
					targ_poke[attack]=area[poke_y-j][poke_x-j];
					attack+=1;
				}
			}
		}
	}
	return attack;
}

/*Finds desired pokemon's coordinates on the battle area*/
void find_pokemon(int area[][AREA_SIZE], pokemon desired_one, int *targ_x, int *targ_y)
{
	for(int i=0; i<AREA_SIZE; ++i)
	{
		for(int j=0; j<AREA_SIZE; ++j)
		{
			if(area[i][j]==desired_one)
			{
				*targ_x=j;
				*targ_y=i;
			}
		}
	}
}
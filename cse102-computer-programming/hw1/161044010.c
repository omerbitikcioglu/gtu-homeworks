#include <stdio.h>
#include <math.h>
#include <time.h>
#include <stdlib.h>
#define PI 3
#define RECTENGULAR 1
#define CIRCULAR 2
#define SQUARE 3
#define RED 0
#define YELLOW 1
#define BLUE 2
#define BLACK 3
#define WHITE 4

double CreateBody (int shape);
int SetColor (int color);
double LoadMoves (int shape, double body_size);
int SetAttackPower (int lower_bound, int upper_bound);
void ShowPokemon (int shape, double body_size, int color, double move_length, int attack_power);
void ShowColor (int color);

int main ()
{
    int shape, color, attack_power;
    double size_of_body, move_length;
    shape = CIRCULAR; /*RECTENGULAR / CIRCULAR / SQUARE Pick one of them*/
    size_of_body = CreateBody (shape);
    color = 798;
    color = SetColor (color);
    move_length = LoadMoves (shape, size_of_body);
    attack_power = SetAttackPower (0, 150);
    ShowPokemon (shape, size_of_body, color, move_length, attack_power);
}

double CreateBody (int shape)
{
    int ver_len, hor_len, radius, side;
    double area;

    switch(shape)
    {
        case RECTENGULAR:
            printf("Enter vertical and horizontal lengths respectively...");
            scanf("%d %d", &ver_len, &hor_len);
            area = ver_len * hor_len; /*Area of a rectangle*/
            break;

        case CIRCULAR:
            printf("Enter the radius value...");
            scanf("%d", &radius);
            area = PI * radius * radius; /*Area of a circle*/
            break;

        case SQUARE:
            printf("Enter the side length...");
            scanf("%d", &side);
            area = side * side; /*Area of a square*/
            break;
    }

    return area; /*The function returns the area of the shape*/
}

int SetColor (int color)
{
    if(color > 0 && color < 1000)
        color %= 5;
    else
        color = 1;

    return color; /*The function returns the revised color*/
}

double LoadMoves(int shape, double body_size)
{
    int ver_len = 5, hor_len, radius, side;
    double move;

    switch(shape)
    {
        case RECTENGULAR:
            hor_len = body_size / ver_len;
            move = 2 * (ver_len + hor_len); /*Perimeter of a rectangular*/
            break;

        case CIRCULAR:
            radius = sqrt(body_size / PI);
            move = 2 * PI * radius; /*Perimeter of a circle*/
            break;

        case SQUARE:
            side = sqrt(body_size);
            move = 4 * side; /*Perimeter of a square*/
            break;
    }

    return move; /*Returns the perimeter of the shape as length of one move.*/
}

int SetAttackPower (int lower_bound, int upper_bound)
{
    int power;
    srand(time(NULL));

    /*This for loop finds a proper attack value between the given lower and upper bounds*/
    for(power = rand() % upper_bound;
        power <= lower_bound;
        power = rand() % upper_bound);

    return power;
}

void ShowPokemon (int shape, double body_size, int color, double move_length, int attack_power)
{
    int ver_len = 5, hor_len, radius, side, i, j, tab, cross;

    switch(shape)
    {
        case RECTENGULAR:
            hor_len = body_size / ver_len; /*Finds the horizontal length by assuming that vertical length is 5*/
            for(i=0; i<ver_len; ++i)
            {
                for(j=0; j<hor_len; ++j)
                    printf("X");
                printf("\n");
            }
            printf("Name: Rectangular Pokemon\n");
            break;

        case CIRCULAR:
            radius = sqrt(body_size / PI); /*Finds the value of radius by using body size*/

            if(radius%2 == 0) /*Even radius value*/
            {
                tab = 1;
                /*First half of the circle*/
                for(i=0; i< radius/2; ++i) /*Rows*/
                {
                    if(i==0)
                    {
                        for(j=0; j< ((radius-1)/2); ++j) /*First tabs*/
                            printf("\t");
                        printf("%5c\n",'X'); /*Aligns the first X*/
                    }
                    else
                    {
                        for(j=i; j< radius/2; ++j) /*First tabs*/
                            printf("\t");
                        for(cross=0; cross<2; ++cross)
                        {
                            printf("X");
                            for(j=1; j<= tab && cross!=1; ++j) /*Tabs between two X*/
                                printf("\t");
                        }
                        tab += 2;
                        printf("\n");
                    }
                }

                /*Center of the circle*/
                tab = radius-1;
                for(cross=0; cross<2; ++cross)
                {
                    printf("X");
                    for(j=1; j<= tab && cross!=1; ++j) /*Tabs between two X*/
                        printf("\t");
                }
                printf("\n");

                tab -=2;
                /*Last half of the circle*/
                for(i= radius/2-1; i>=0; --i) /*Rows*/
                {
                    if(i==0) /*First line only one X*/
                    {
                        for(j=0; j< ((radius-1)/2); ++j) /*First tabs*/
                            printf("\t");
                        printf("%5c\n",'X'); /*Aligns the last X*/
                    }
                    else
                    {
                        for(j=i; j< radius/2; ++j) /*First tabs*/
                            printf("\t");
                        for(cross=0; cross<2; ++cross)
                        {
                            printf("X");
                            for(j=1; j<= tab && cross!=1; ++j) /*Tabs between two X*/
                                printf("\t");
                        }
                        tab -= 2;
                        printf("\n");
                    }
                }
            }
            else /*Odd radius value*/
            {
                tab = 2;
                if(radius == 1)
                    printf("X\n");
                else
                {
                    /*First half of the circle*/
                    for(i=0; i< radius/2; ++i) /*Rows*/
                    {
                        for(j=i; j< radius/2; ++j) /*First tabs*/
                            printf("\t");
                        if(i==0) /*First line only one X*/
                            printf("X");
                        else
                        {
                            for(cross=0; cross<2; ++cross)
                            {
                                printf("X");
                                for(j=1; j<= tab && cross!=1; ++j) /*Tabs between two X*/
                                    printf("\t");
                            }
                            tab += 2;
                        }
                        printf("\n");
                    }

                    /*Center of the circle*/
                    tab = radius-1;
                    for(cross=0; cross<2; ++cross)
                    {
                        printf("X");
                        for(j=1; j<= tab && cross!=1; ++j) /*Tabs between two X*/
                            printf("\t");
                    }
                    printf("\n");

                    tab -=2;
                    /*Last half of the circle*/
                    for(i= radius/2-1; i>=0; --i) /*Rows*/
                    {
                        for(j=i; j< radius/2; ++j) /*First tabs*/
                            printf("\t");
                        if(i==0) /*Last line only one X*/
                            printf("X");
                        else
                        {
                            for(cross=0; cross<2; ++cross)
                            {
                                printf("X");
                                for(j=1; j<= tab && cross!=1; ++j) /*Tabs between two X*/
                                    printf("\t");
                            }
                            tab -= 2;
                        }
                        printf("\n");
                    }
                }
            }
            printf("Name: Circular Pokemon\n");
            break;

        case SQUARE:
            side = sqrt(body_size); /*Square root of area of a square gives one side's length*/
            for(i=0; i<side; ++i)
            {
                for(j=0; j<side; ++j)
                    printf("X");
                printf("\n");
            }
            printf("Name: Square Pokemon\n");
            break;
    }
    printf("Size: %.2f\n", body_size);
    ShowColor(color);
    printf("Move: %.2f\n", move_length);
    printf("Attack Power: %d\n", attack_power);
}

void ShowColor (int color)
{
    switch(color)
    {
        case RED:       printf("Color: Red\n");
                        break;
        case YELLOW:    printf("Color: Yellow\n");
                        break;
        case BLUE:      printf("Color: Blue\n");
                        break;
        case BLACK:     printf("Color: Black\n");
                        break;
        case WHITE:     printf("Color: White\n");
                        break;
    }
}

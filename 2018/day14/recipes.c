#include <stdlib.h>
#include <stdio.h>

typedef int bool;
#define true 1
#define false 0

// checks a match between the number and the last recipes of the array
// recursive for the length of the number
bool check(int* recipes, int num, int last)
{
	bool match = false;

	// avoids segfault
	if(last >= 0)
	{
		// if it is 0 all the digits have matched
		if(num != 0)
		{
			if(recipes[last] == num%10)
			{
				// checks the next digit
				match = check(recipes, num/10, last-1);
			}
		}
		else
		{
			match = true;
		}
	}

	return match;
}

// creates a new recipe and checks if there is a match between the input and
// the last recipes
bool createRecipe(int* recipes, int* workers, int* last, int num)
{
	int newMix = recipes[workers[0]] + recipes[workers[1]];
	bool match = false;
	
	// if there are two digits
	if(newMix >= 10)
	{
		// adds the first digit to the array and updates the last pos
		recipes[++*last] = newMix/10;
		match = check(recipes, num, *last);
		// leaves last digit
		newMix %= 10;
	}

	// so that it does not add/check twice for a match
	if(!match)
	{
		recipes[++*last] = newMix;
		match = check(recipes, num, *last);
	}

	return match;
}

// moves the worker to the next recipe
void moveWorkers(int* recipes, int* workers, int last)
{
	for(int i=0; i<2; i++)
	{
		// loops around
		workers[i] = (workers[i] + recipes[workers[i]] + 1) % (last+1);
	}
}

// prints the n last elements of the array
void printLast(int n, int last, int* recipes)
{
	for(int i=last-n; i<=last; i++) printf("%d", recipes[i]);
	printf("\n");
}

// prints the nine recipes after the given number of recipes
void nine_after(int* last, int recipeNum, int* recipes, int* workers)
{
	// calculates the number of recipes plus nine
	while(*last < recipeNum+9)
	{
		createRecipe(recipes, workers, last, input);
		moveWorkers(recipes, workers, *last);
	}

	printLast(9, *last, recipes);
}

// prints the number of recipes before the input is matched with the last
// recipes
int match_num(int* last, int input, int* recipes, int* workers)
{
	bool match = false;
	int numLength = 5;

	while(!match)
	{
		match = createRecipe(recipes, workers, last, input);
		moveWorkers(recipes, workers, *last);
	}

	printf("%d\n", *last-numLength);
}

int main()
{
	// for faster iterations
	int* recipes = malloc(100000000*sizeof(int));

	// start parameters
	recipes[0] = 3;
	recipes[1] = 7;
	// position of the last element
	int last = 1;
	int workers[] = {0, 1};

	// number of recipes
	int input = 704321;

	nine_after(&last, input, recipes, workers);

	match_num(&last, input, recipes, workers);

	return 0;
}
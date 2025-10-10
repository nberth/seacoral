// The maze demo is taken from Felipe Andres Manzano's blog:
// http://feliam.wordpress.com/2010/10/07/the-symbolic-maze/
//
//

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdbool.h>

#define ITERS 80

#define H 13
#define W 17

char maze[H][W]={
"+-+-------------+",
"| |             |", 
"| | +-----* *---+",
"|   |           |",
"+---+-* *-------+",
"|               |",
"+ +-------------+",
"| |       |   |#|",
"| | *---+ * * * |",
"| |     |   |   |",
"| +---* +-------+",
"|               |",
"+---------------+",};

/*
void draw ()
{
	int i, j;
	for (i = 0; i < H; i++)
	  {
		  for (j = 0; j < W; j++)
				  printf ("%c", maze[i][j]);
		  printf ("\n");
	  }
	printf ("\n");
}
*/

int solve (char program[80])
{
	int x, y;     //Player position
	int ox, oy;   //Old player position
	int i = 0;    //Iteration number
	x = 1;
	y = 1;
	maze[y][x]='X';
	// draw();
	while(i < ITERS)
	{
		ox = x;    //Save old player position
		oy = y;
		switch (program[i])
		{
			case 'w':
				y--;
				break;
			case 's':
				y++;
				break;
			case 'a':
				x--;
				break;
			case 'd':
				x++;
				break;
			default:
			  // printf("Wrong command!(only w,s,a,d accepted!)\n");
			  // printf("You lose!\n");
			  // exit(-1);
			  return -1;
		}
		if (maze[y][x] == '#')
		{
		  // printf("You win!\n");
		  // exit(0);
		  return 0;
		}
		if (maze[y][x] != ' ') {
		  x = ox;
		  y = oy;
		}
		if (ox==x && oy==y){
		  // printf("You lose\n");
		  // exit(-2);
		  return -2;
		}

		maze[y][x]='X';
		// draw ();          //draw it
		i++;
		// sleep(1);
	}
	// printf("You lose\n");
	return -2;
}

/*
int main() {
  char *program="ssssddddwwaawwddddssssddwwww";
  solve(program);
}
*/

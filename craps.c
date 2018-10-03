#include <ctype.h>
#include <stdbool.h>   /* C99 only */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int roll_dice(void);
bool play_game(void);

int main(void)
{
  char again;

  srand((unsigned) time(NULL));

  do {
    if (play_game()) {
      printf("You win!\n\n");
    } else {
      printf("You lose!\n\n");
    }

    printf("Play again? ");
    scanf(" %c", &again);

    printf("\n");   /* blank line */
  } while (tolower(again) == 'y');

  return 0;
}

int roll_dice(void)
{
  return (rand() % 6 + 1) + (rand() % 6 + 1);
}

bool play_game(void)
{
  // Program your game here. You can use print statements to display die rolls
}

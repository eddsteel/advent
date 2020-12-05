#include <stdio.h>
#include <stdlib.h>

unsigned int * load_input(FILE *f, unsigned int* num, unsigned int lines) {
  unsigned int i;
  unsigned int j;
  int c;

  for (i = 0; i < lines; i++) {
    if (fscanf(f, "%d\n", &j) != 1) {
      printf("Failed to read integer from input.\n");
      exit(2);
    }
    num[i] = j;
  }

  return num;
}

unsigned int answer1(unsigned int* num, unsigned int l) {
  unsigned int i;
  unsigned int j;

  for (i = 0; i < l; i++) {
    for (j = 1; j < l; j++) {
      if (j >= i)
        continue;
      if (*(num + i) + *(num + j) == 2020) {
        return *(num + i) * *(num + j);
      }
    }
  }
  return 0;
}

unsigned int answer2(unsigned int* num, unsigned int l) {
  unsigned int i;
  unsigned int j;
  unsigned int k;

  for (i = 0; i < l; i++) {
    for (j = 1; j < l; j++) {
      for (k = 2; k < l; k++) {
        if (j >= i || k >= j)
          continue;
        if (*(num + i) + *(num + j) + *(num + k) == 2020) {
          return *(num + i) * *(num + j) * *(num + k);
        }
      }
    }
  }
  return 0;
}

void stars(FILE * f, unsigned int l) {
  unsigned int num[l];
  load_input(f, num, l);
  printf("answer 1: %d\n", answer1(num, l));
  printf("answer 2: %d\n", answer2(num, l));
}

int main() {
  FILE *f;
  unsigned int l = 0; // line count
  int c;

  if ((f = fopen("./input/1", "r")) == NULL) {
    printf("Failed to read input.\n");
    exit(1);
  }
  while ((c = fgetc(f)) != EOF) {
    if (c == '\n')
      ++l;
  }
  rewind(f);
  stars(f, l);
  fclose(f);

  return 0;
}

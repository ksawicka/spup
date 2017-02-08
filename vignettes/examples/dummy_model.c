/*
   Simple model in C for testing error propagation in R
   Dennis Walvoort, July 2016
*/


#include <stdio.h>

int main() {

  FILE *fp;
  double x[9] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0};
  double y;
  double b0;
  double b1;
  int i;

  fp = fopen("input.txt", "r");
  if (fp == NULL) {
    printf("Can't read input.txt\n");
    return 1;
  }
  fscanf(fp, "%lf %lf\n", &b0, &b1);
  fclose(fp);

  fp = fopen("output.txt", "w");
  if (fp == NULL) {
    printf("Can't create output.txt\n");
    return 1;
  }
  else {
    for (i=0; i<9; i++) {
      y = b0 + b1 * x[i];
      fprintf(fp, "%10.2f\n", y);
    }
    fclose(fp);
  }

  return 0;
}

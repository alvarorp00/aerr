#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include <stdint.h>

bool f1(int_least64_t x, int_least64_t y) {
  return x - y > 0;
}

bool f2(int_least64_t x, int_least64_t y) {
  return x > y;
}

int_least64_t generate_random();

const int_least64_t NUM_TESTS = 1000;

int main(void) {
  srand(time(NULL));
  
  for (int_least64_t i = 0; i < NUM_TESTS; i++) {
    int_least64_t x = generate_random();
    int_least64_t y = generate_random();
    
    printf("x = %15ld, y = %15ld, x - y = %15ld\n", x, y, x - y);
        
    assert(f1(x, y) == f2(x, y));
  }
  
  return 0;
}

int_least64_t generate_random() {
  int_least64_t sign = rand() % 2;
  if (sign == 0) sign = -1;
  return rand() * sign;
}
#include <stdio.h>
#include <stdlib.h>

int compute(int x, int y, int z){
    7 + (x = (y = 3 * 5) % 9);
    z = x * y;
    z = 3;

    printf("X: %d | Y: %d | Z: %d\n", x, y, z);
}

int main(){
    int x = 0, y = 0, z = 0;
    compute(x, y, z);
}
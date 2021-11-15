#include <stdio.h>
#include <stdlib.h>

int compute(int x, int y, int z){
    y+5*x-2+z*3;
    x=5;
    y=6;
    x=(3+5)-8*(10/2);
    y=x*x-(12*12);
    z=z/z+(+-+-+-+-z-z)+(x*z)%z+(y+z)*0-x*y;
    x=(-y*-y-(y*y-4*x*z))/(2*x*2*x);

    printf("X: %d | Y: %d | Z: %d\n", x, y, z);
}

int main(){
    int x = 2, y = 3, z = 5;
    compute(x, y, z);
}
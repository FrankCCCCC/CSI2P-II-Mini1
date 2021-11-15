#include "../function.h"

Result compute(int x, int y, int z){
    z=x+5;
    y=z/10-7*x;
    -y-(+z)%(z+100);
    z =(x++) + (y--);
    x=(--y)*(++z);
    x=z-+-+-+-++y;
        ;       
    x=y=z=3+5;

    Result res = {x, y, z};
    return res;
}
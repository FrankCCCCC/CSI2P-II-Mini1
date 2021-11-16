#include "../function.h"

Result compute(int x, int y, int z){
    y = x++ - x--;
    z = x++ + x++;

    Result res = {x, y, z};
    return res;
}
#include "../function.h"

Result compute(int x, int y, int z){
    y = (x++ - 1) - ++x;
    x = z = (--x - 1) + ++x * ((((--y * - -10)) - (x++ - x--))) * - -10;
    z = ++y + z-- * x++;
    y = x = z * y / -4 - (((1 + - -991)));

    Result res = {x, y, z};
    return res;
}
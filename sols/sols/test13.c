#include "../function.h"

Result compute(int x, int y, int z){
    y = (--x - 1) - ++x;
    z = (--x - 1) + ++x * --y / - -10 - (x++ - x--) * - -10;
    z = ++y + z-- * x++;
    x = z * y / -4 - 1;

    Result res = {x, y, z};
    return res;
}
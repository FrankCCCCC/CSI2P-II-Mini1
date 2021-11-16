#include "../function.h"

Result compute(int x, int y, int z){
    y = (--x - 1) - ++x;
    z = (--x - 1) + ++x;

    Result res = {x, y, z};
    return res;
}
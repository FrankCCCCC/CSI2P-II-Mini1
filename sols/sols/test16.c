#include "../function.h"

Result compute(int x, int y, int z){
    x = (--x) + 0 * 8 * y + y-- - x + z * 1 + z * z * z - z * y * 9 - - -3 * 9 * 100;
    y = (--z) - 0 * 5 * z + y * -x * z - --z * -y * -9 - -3 * -9 * -100 + x-- + y - 10 * 1;
    z = (--x) * (x++) - 100 * -0 * - - -z - y %  6 -y * z++ - --z * -(++y) * -x - -x * -9 * -100 + y-- + x - -1029 * --x;

    Result res = {x, y, z};
    return res;
}
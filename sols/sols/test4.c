#include "../function.h"

Result compute(int x, int y, int z){
    x=(x+(y-(z*(x/(y%(z+(x-(y*(z/(x%(y+(z-(x*(y/(z%5)))))))))))))));
    y=(((((((((((((((x+5)-y)*z)/x)%y)+z)-x)*y)/z)%x)+y)-z)*x)/y)%z);

    Result res = {x, y, z};
    return res;
}
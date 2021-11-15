#include <stdio.h>
#include <stdlib.h>
#include "function.h"

int main(int argc, char* argv[]){
    int x = 2, y = 3, z = 5;
    if(argc == 4){
        x = atoi(argv[1]);
        y = atoi(argv[2]);
        z = atoi(argv[3]);
        printf("Use custom variables X = %d | Y = %d | Z = %d\n", x, y, z);
    }else{
        printf("Use default variables X = %d | Y = %d | Z = %d\n", x, y, z);
    }
    
    Result res = compute(x, y, z);
    printf("Expcted X: %d | Y: %d | Z: %d\n", res.x, res.y, res.z);
}
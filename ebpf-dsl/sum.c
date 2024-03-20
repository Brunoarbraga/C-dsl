#include <stdio.h>

int main() {
    int  r0 = 0 ;
    printf( "Please enter 4 numbers\n" );
    for(int v1  = 0; v1 < 4 ; v1 ++){
        int  v2 ;
        scanf("%d", & v2 );
        int  v3 = r0 ;
        r0 = (v3 + v2) ;
    }
    printf( "The sum of your numbers is " );
    int  v4 = r0 ;
    printf("%d", v4 );
    printf( ".\n" );
  return 0;
}

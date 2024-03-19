#include <stdio.h>

int main() {
    int  r0 = 1 ;
    printf( "Enter the number:" );
    int  v1 ;
    scanf("%d", & v1 );
    printf( "\n" );
    for(int v2  = 0; v2 < v1 ; v2 ++){
        int  v3 = r0 ;
        r0 = v3 * v2 + 1 ;
    }
    printf( "The result factorial is:" );
    int  v4 = r0 ;
    printf("%d", v4 );
    printf( "\n" );
  return 0;
}

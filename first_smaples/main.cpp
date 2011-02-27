#include <stdio.h>

int i, j, k;
bool flag;

int main()
{
	i = 1;
	k = 0;
	while(k <= 1000){
		flag = true;
		for(j = 2; j < i; j++){
			if(i % j == 0){
				flag = false;
//				break;
			}
		}
		if(flag){
			printf("%d ", i);
			k++;
		}
		i++;
	}
	return 0;
}

#include <iostream>
#include <cstdio>
#include <algorithm>

using namespace std;


int solve(int N, int rating[]){

	sort(rating, rating + N); // sort it


	for(int u = 0; u < N; u++)
		printf("%d ", rating[u]);
	printf("\n");

	int i = 0, k = 0;
	int j = N - 1 - i, ans;
	int a1, b1, a0, b0;
	int idx = 0;

	a0 = 0;
	a1 = 0;

	b0 = 0, b1 = 0;

	ans = b0;

	k = 0;
	while(k < N){

		if(!(k&1)){

			idx = i;
		} else{

			idx = j;
			i++;
			j = N - 1 - i;
		}

		a0 = a1;
		a1 = rating[idx];

		b1 = min(b0, b1) + (a1 > a0 ? 1 : 0);

		ans += b1;

		b0 = b1;
		b1 = 1;

		k++;

	}

	return ans;

}
int main(int argc, char const *argv[])
{
	
	int N, rating[100000 +1];

	scanf("%d", &N);

	for(int i = 0; i < N; i++)
		scanf("%d", &rating[i]);

	printf("%d\n", solve(N, rating));

	return 0;
}
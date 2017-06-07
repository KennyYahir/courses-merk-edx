#include <iostream>
#include <cstdio>
#include <algorithm>
#include <vector>

using namespace std;

typedef vector <long long> vi;

long long solve(long long N, long long rating[]){

	vi mins(N, 1);
	vi mins2(N, 1);

	long long mini = mins[0], maxi;
	long long ans = 0;

	for(long long i = 1; i < N; i++){
		mins[i] = (rating[i] > rating[i-1] ? (mins[i-1] + 1) : mini);

		mini = min(mini, mins[i]);
	}

	// for (long long i = 0; i < N; ++i)
	// {
	// 	prlong longf("%d ", mins[i]);
	// }
	// prlong longf("\n");

	mini = mins2[N-1];

	for (long long i = N-1; i > 0; --i)
	{
		mins2[i-1] = (rating[i-1] > rating[i] ? (mins[i] + 1): mini);

		mini = min(mini, mins2[i-1]);

		ans += max(mins[i], mins2[i]);
	}

	ans += max(mins[0], mins2[0]);

	return ans;
}

int main(int argc, char const *argv[])
{
	long long N, rating[100000 + 1];

	scanf("%lld", &N);

	for(long long i = 0; i < N; i++)
		scanf("%lld", &rating[i]);

	printf("%lld\n", solve(N, rating));
	return 0;
}
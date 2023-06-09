from typing import *

def knapsack01(values: List[int], weights: List[int], cap: int) -> int:
    n = len(values)
    dp = [[0 for _ in range(cap+1)] for _ in range(n+1)]
    for i in range(1, n+1):
        for j in range(1, cap+1):
            if j >= weights[i-1]:
                dp[i][j] = max(dp[i-1][j], dp[i-1][j-weights[i-1]] + values[i-1])
                print(i, j, dp[i][j])
            else:
                dp[i][j] = dp[i-1][j]
    return dp[-1][-1]
    
values = [3,4,5,8,10]
weights = [2,3,4,5,9]
capacity = 20

max_value = knapsack01(values, weights, capacity)
print("Maximum value:", max_value)
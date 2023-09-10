# CSE 321 - Homework 5
import random
import numpy as np


# Problem 1
def longest_common_substring(arr_strings):
    if len(arr_strings) == 0:
        return ""
    if len(arr_strings) == 1:
        return arr_strings[0]

    # Base case
    if all(a_string == arr_strings[0] for a_string in arr_strings):
        return arr_strings[0]

    # Find the maximum length of the strings
    max_len = len(max(arr_strings, key=len))
    div_point = max_len // 2
    if div_point == 0:
        return ""

    # Divide the strings
    strings_left = []
    strings_right = []
    for a_string in arr_strings:
        strings_left.append(a_string[:div_point])
        strings_right.append(a_string[div_point:])
    print(f"strings_left: {strings_left}")
    print(f"strings_right: {strings_right}")

    # Find the longest common substring present at the beginning
    lcs_left = longest_common_substring(strings_left)
    if len(lcs_left) == div_point:
        return lcs_left + longest_common_substring(strings_right)
    else:
        return lcs_left


# Problem 2
class Merchant:
    def __init__(self, arr_prices):
        self.prices = arr_prices
        self.indexes = list(range(len(arr_prices)))
        self.buy_index = 0
        self.sell_index = 0
        self.max_profit = 0

    def reset(self):
        self.buy_index = 0
        self.sell_index = 0
        self.max_profit = 0

    def print_result(self):
        print(f"Input: {self.prices}")
        print(f"Buy on Day{self.buy_index} for {self.prices[self.buy_index]}₺ "
              f"and sell on Day{self.sell_index} for {self.prices[self.sell_index]}₺.")

    # Divide and conquer algorithm to find max profit
    def find_max_profit_dac(self):
        self.reset()
        if len(self.prices) < 2:
            return
        else:
            self.find_max_profit_dac_helper(self.prices, self.indexes)

    # Helper recursive function for divide and conquer
    def find_max_profit_dac_helper(self, prices, indexes):
        # Base case
        if len(indexes) == 1:
            return
        if len(indexes) == 2:
            if self.prices[indexes[0]] < self.prices[indexes[1]]:
                profit = self.prices[indexes[1]] - self.prices[indexes[0]]
                if profit > self.max_profit:
                    self.max_profit = profit
                    self.buy_index = indexes[0]
                    self.sell_index = indexes[1]
            return
        mid = len(indexes) // 2
        self.find_max_profit_dac_helper(prices[:mid], indexes[:mid])  # Left part
        self.find_max_profit_dac_helper(prices[mid:], indexes[mid:])  # Right part
        left_min = min(prices[:mid])
        right_max = max(prices[mid:])
        mixed_max_profit = right_max - left_min
        if mixed_max_profit > self.max_profit:
            self.max_profit = mixed_max_profit
            for i in range(0, mid):
                if self.prices[indexes[i]] == left_min:
                    self.buy_index = indexes[i]
            for i in range(mid, len(indexes)):
                if self.prices[indexes[i]] == right_max:
                    self.sell_index = indexes[i]
        return

    # Linear time algorithm to find the max profit
    def find_max_profit_linear(self):
        self.reset()
        if len(self.prices) < 2:
            return
        else:
            idx_buy = 0
            idx_sell = len(self.prices) - 1
            while idx_buy != idx_sell:
                if self.prices[idx_buy] <= self.prices[idx_sell]:
                    profit = self.prices[idx_sell] - self.prices[idx_buy]
                    if profit > self.max_profit:
                        self.max_profit = profit
                        self.buy_index = idx_buy
                        self.sell_index = idx_sell
                    idx_buy += 1
                else:
                    idx_sell -= 1
            return


# Problem 3
def longest_increasing_subarray(arr_nums):
    n = len(arr_nums)
    dp = [1] * n

    for i in range(1, n):
        if arr_nums[i] > arr_nums[i - 1]:
            dp[i] = dp[i - 1] + 1

    max_len = max(dp)
    idx = dp.index(max_len)
    sub_array = arr_nums[idx + 1 - max_len: idx + 1]

    return max_len, sub_array


# Problem 4
class FindMaxPoint2DGame:
    def __init__(self, map, n, m):
        self.map = map
        self.n = n
        self.m = m
        self.max_points = 0
        self.path = []

    def print_result(self):
        print(f"Input: n = {self.n}, m = {self.m}")
        print("Game map:")
        print(self.map)
        print("Output:")
        print(f"Route: {self.path}")
        print(f"Points: {self.max_points}")

    def algo_dfs(self, i, j, points, visited, curr_path):
        # Update maximum points and path
        if points > self.max_points:
            self.max_points = points
            self.path = curr_path[:]
        # Base case: end of map reached
        if i == self.n - 1 and j == self.m - 1:
            return
        # Recursive case: continue traversing map
        if i < self.n - 1 and (i + 1, j) not in visited:
            self.algo_dfs(i + 1, j, points + self.map[i + 1][j], visited + [(i + 1, j)], curr_path + [(i + 2, j + 1)])
        if j < self.m - 1 and (i, j + 1) not in visited:
            self.algo_dfs(i, j + 1, points + self.map[i][j + 1], visited + [(i, j + 1)], curr_path + [(i + 1, j + 2)])

    def algo_dp(self):
        # Initialize 2D array to store maximum points at each coordinate
        dp = [[0] * self.m for _ in range(self.n)]

        # Iterate through map and fill dp array
        for i in range(self.n):
            for j in range(self.m):
                # Check if element is in first row or column
                if i == 0 and j == 0:
                    dp[i][j] = self.map[i][j]
                elif i == 0:
                    dp[i][j] = dp[i][j - 1] + self.map[i][j]
                elif j == 0:
                    dp[i][j] = dp[i - 1][j] + self.map[i][j]
                # Otherwise, element is in middle of map
                else:
                    dp[i][j] = max(dp[i - 1][j], dp[i][j - 1]) + self.map[i][j]

        # Set the maximum point
        self.max_points = dp[self.n - 1][self.m - 1]

        # Initialize path with last element in map
        self.path.append((self.n, self.m))

        # Iterate through dp array in reverse and fill path list
        i = self.n - 1
        j = self.m - 1
        while i > 0 or j > 0:
            # Check if element is in first row or column
            if i == 0 and j == 0:
                break
            elif i == 0:
                j -= 1
            elif j == 0:
                i -= 1
            # Otherwise, element is in middle of map
            else:
                if dp[i][j] == dp[i - 1][j] + self.map[i][j]:
                    i -= 1
                else:
                    j -= 1
            # Add element to start of path list
            self.path.insert(0, (i + 1, j + 1))

    def algo_greedy(self):
        # Initialize path with first element in map
        self.path = [(1, 1)]
        # Initialize maximum number of points with first element in map
        self.max_points = self.map[0][0]

        # Initialize current row and column indices
        i = 0
        j = 0

        # Iterate through map and fill path list
        while i < self.n - 1 or j < self.m - 1:
            # Check if element to the right or down has higher value
            if i < self.n - 1 and j < self.m - 1:
                if self.map[i + 1][j] > self.map[i][j + 1]:
                    i += 1
                else:
                    j += 1
            elif i < self.n - 1:
                i += 1
            else:
                j += 1

            # Add element to end of path list and update maximum number of points
            self.path.append((i + 1, j + 1))
            self.max_points += map[i][j]

    def find_max_points(self, algo):
        if algo == 1:  # Deep first search
            self.algo_dfs(0, 0, self.map[0][0], [(0, 0)], [(1, 1)])
        elif algo == 2:  # Dynamic programming
            self.algo_dp()
        elif algo == 3:  # Grredy algorithm
            self.algo_greedy()


# Driver
if __name__ == '__main__':
    print("### Problem 1 ###")
    strings = []
    num_strings = int(input("Number of strings?: "))
    for i in range(0, num_strings):
        a_string = input(f"Enter strings[{i}]: ")
        strings.append(a_string)

    print(f"Input: {strings}")
    print(f"Output: {longest_common_substring(strings)}")

    print("### Problem 2 ###")
    prices = []
    num_prices = int(input("Number of prices?: "))
    for i in range(0, num_prices):
        a_price = int(input(f"Enter prices[{i}]: "))
        prices.append(a_price)
    m = Merchant(prices)
    print("1) Decrease and Conquer")
    print("2) Linear")
    select = int(input("Select: "))
    if select == 1:
        m.find_max_profit_dac()
        m.print_result()
    elif select == 2:
        m.find_max_profit_linear()
        m.print_result()
    else:
        print("Select either 1 or 2!")

    print("### Problem 3 ###")
    values = []
    num_values = int(input("Number of values?: "))
    for i in range(0, num_values):
        a_price = int(input(f"Enter values[{i}]: "))
        values.append(a_price)

    max_len, sub_array = longest_increasing_subarray(values)
    print(f"Input: {values}")
    print(f"Output: {max_len} (the sub-array is {sub_array})")

    print("### Problem 4 ###")
    rows = random.randint(2, 9)
    cols = random.randint(2, 9)
    map = np.random.randint(1, 100, size=(rows, cols))
    game = FindMaxPoint2DGame(map, rows, cols)

    print("1) Deep First Search")
    print("2) Dynamic Programming")
    print("3) Greedy Algorithm")
    select = int(input("Select: "))
    if select == 1 or select == 2 or select == 3:
        game.find_max_points(select)
        game.print_result()
    else:
        print("Select either 1, 2 or 3!")

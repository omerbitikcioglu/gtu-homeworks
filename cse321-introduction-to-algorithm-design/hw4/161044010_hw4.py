import random
import numpy as np


# Problem 1
class FindMaxPoint2DGame:
    def __init__(self, map, n, m):
        self.map = map
        self.n = n
        self.m = m
        self.max_points = 0
        self.path = []

    def dfs(self, i, j, points, visited, curr_path):
        # Update maximum points and path
        if points > self.max_points:
            self.max_points = points
            self.path = curr_path[:]
        # Base case: end of map reached
        if i == self.n - 1 and j == self.m - 1:
            return
        # Recursive case: continue traversing map
        if i < self.n - 1 and (i + 1, j) not in visited:
            self.dfs(i + 1, j, points + self.map[i + 1][j], visited + [(i + 1, j)], curr_path + [(i + 2, j + 1)])
        if j < self.m - 1 and (i, j + 1) not in visited:
            self.dfs(i, j + 1, points + self.map[i][j + 1], visited + [(i, j + 1)], curr_path + [(i + 1, j + 2)])

    def find_max_points(self):
        # Start traversing map from (1, 1)
        self.dfs(0, 0, self.map[0][0], [(0, 0)], [(1, 1)])

        # Return path with maximum total points
        return self.path, self.max_points


# Problem 2
def quickselect_median(arr):
    if not arr:
        return None
    if len(arr) % 2 == 0:
        return (quickselect(arr, len(arr) // 2 - 1) + quickselect(arr, len(arr) // 2)) / 2
    else:
        return quickselect(arr, len(arr) // 2)


# Find kth smallest element in arr
def quickselect(arr, k):
    if not arr:
        return None
    # Check if all elements in the array are the same
    if all(x == arr[0] for x in arr):
        return arr[0]
    pivot = select_pivot(arr)
    # Divide the array into two parts based on the pivot
    left = [x for x in arr if x < pivot]
    right = [x for x in arr if x > pivot]
    # Search for based on the pivot partition
    if k < len(left):
        return quickselect(left, k)
    elif k > len(left):
        return quickselect(right, k - len(left) - 1)
    else:
        return pivot


def select_pivot(arr):
    # Select a random element from the array as the pivot
    return random.choice(arr)


# Problem 3
class Node:
    def __init__(self, value, next=None):
        self.value = value
        self.next = next


# 3.a Circular Linked List
def find_winner_circular(players):
    # Create a circular linked list with the players as the nodes
    head = Node(players[0])
    current = head
    for player in players[1:]:
        current.next = Node(player, head)
        current = current.next
    current.next = head

    # Find the starting player
    while current.value != 'P1':
        current = current.next

    # Eliminate players until there is only one remaining
    while current.next != current:
        print(f"{current.value} eliminates {current.next.value}.")
        current.next = current.next.next
        current = current.next

    # Return the value of the remaining player
    return current.value


# 3.b Decrease and Conquer
def find_winner_dac(players):
    # Turn is first or last player in the list
    turn = "First"
    print(f"Turn: {turn}")
    # Eliminate the players until one player left
    while len(players) != 1:
        players_old = players
        if turn == "First":
            players = players[::2]
            print(players)
        else:
            players = players[1::2]
            print(players)
        if len(players_old) % 2 != 0:
            if turn == "First":
                turn = "Last"
            else:
                turn = "First"
        print(f"Turn: {turn}")
    return players[0]


# Driver
if __name__ == '__main__':
    # Problem 1
    rows = random.randint(2, 9)
    cols = random.randint(2, 9)
    map = np.random.randint(1, 100, size=(rows, cols))

    p1 = FindMaxPoint2DGame(map, rows, cols)
    path, max_points = p1.find_max_points()

    print("### Problem 1 ###")
    print(f"Map:\n{map}\nRows: {rows}, Columns: {cols}")
    print(f"Path: {path}")
    print(f"Max Point: {max_points}\n")

    # Problem 2
    arr_size = random.randint(1, 20)
    arr = random.sample(range(0, 100), arr_size)

    result = quickselect_median(arr)

    print("### Problem 2 ###")
    print(f"Median of {arr} = {result}\n")

    # Problem 3.a
    num_players = random.randint(1, 20)
    players = []
    for i in range(0, num_players):
        players.append(f"P{i + 1}")

    print("### Problem 3.a ###")
    print(f"Players: {players}")
    winner = find_winner_circular(players)
    print(f'The winner is {winner}!\n')

    # Problem 3.b
    print("### Problem 3.b ###")
    # Make the array tuple to slice in constant time
    players = tuple(players)
    print(players)
    winner = find_winner_dac(players)
    print(f'The winner is {winner}!\n')

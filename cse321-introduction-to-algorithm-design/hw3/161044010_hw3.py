# Python program to print topological sorting of a DAG
from collections import defaultdict
import numpy as np


# Class to represent a graph


class Graph:
    def __init__(self, vertices):
        self.graph = defaultdict(list)  # dictionary containing adjacency List
        self.V = vertices  # No. of vertices

    # function to add an edge to graph
    def add_edge(self, u, v):
        self.graph[u].append(v)

    # A recursive function used by dfs_topological_sort
    def dfs_util(self, v, visited, stack):

        # Mark the current node as visited.
        visited[v] = True

        # Recur for all the vertices adjacent to this vertex
        for i in self.graph[v]:
            if not visited[i]:
                self.dfs_util(i, visited, stack)

        # Push current vertex to stack which stores result
        stack.append(v)

    # The function to do DFS based Topological Sort. It uses recursive
    # dfs_topological_sort_util()
    def dfs_topological_sort(self):
        # Mark all the vertices as not visited
        visited = [False] * self.V
        stack = []

        # Call the recursive helper function to store Topological
        # Sort starting from all vertices one by one
        for i in range(self.V):
            if not visited[i]:
                self.dfs_util(i, visited, stack)

        # Print contents of the stack
        print(stack[::-1])  # return list in reverse order

    # The function to do BFS based Topological Sort.
    def bfs_topological_sort(self):

        # Create a vector to store in-degrees of all
        # vertices. Initialize all in-degrees as 0.
        in_degree = [0] * self.V

        # Traverse adjacency lists to fill in-degrees of
        # vertices.  This step takes O(V + E) time
        for i in self.graph:
            for j in self.graph[i]:
                in_degree[j] += 1

        # Create a queue and enqueue all vertices with
        # in-degree 0
        queue = []
        for i in range(self.V):
            if in_degree[i] == 0:
                queue.append(i)

        # Initialize count of visited vertices
        cnt = 0

        # Create a vector to store result (A topological
        # ordering of the vertices)
        top_order = []

        # One by one dequeue vertices from queue and enqueue
        # adjacent if in-degree of adjacent becomes 0
        while queue:

            # Extract front of queue (or perform dequeue)
            # and add it to topological order
            u = queue.pop(0)
            top_order.append(u)

            # Iterate through all neighbouring nodes
            # of dequeued node u and decrease their in-degree
            # by 1
            for i in self.graph[u]:
                in_degree[i] -= 1
                # If in-degree becomes zero, add it to queue
                if in_degree[i] == 0:
                    queue.append(i)

            cnt += 1

        # Check if there was a cycle
        if cnt != self.V:
            print("There exists a cycle in the graph")
        else:
            # Print topological order
            print(top_order)


class Sudoku:
    def __init__(self, input_grid):
        self.grid = input_grid

    def possible(self, y, x, n):
        for i in range(0, 9):
            if self.grid[y][i] == n:
                return False
        for i in range(0, 9):
            if self.grid[i][x] == n:
                return False
        x0 = (x // 3) * 3
        y0 = (y // 3) * 3
        for i in range(0, 3):
            for j in range(0, 3):
                if self.grid[y0 + i][x0 + j] == n:
                    return False
        return True

    def solve(self):
        for y in range(9):
            for x in range(9):
                if self.grid[y][x] == 0:
                    for n in range(1, 10):
                        if self.possible(y, x, n):
                            self.grid[y][x] = n
                            self.solve()
                            self.grid[y][x] = 0
                    return
        self.print()
        input("More?")

    def print(self):
        print(np.matrix(self.grid))


def binary_exponentiation(a, n):
    result = 1
    while n > 0:
        if n % 2 == 1:
            result = result * a
        a = a * a
        n = n // 2
    return result


# Question 1-a),b)
def question_1():
    n = input("Enter number of nodes of DAG: ")
    n = int(n)
    g = Graph(n)

    e = int(input("Enter number of edges of DAG: "))
    max_num_of_edges = (n * (n - 1)) / 2
    if e <= max_num_of_edges:
        for i in range(0, e):
            u, v = input("Enter edge %d (5,2 means 5->2):\n" % (i + 1)).split(",", 2)
            u, v = int(u), int(v)
            g.add_edge(u, v)
            print("%d->%d", (u, g.graph[u]))
    else:
        print("Edge number is too high!")
        return

    print("1-a)DFS based sort: "), g.dfs_topological_sort()  # DFS
    print("1-b)BFS based sort:"), g.bfs_topological_sort()  # BFS


# Question 2)
def question_2():
    a, n = input("Enter a and n values: ").split(",", 2)
    a, n = int(a), int(n)
    print("2) %d ^ %d = %d" % (a, n, binary_exponentiation(a, n)))


# Question 3)
def question_3():
    print("Enter sudoku vertices one by one seperated by spaces")
    grid = []
    for i in range(0, 9):
        row = []
        print("Enter elements of row %d: " % (i + 1))
        for j in range(0, 9):
            row.append(int(input()))
        grid.append(row)
    print("3)")
    """"" You can use it if you tired of typing
    grid = [[5, 3, 0, 0, 7, 0, 0, 0, 0],
            [6, 0, 0, 1, 9, 5, 0, 0, 0],
            [0, 9, 8, 0, 0, 0, 0, 6, 0],
            [8, 0, 0, 0, 6, 0, 0, 0, 3],
            [4, 0, 0, 8, 0, 3, 0, 0, 1],
            [7, 0, 0, 0, 2, 0, 0, 0, 6],
            [0, 6, 0, 0, 0, 0, 2, 8, 0],
            [0, 0, 0, 4, 1, 9, 0, 0, 5],
            [0, 0, 0, 0, 8, 0, 0, 7, 9]]
    """""
    sudoku = Sudoku(grid)
    sudoku.solve()


# Driver Code
if __name__ == '__main__':
    print("HW3 Menu")
    while True:
        print("1) Question 1")
        print("2) Question 2")
        print("3) Question 3")
        print("4) Exit")
        selection = int(input("Select one: "))
        if selection == 1:
            question_1()
        elif selection == 2:
            question_2()
        elif selection == 3:
            question_3()
        elif selection == 4:
            break
        else:
            print("Please type 1, 2, or 3!")

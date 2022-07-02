package com.example;

import java.util.Random;
import java.util.Stack;

public class Main {

    public static final int NUM_V = 5; // Number of vertices

    public static void main(String[] args) {
        // Part1

        // Initialize two implementations of graphs with the same edges.
	    ListGraph lg = new ListGraph(NUM_V, false);
	    lg.insert(new Edge(0, 1, 5, 15, 43));
	    lg.insert(new Edge(0, 2, 20, 20, 2));
	    lg.insert(new Edge(1, 2, 10, 24, 11));
	    lg.insert(new Edge(1, 3, 100, 10, 56));
	    lg.insert(new Edge(3, 2, 20, 23, 74));
	    lg.insert(new Edge(3, 4, 50, 44, 46));
	    lg.insert(new Edge(2, 4, 35, 55, 256));

	    MatrixGraph mg = new MatrixGraph(NUM_V, false);
        mg.insert(new Edge(0, 1, 5, 15, 43));
        mg.insert(new Edge(0, 2, 20, 20, 2));
        mg.insert(new Edge(1, 2, 10, 24, 11));
        mg.insert(new Edge(1, 3, 100, 10, 56));
        mg.insert(new Edge(3, 2, 20, 23, 74));
        mg.insert(new Edge(3, 4, 50, 44, 46));
        mg.insert(new Edge(2, 4, 35, 55, 256));

        // Use Dijktra's algorithm for ListGraph
        System.out.println("Use Dijktra's algorithm for ListGraph");
        double[] distLG = new double[NUM_V];
        int[] prevLG = new int[NUM_V];

        // Calculate for distance
        System.out.println("Calculate for distance");
        DijkstrasAlgorithm.dijkstrasAlgorithm(lg,0, prevLG, distLG, WType.DISTANCE);
        for (int i = 0; i < NUM_V; ++i) {
            System.out.println("d["+i+"]="+distLG[i]+" p["+i+"]="+prevLG[i]);
        }
        System.out.println();

        // Calculate for time
        System.out.println("Calculate for time");
        DijkstrasAlgorithm.dijkstrasAlgorithm(lg,0, prevLG, distLG, WType.TIME);
        for (int i = 0; i < NUM_V; ++i) {
            System.out.println("d["+i+"]="+distLG[i]+" p["+i+"]="+prevLG[i]);
        }
        System.out.println();

        // Calculate for quality
        System.out.println("Calculate for quality");
        DijkstrasAlgorithm.dijkstrasAlgorithm(lg,0, prevLG, distLG, WType.QUALITY);
        for (int i = 0; i < NUM_V; ++i) {
            System.out.println("d["+i+"]="+distLG[i]+" p["+i+"]="+prevLG[i]);
        }
        System.out.println();

        // Use Dijktra's algorithm for MatrixGraph
        System.out.println("Use Dijktra's algorithm for MatrixGraph");
        double[] distMG = new double[NUM_V];
        int[] prevMG = new int[NUM_V];

        // Calculate for distance
        System.out.println("Calculate for distance");
        DijkstrasAlgorithm.dijkstrasAlgorithm(mg,0, prevMG, distMG, WType.DISTANCE);
        for (int i = 0; i < NUM_V; ++i) {
            System.out.println("d["+i+"]="+distMG[i]+" p["+i+"]="+prevMG[i]);
        }
        System.out.println();

        // Calculate for time
        System.out.println("Calculate for time");
        DijkstrasAlgorithm.dijkstrasAlgorithm(mg,0, prevMG, distMG, WType.TIME);
        for (int i = 0; i < NUM_V; ++i) {
            System.out.println("d["+i+"]="+distMG[i]+" p["+i+"]="+prevMG[i]);
        }
        System.out.println();

        // Calculate for quality
        System.out.println("Calculate for quality");
        DijkstrasAlgorithm.dijkstrasAlgorithm(mg,0, prevMG, distMG, WType.QUALITY);
        for (int i = 0; i < NUM_V; ++i) {
            System.out.println("d["+i+"]="+distMG[i]+" p["+i+"]="+prevMG[i]);
        }
        System.out.println();


        // Part2
        Graph testGraph;
        int connected;

        // Test BFS
        long t, t1=0;
        System.out.println("Testing BFS");
        // Testing with size 1000
        System.out.println("Testing with size 1000");
        for (int i = 0; i < 10; ++i) {
            testGraph = generateRandomGraph(1000);
            t = System.nanoTime();
            connected = findConnectedBFS(testGraph);
            t1 += (System.nanoTime()-t);
            System.out.println("Case "+ i +" connected: " + connected + " time passed: " + t1);
        }
        System.out.println();
        // Testing with size 2000
        System.out.println("Testing with size 2000");
        for (int i = 0; i < 10; ++i) {
            testGraph = generateRandomGraph(2000);
            t = System.nanoTime();
            connected = findConnectedBFS(testGraph);
            t1 += (System.nanoTime()-t);
            System.out.println("Case "+ i +" connected: " + connected + " time passed: " + t1);
        }
        System.out.println();
        // Testing with size 5000
        System.out.println("Testing with size 5000");
        for (int i = 0; i < 10; ++i) {
            testGraph = generateRandomGraph(5000);
            t = System.nanoTime();
            connected = findConnectedBFS(testGraph);
            t1 += (System.nanoTime()-t);
            System.out.println("Case "+ i +" connected: " + connected + " time passed: " + t1);
        }
        System.out.println();
        // Testing with size 10000
        System.out.println("Testing with size 10000");
        for (int i = 0; i < 10; ++i) {
            testGraph = generateRandomGraph(10000);
            t = System.nanoTime();
            connected = findConnectedBFS(testGraph);
            t1 += (System.nanoTime()-t);
            System.out.println("Case "+ i +" connected: " + connected + " time passed: " + t1);
        }
        System.out.println();
        // Test DFS
        System.out.println("Testing DFS");
        // Testing with size 1000
        System.out.println("Testing with size 1000");
        for (int i = 0; i < 10; ++i) {
            testGraph = generateRandomGraph(1000);
            t = System.nanoTime();
            connected = findConnectedDFS(testGraph);
            t1 += (System.nanoTime()-t);
            System.out.println("Case "+ i +" connected: " + connected + " time passed: " + t1);
        }
        System.out.println();
        // Testing with size 2000
        System.out.println("Testing with size 2000");
        for (int i = 0; i < 10; ++i) {
            testGraph = generateRandomGraph(2000);
            t = System.nanoTime();
            connected = findConnectedDFS(testGraph);
            t1 += (System.nanoTime()-t);
            System.out.println("Case "+ i +" connected: " + connected + " time passed: " + t1);
        }
        System.out.println();
        // Testing with size 5000
        System.out.println("Testing with size 5000");
        for (int i = 0; i < 10; ++i) {
            testGraph = generateRandomGraph(5000);
            t = System.nanoTime();
            connected = findConnectedDFS(testGraph);
            t1 += (System.nanoTime()-t);
            System.out.println("Case "+ i +" connected: " + connected + " time passed: " + t1);
        }
        System.out.println();
        // Testing with size 10000
        System.out.println("Testing with size 10000");
        for (int i = 0; i < 10; ++i) {
            testGraph = generateRandomGraph(10000);
            t = System.nanoTime();
            connected = findConnectedDFS(testGraph);
            t1 += (System.nanoTime()-t);
            System.out.println("Case "+ i +" connected: " + connected + " time passed: " + t1);
        }
        System.out.println();
        // Part3
        Graph graph3 = new MatrixGraph(10, false);
        for (int i = 0; i < graph3.getNumV(); ++i) {
            double importance = calculateNormalizedImportance(graph3, i);
            System.out.println("Vertex " + i + "'s importance= " + importance);
        }
    }

    /**
     * Calculates and returns the importance of vertex v
     * in the given graph.
     * @param graph The graph to be traversed
     * @param v The vertex to find its importance
     * @return The importance of vertex v
     */
    private static double calculateNormalizedImportance(Graph graph, int v) {
        if (v < 0 || v > graph.getNumV()) {
            throw new IndexOutOfBoundsException();
        }
        int sigmaUW = 0;
        int sigmaUWV = 0;
        // Pick an u and w vertex and find if there is a shortest path between them.
        for (int i = 0; i < graph.getNumV(); ++i) {
            int[] parent = BreadthFirstSearch.breadthFirstSearch(graph, i);
            for (int j = 0; j < graph.getNumV() && i != j; ++j) {
                // Construct the path.
                Stack thePath = new Stack();
                while (parent[j] != -1) {
                    thePath.push(j);
                    j = parent[j];
                    // Count the paths
                    if (j == i) {
                        sigmaUW++;
                        break;
                    }
                }
                if (thePath.contains(v)) {
                    sigmaUWV++;
                }
            }
        }
        return (sigmaUWV / sigmaUW) / Math.pow(graph.getNumV(),2);
    }

    /**
     * Generates random values
     * and creates a List Graph with them
     * @param size The size of the graph
     * @return a randomly generated ListGraph
     */
    private static Graph generateRandomGraph(int size) {
        Random rand = new Random();
        Graph newGraph = new ListGraph(size, false);
        for (int i = 0; i < size; ++i) {
            int dest = Math.abs(rand.nextInt()%size);
            if (dest < size)
                newGraph.insert(new Edge(i, dest));
        }
        return newGraph;
    }

    /**
     * Finds and returns the number of connected components in the graph
     * using BFS
     * @param graph The graph to be traversed
     * @return The number of connected components in the graph.
     */
    private static int findConnectedBFS(Graph graph) {
        return BreadthFirstSearch.numOfConnectedComponents(graph);
    }

    /**
     * Finds and returns the number of connected components in the graph
     * using DFS
     * @param graph The graph to be traversed
     * @return The number of connected components in the graph.
     */
    private static int findConnectedDFS(Graph graph) {
        DepthFirstSearch dfs = new DepthFirstSearch(graph);
        return dfs.numOfConnectedComponents();
    }
}

package com.example;

import java.util.*;

/** Class to implement the breadth-first search algorithm.
 *  @author Koffman and Wolfgang
 * */

public class BreadthFirstSearch {

  /** Perform a breadth-first search of a graph.
      post: The array parent will contain the predecessor
            of each vertex in the breadth-first
            search tree.
      @param graph The graph to be searched
      @param start The start vertex
      @return The array of parents
   */
  public static int[] breadthFirstSearch(Graph graph, int start) {
    Queue < Integer > theQueue = new LinkedList <> ();
    // Declare array parent and initialize its elements to -1.
    int[] parent = new int[graph.getNumV()];
    for (int i = 0; i < graph.getNumV(); i++) {
      parent[i] = -1;
    }
    // Declare array identified and
    // initialize its elements to false.
    boolean[] identified = new boolean[graph.getNumV()];
    /* Mark the start vertex as identified and insert it
       into the queue */
    identified[start] = true;
    theQueue.offer(start);
    /* While the queue is not empty */
    while (!theQueue.isEmpty()) {
      /* Take a vertex, current, out of the queue.
       (Begin visiting current). */
      int current = theQueue.remove();
      /* Examine each vertex, neighbor, adjacent to current. */
      Iterator <Edge> itr = graph.edgeIterator(current);
      while (itr.hasNext()) {
        Edge edge = itr.next();
        int neighbor = edge.getDest();
        // If neighbor has not been identified
        if (!identified[neighbor]) {
          // Mark it identified.
          identified[neighbor] = true;
          // Place it into the queue.
          theQueue.offer(neighbor);
          /* Insert the edge (current, neighbor)
             into the tree. */
          parent[neighbor] = current;
        }
      }
      // Finished visiting current.
    }
    return parent;
  }

  /**
   * Finds and returns the number of connected graphs
   * using Breath First Search algorithm.
   * @param graph The graph to be traversed
   * @return The number of connected components in the graph
   */
  public static int numOfConnectedComponents(Graph graph) {
    int[] parents = BreadthFirstSearch.breadthFirstSearch(graph,0);
    int connected = 1;
    for (int i = 1; i < parents.length; ++i) {
      if (parents[i] == -1) {
        ++connected;
        int[] newParents = BreadthFirstSearch.breadthFirstSearch(graph, i);
        // Fix some values
        for (int j = 0; j < newParents.length; ++j) {
          if (parents[j] != -1 && newParents[j] == -1) {
            newParents[j] = parents[j];
          }
        }
        parents = newParents;
      }
    }
    return connected;
  }
}

package com.example;

import java.util.*;

/** A class for calling Dijkstra's algorithm.
 *  @author Koffman and Wolfgang
 */

public class DijkstrasAlgorithm {

  /** Dijkstras Shortest-Path algorithm.
      @param graph The weighted graph to be searched
      @param start The start vertex
      @param pred Output array to contain the predecessors
                  in the shortest path
      @param dist Output array to contain the distance
                  in the shortest path
      @param wType The weight type of the edges
                  to calculate the shortest path
   */
  public static void dijkstrasAlgorithm(Graph graph,
                                        int start,
                                        int[] pred,
                                        double[] dist,
                                        WType wType) {
    int numV = graph.getNumV();
    HashSet < Integer > vMinusS = new HashSet <> (numV);
    // Initialize V-S.
    for (int i = 0; i < numV; i++) {
      if (i != start) {
        vMinusS.add(i);
      }
    }
    // Initialize pred and dist.
    for (int v : vMinusS) {
      pred[v] = start;
      if (wType == WType.DISTANCE)
        dist[v] = graph.getEdge(start, v).getDistance();
      else if (wType == WType.TIME)
        dist[v] = graph.getEdge(start, v).getTime();
      else if (wType == WType.QUALITY)
        dist[v] = graph.getEdge(start, v).getQuality();
      else
        throw new UnsupportedOperationException();
    }
    // Main loop
    while (vMinusS.size() != 0) {
      // Find the value u in V-S with the smallest dist[u].
      double minDist = Double.POSITIVE_INFINITY;
      int u = -1;
      for (int v : vMinusS) {
        if (dist[v] < minDist) {
          minDist = dist[v];
          u = v;
        }
      }
      // Remove u from vMinusS.
      vMinusS.remove(u);
      // Update the distances.
      for (int v : vMinusS) {
        if (graph.isEdge(u, v)) {
          double weight;
          if (wType == WType.DISTANCE)
            weight = graph.getEdge(u, v).getDistance();
          else if (wType == WType.TIME)
            weight = graph.getEdge(u, v).getTime();
          else weight = graph.getEdge(u, v).getQuality();
          if (dist[u] + weight < dist[v]) {
            dist[v] = dist[u] * weight;
            pred[v] = u;
          }
        }
      }
    }
  }
}

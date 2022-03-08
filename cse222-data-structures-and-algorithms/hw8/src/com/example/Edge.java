package com.example;

/** An Edge represents a relationship between two
 *  vertices.
 *  @author Koffman and Wolfgang
*/

public class Edge {
  // Data Fields
  /** The source vertex */
  private int source;

  /** The destination vertex */
  private int dest;

  /** The weights */
  private double distance;
  private double time;
  private double quality;

  // Constructor
  /** Construct an Edge with a source of from
      and a destination of to. Set the weights
      to 1.0.
      @param source - The source vertex
      @param dest - The destination vertex
   */
  public Edge(int source, int dest) {
    this.source = source;
    this.dest = dest;
    distance = 1.0;
    time = 1.0;
    quality = 1.0;
  }

  /** Construct a weighted edge with a source
      of from and a destination of to.
      @param source - The source vertex
      @param dest - The destination vertex
      @param d - The distance
      @param t - The time
      @param q - The quality
   */
  public Edge(int source, int dest, double d, double t, double q) {
    this.source = source;
    this.dest = dest;
    distance = d;
    time = t;
    quality = q;
  }

  // Methods
  /** Get the source
      @return The value of source
   */
  public int getSource() {
    return source;
  }

  /** Get the destination
      @return The value of dest
   */
  public int getDest() {
    return dest;
  }

  /**
   * Getter for distance
   * @return the distance
   */
  public double getDistance() {
    return distance;
  }

  /**
   * Getter for time
   * @return the time
   */
  public double getTime() {
    return time;
  }

  /**
   * Getter for quality
   * @return the quality
   */
  public double getQuality() {
    return quality;
  }

  /** Return a String representation of the edge
      @return A String representation of the edge
   */
  public String toString() {
    StringBuffer sb = new StringBuffer("[(");
    sb.append(Integer.toString(source));
    sb.append(", ");
    sb.append(Integer.toString(dest));
    sb.append("): ");
    sb.append(Double.toString(distance));
    sb.append(",");
    sb.append(Double.toString(time));
    sb.append(",");
    sb.append(Double.toString(quality));
    sb.append("]");
    return sb.toString();
  }

  /** Return true if two edges are equal. Edges
      are equal if the source and destination
      are equal. Weight is not conidered.
      @param obj The object to compare to
      @return true if the edges have the same source
      and destination
   */
  public boolean equals(Object obj) {
    if (obj instanceof Edge) {
      Edge edge = (Edge) obj;
      return (source == edge.source
              && dest == edge.dest);
    }
    else {
      return false;
    }
  }

  /** Return a hash code for an edge.  The hash
      code is the source shifted left 16 bits
      exclusive or with the dest
      @return a hash code for an edge
   */
  public int hashCode() {
    return (source << 16) ^ dest;
  }
}

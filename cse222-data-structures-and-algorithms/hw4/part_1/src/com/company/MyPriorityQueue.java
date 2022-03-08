package com.company;

import java.util.*;

/**
 * Book's priority queue implementation with additional functionalities.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */

public class MyPriorityQueue < E > extends AbstractQueue < E > implements Queue < E > {

    // Data Fields
    /** The ArrayList to hold the data. */
    private ArrayList < E > theData;

    /** An optional reference to a Comparator object. */
    Comparator < E > comparator = null;

    // Methods
    // Constructor
    public MyPriorityQueue() {
        theData = new ArrayList < E > ();
    }

    /** Creates a heap-based priority queue with the specified initial
     capacity that orders its elements according to the specified
     comparator.
     @param cap The initial capacity for this priority queue
     @param comp The comparator used to order this priority queue
     @throws IllegalArgumentException if cap is less than 1
     */
    public MyPriorityQueue(int cap, Comparator < E > comp) {
        if (cap < 1)
            throw new IllegalArgumentException();
        theData = new ArrayList < E > (cap + 1);
        comparator = comp;
    }

    /** Insert an item into the priority queue.
     pre: The ArrayList theData is in heap order.
     post: The item is in the priority queue and
     theData is in heap order.
     @param item The item to be inserted
     @throws NullPointerException if the item to be inserted is null.
     */
    public boolean offer(E item) {
        // Add the item to the heap.
        theData.add(item);
        // child is newly inserted item.
        int child = theData.size() - 1;
        int parent = (child - 1) / 2; // Find child's parent.
        // Reheap
        while (parent >= 0 && compare(theData.get(parent),
                theData.get(child)) > 0) {
            swap(parent, child);
            child = parent;
            parent = (child - 1) / 2;
        }
        return true;
    }

    /** Remove an item from the priority queue
     pre: The ArrayList theData is in heap order.
     post: Removed smallest item, theData is in heap order.
     @return The item with the smallest priority value or null if empty.
     */
    public E poll() {
        if (isEmpty()) {
            return null;
        }
        // Save the top of the heap.
        E result = theData.get(0);
        // If only one item then remove it.
        if (theData.size() == 1) {
            theData.remove(0);
            return result;
        }
    /* Remove the last item from the ArrayList and place it into
       the first position. */
        theData.set(0, theData.remove(theData.size() - 1));
        // The parent starts at the top.
        int parent = 0;
        while (true) {
            int leftChild = 2 * parent + 1;
            if (leftChild >= theData.size()) {
                break; // Out of heap.
            }
            int rightChild = leftChild + 1;
            int minChild = leftChild; // Assume leftChild is smaller.
            // See whether rightChild is smaller.
            if (rightChild < theData.size()
                    && compare(theData.get(leftChild),
                    theData.get(rightChild)) > 0) {
                minChild = rightChild;
            }
            // assert: minChild is the index of the smaller child.
            // Move smaller child up heap if necessary.
            if (compare(theData.get(parent),
                    theData.get(minChild)) > 0) {
                swap(parent, minChild);
                parent = minChild;
            }
            else { // Heap property is restored.
                break;
            }
        }
        return result;
    }

    /** Compare two items using either a Comparator object�s compare method
     or their natural ordering using method compareTo.
     pre: If comparator is null, left and right implement Comparable<E>.
     @param left One item
     @param right The other item
     @return Negative int if left less than right,
     0 if left equals right,
     positive int if left > right
     @throws ClassCastException if items are not Comparable
     */
    private int compare(E left, E right) {
        if (comparator != null) { // A Comparator is defined.
            return comparator.compare(left, right);
        }
        else { // Use left's compareTo method.
            return ( (Comparable < E > ) left).compareTo(right);
        }
    }

    /**
     * Swaps the items at the position a with at the position b
     * @param a the index of the first item
     * @param b the index of the second item
     */
    public void swap(int a, int b) {
        E temp = theData.get(a);
        theData.set(a, theData.get(b));
        theData.set(b, temp);
    }

    /**
     * @return the size of the priority queue
     */
    @Override
    public int size() {
        return theData.size();
    }

    /**
     * @return an iterator over the data
     */
    @Override
    public Iterator <E> iterator() {
        Iterator<E> it = new Iterator<E>() {
            private int index = 0;
            private E lastItemReturned;

            @Override
            public boolean hasNext() {
                return index < theData.size()-1;
            }

            @Override
            public E next() {
                try {
                    lastItemReturned = theData.get(index++);
                } catch (IndexOutOfBoundsException e) {
                    lastItemReturned = null;
                }
                return lastItemReturned;
            }

            /**
             * Sets the value of the last item returned.
             * @param value The value to be set
             */
            public void setLastItemReturned(E value) {
                lastItemReturned = value;
            }
        };
        return it;
    }

    /**
     * @return the smallest item of the priority queue
     */
    @Override
    public E peek() {
        return theData.get(0);
    }

    // ADDITIONAL FEATURES

    /**
     * Searches for an element
     * @param item the item to be searched
     * @return true if the item is found in the heap
     */
    public boolean search(E item) {
        for (E element : theData) {
            if (compare(element, item) == 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * Merges the heap with the given heap.
     * @param anotherHeap the heap to be merged with.
     */
    public void merge(MyPriorityQueue<E> anotherHeap) {
        int size = anotherHeap.size();
        for (int i = 0; i < size; ++i) {
            offer(anotherHeap.poll());
        }
    }

    /**
     * Removes the ith largest element from the heap.
     * @param i specifies the which largest element to be removed.
     * @return the ith largest element
     */
    public E removeIthLargest(int i) {
        if (i < 1 || i > theData.size()) {
            return null;
        }
        i = (theData.size() + 1) - i; // ith largest element is (n+1-i)th smallest element
        ArrayList<E> temp = new ArrayList<>();
        for (int j = 0; j < i; ++j){
            temp.add(this.poll());
        }
        E ithLargest = temp.remove(temp.size()-1);
        // Offer the unnecessarily deleted items
        for (E itemToBeAdded : temp) {
            this.offer(itemToBeAdded);
        }
        return ithLargest;
    }


}


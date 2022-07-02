package com.company;

import java.lang.reflect.Array;
import java.util.Arrays;

/**
 * It's a data structure that holds data as a simple array,
 * and lets you to add and remove item from the array easily.
 *
 * @param <T> type of the items in the array.
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class MutableArray<T> implements SearchableArray<T> {
    private int capacity = 5;
    private final int INCREASE_RATE = 5;
    private int numOfElements = 0;
    private T[] arr;

    public MutableArray(Class<T> tclass) {
        arr = (T[]) Array.newInstance(tclass, capacity);
    }

    /**
     * Getter for the number of elements inside the array.
     *
     * @return the number of elements of the mutable array.
     */
    public int getNumOfElements() {
        return numOfElements;
    }

    /**
     * Gets the item in the index of the array
     *
     * @param index the index of the item to be get
     * @return the item at the index of the array
     */
    public T getItem(int index) {
        return arr[index];
    }

    /**
     * Adds a new item to the end of the array.
     * If the capacity is not enough for a new item, increases it.
     * If already an array exist it appends the given item to the array.
     *
     * @param newItem new item to be added to the array
     */
    public void addItem(T newItem) {
        try {
            if(numOfElements == capacity){
                // Increase the capacity of the array
                capacity += INCREASE_RATE;
                T[] newArr = Arrays.copyOf(arr, capacity);

                // Append the new item to the array
                newArr[numOfElements] = newItem;
                arr = newArr;
            } else {
                arr[numOfElements] = newItem;
            }
            numOfElements++;
        } catch (Exception e) {
            System.out.println(e.toString());
        }
    }

    /**
     * Removes the given item from the array.
     *
     * @param item the item to be removed
     */
    public void removeItem(T item) {
        try {
            for (int i = 0; i < numOfElements; ++i) {
                if(arr[i].equals(item)) {
                    removeIthItem(i);
                }
            }
        } catch (Exception e){
            System.out.println(e.toString());
        }
    }

    /**
     * Removes the ith item in the array.
     *
     * @param i index of the item to be removed.
     */
    private void removeIthItem(int i) {
        int j;
        for(j = i; j < numOfElements - 1; ++j) {
            arr[j] = arr[j+1];
        }
        arr[j] = null;
        numOfElements--;

        if(capacity - numOfElements == INCREASE_RATE && numOfElements != 0) {
            T[] newArr = Arrays.copyOf(arr, numOfElements);
            arr = newArr;
        }
    }

    /**
     * Searches in mutable array for given item
     *
     * @param item the item to be searched.
     * @return the true if it finds in the array, otherwise returns false.
     */
    @Override
    public boolean searchFor(T item) {
        for (int i = 0; i < numOfElements; ++i) {
            if(arr[i].equals(item)){
                return true;
            }
        }
        return false;
    }

    /**
     * Searches in mutable array for given item
     *
     * @param item the item to be searched.
     * @return the index of item if it finds in the array, otherwise returns -1.
     */
    @Override
    public int searchForIndex(T item) {
        for (int i = 0; i < numOfElements; ++i) {
            if(arr[i].equals(item)){
                return i;
            }
        }
        return -1;
    }


}

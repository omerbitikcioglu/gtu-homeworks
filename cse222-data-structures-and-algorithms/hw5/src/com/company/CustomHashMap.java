package com.company;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Random;
import java.util.Set;

/**
 * Custom HashMap class that includes custom iterator for HashMap
 *
 * @author Ömer Faruk Bitikçioğlu
 */
public class CustomHashMap < K, V >
        implements KWHashMap < K, V > {
    // Data Fields
    private Entry < K, V > [] table;
    private static final int START_CAPACITY = 101;
    private double LOAD_THRESHOLD = 0.75;
    private int numKeys;
    private int numDeletes;
    private final Entry < K, V > DELETED =
            new Entry < K, V > (null, null);

    // Constructor
    public CustomHashMap() {
        table = new Entry[START_CAPACITY];
    }

    /**
     * No parameter iterator method which generates an iterator which starts from
     * randomly selected index.
     *
     * @return the iterator which iterates over the keys of HashMap
     */
    public MapIterator iterator() { return new MapIterator(); }

    /**
     * This creates an iterator which starts from the given key.
     * If the key is not found, index is selected randomly.
     *
     * @param key The key value to be started
     * @return the iterator which iterates over the keys of HashMap
     */
    public MapIterator iterator(K key) { return new MapIterator(key); }

    /** Contains key-value pairs for a hash table. */
    private static class Entry < K, V > {

        /** The key */
        private K key;

        /** The value */
        private V value;

        /** Creates a new key-value pair.
         @param key The key
         @param value The value
         */
        public Entry(K key, V value) {
            this.key = key;
            this.value = value;
        }

        /** Retrieves the key.
         @return The key
         */
        public K getKey() {
            return key;
        }

        /** Retrieves the value.
         @return The value
         */
        public V getValue() {
            return value;
        }

        /** Sets the value.
         @param val The new value
         @return The old value
         */
        public V setValue(V val) {
            V oldVal = value;
            value = val;
            return oldVal;
        }
    }

    /** Returns the number of entries in the map */
    public int size() {
        return numKeys;
    }

    /** Returns true if empty */
    public boolean isEmpty() {
        return numKeys == 0;
    }

    /** Finds either the target key or the first empty slot in the
     search chain using linear probing.
     pre: The table is not full.
     @param key The key of the target object
     @return The position of the target or the first empty slot if
     the target is not in the table.
     */
    private int find(Object key) {
        // Calculate the starting index.
        int index = key.hashCode() % table.length;
        if (index < 0)
            index += table.length; // Make it positive.

        // Increment index until an empty slot is reached
        // or the key is found.
        while ( (table[index] != null)
                && (!key.equals(table[index].key))) {
            index++;
            // Check for wraparound.
            if (index >= table.length)
                index = 0; // Wrap around.
        }
        return index;
    }

    /** Method get for class HashtableOpen.
     @param key The key being sought
     @return the value associated with this key if found;
     otherwise, null
     */
    public V get(Object key) {
        // Find the first table element that is empty
        // or the table element that contains the key.
        int index = find(key);

        // If the search is successful, return the value.
        if (table[index] != null)
            return table[index].value;
        else
            return null; // key not found.
    }

    /** Method put for class HashtableOpen.
     post: This key-value pair is inserted in the
     table and numKeys is incremented. If the key is already
     in the table, its value is changed to the argument
     value and numKeys is not changed. If the LOAD_THRESHOLD
     is exceeded, the table is expanded.
     @param key The key of item being inserted
     @param value The value for this key
     @return Old value associated with this key if found;
     otherwise, null
     */
    public V put(K key, V value) {
        // Find the first table element that is empty
        // or the table element that contains the key.
        int index = find(key);

        // If an empty element was found, insert new entry.
        if (table[index] == null) {
            table[index] = new Entry < K, V > (key, value);
            numKeys++;
            // Check whether rehash is needed.
            double loadFactor =
                    (double) (numKeys + numDeletes) / table.length;
            if (loadFactor > LOAD_THRESHOLD)
                rehash();
            return null;
        }

        // assert: table element that contains the key was found.
        // Replace value for this key.
        V oldVal = table[index].value;
        table[index].value = value;
        return oldVal;
    }

    /** Expands table size when loadFactor exceeds LOAD_THRESHOLD
     post: The size of table is doubled and is an odd integer.
     Each nondeleted entry from the original table is
     reinserted into the expanded table.
     The value of numKeys is reset to the number of items
     actually inserted; numDeletes is reset to 0.
     */
    private void rehash() {
        // Save a reference to oldTable.
        Entry < K, V > [] oldTable = table;
        // Double capacity of this table.
        table = new Entry[2 * oldTable.length + 1];

        // Reinsert all items in oldTable into expanded table.
        numKeys = 0;
        numDeletes = 0;
        for (int i = 0; i < oldTable.length; i++) {
            if ( (oldTable[i] != null) && (oldTable[i] != DELETED)) {
                // Insert entry in expanded table
                put(oldTable[i].key, oldTable[i].value);
            }
        }
    }

    /** Remove the item with a given key value
     *  @param key The key to be removed
     *  @return The value associated with this key, or null
     *  if the key is not in the table.
     */
    public V remove(Object key) {
        int index = find(key);
        if (table[index] == null)
            return null;
        V oldValue = table[index].value;
        table[index] = DELETED;
        numKeys--;
        numDeletes++;
        return oldValue;
    }

    private class MapIterator implements Iterator<K> {

        /** The index of the iterator*/
        private int index;

        /** The array to check if the item is iterated over */
        private final boolean[] isIterated = new boolean[size()];

        private K[] keys = (K[]) new Object[size()];

        /**
         * Zero parameter constructor.
         * Starting index is randomly selected.
         */
        public MapIterator(){
            Random random = new Random();
            index = random.nextInt(size());
            findKeys();
        }

        /**
         * The iterator starts from the given key.
         * If the key is not found, it starts from a random index.
         */
        public MapIterator(K key){
            boolean found = false;
            findKeys();
            for (int i = 0; i < size() && !found; ++i) {
                if (keys[i].equals(key)) {
                    found = true;
                    index = i;
                }
            }
            if (!found) {
                Random random = new Random();
                index = random.nextInt(size());
            }
        }

        /** Finds the keys of the table */
        private void findKeys() {
            for (int i = 0, j = 0; i < table.length; ++i) {
                if(table[i] != null) {
                    keys[j++] = table[i].getKey();
                }
            }
        }

        /**
         * The method returns true if there are still not-iterated key/s in the Map,
         * otherwise returns false.
         *
         * @return true if the iteration has more elements
         */
        @Override
        public boolean hasNext() {
            for (int i = 0; i < size(); ++i){
                if (!isIterated[i])
                    return true;
            }
            return false;
        }

        /**
         * The function returns the next key in the Map.
         * It returns the first key when there is no not-iterated key in the Map.
         *
         * @return the next element in the iteration
         */
        @Override
        public K next() {
            K nextKey = keys[index];
            isIterated[index] = true;
            index = (index + 1) % size();
            return nextKey;
        }

        /**
         * The iterator points to the previous key in the Map.
         * It returns the last key when the iterator is at the first key.
         *
         * @return the previous element in the iteration
         */
        public K prev() {
            if (index == 0) {
                index = size()-1;
            } else {
                index--;
            }
            isIterated[index] = true;
            return table[index].getKey();
        }
    }
}


package com.company;

import java.util.HashMap;
import java.util.Iterator;
import java.util.TreeSet;

/**
 * Hash table implementation using chaining.
 * Table holds the entries as TreeSet
 *
 * @author Koffman and Wolfgang
 * @author Ömer Faruk Bitikçioğlu
 */

public class HashtableChainTreeSet<K extends Comparable<K>, V>
        implements KWHashMap<K, V> {
    /** The table */
    private TreeSet< Entry <K, V>>[] table;

    /** The number of keys */
    private int numKeys;

    /** The capacity */
    private static final int CAPACITY = 101;

    /** The maximum load factor */
    private static final double LOAD_THRESHOLD = 3.0;

    /** Contains key-value pairs for a hash table. */
    private static class Entry < K extends Comparable<K>, V > implements Comparable<Entry<K,V>>{

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

        @Override
        public int compareTo(Entry<K, V> o) {
            return key.compareTo(o.key);
        }
    }

    // Constructor
    public HashtableChainTreeSet() {
        table = new TreeSet[CAPACITY];
    }

    /** Method get for class HashtableChain.
     @param key The key being sought
     @return The value associated with this key if found;
     otherwise, null
     */
    public V get(Object key) {
        int index = key.hashCode() % table.length;
        if (index < 0)
            index += table.length;
        if (table[index] == null)
            return null; // key is not in the table.

        // Search the list at table[index] to find the key.
        for (Entry < K, V > nextItem : table[index]) {
            if (nextItem.key.equals(key))
                return nextItem.value;
        }

        // assert: key is not in the table.
        return null;
    }

    /** Method put for class HashtableChain.
     post: This key-value pair is inserted in the
     table and numKeys is incremented. If the key is already
     in the table, its value is changed to the argument
     value and numKeys is not changed.
     @param key The key of item being inserted
     @param value The value for this key
     @return The old value associated with this key if
     found; otherwise, null
     */
    public V put(K key, V value) {
        int index = key.hashCode() % table.length;
        if (index < 0)
            index += table.length;
        if (table[index] == null) {
            // Create a new linked list at table[index].
            table[index] = new TreeSet < Entry < K, V >> ();
        }

        // Search the list at table[index] to find the key.
        for (Entry < K, V > nextItem : table[index]) {
            // If the search is successful, replace the old value.
            if (nextItem.key.equals(key)) {
                // Replace value for this key.
                V oldVal = nextItem.value;
                nextItem.setValue(value);
                return oldVal;
            }
        }

        // assert: key is not in the table, add new item.
        table[index].add(new Entry < K, V > (key, value));
        numKeys++;
        if (numKeys > (LOAD_THRESHOLD * table.length))
            rehash();
        return null;
    }

    /** Returns the number of entries in the map */
    public int size() {
        return numKeys;
    }

    /** Returns true if empty */
    public boolean isEmpty() {
        return numKeys == 0;
    }


    /**
     * Finds the entry according to the key in the table and removes it.
     * To be able to search and remove in linked list, it uses the iterator of the list.
     *
     * @param key the key of the entry to be removed
     * @return the value of the removed entry
     */
    public V remove(Object key) {
        int index = key.hashCode() % table.length;
        if (index < 0)
            index += table.length;
        if (table[index] == null)
            return null; // Key not in table
        Iterator < Entry < K, V >> iter = table[index].iterator();
        while (iter.hasNext()) {
            Entry < K, V > nextItem = iter.next();
            // If the search is successful, return the value.
            if (nextItem.key.equals(key)) {
                V returnValue = nextItem.value;
                iter.remove();
                return returnValue;
            }
        }
        return null; // Key not in table
    }

    /**
     * Expands table size when loadFactor exceeds LOAD_THRESHOLD
     * post: the size of table is doubled and is an
     * odd integer. Each non-deleted entry from the original
     * table is reinserted into the expanded table.
     * The value of numKeys is reset to the number of items
     * actually inserted; numDeletes is reset to 0.
     */
    public void rehash() {
        // Save a reference to oldTable
        TreeSet< Entry < K, V >>[] oldTable = table;
        // Double capacity of this table
        table = new TreeSet[2 * oldTable.length + 1];

        // Reinsert all items in oldTable into expanded table.
        numKeys = 0;
        for (int i = 0; i < oldTable.length; i++) {
            if (oldTable[i] != null) {
                for (Entry < K, V > nextEntry : oldTable[i]) {
                    // Insert entry in expanded table
                    put(nextEntry.key, nextEntry.value);
                }
            }
        }
    }
}

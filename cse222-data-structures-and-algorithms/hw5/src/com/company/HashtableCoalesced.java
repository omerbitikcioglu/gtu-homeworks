package com.company;

/**
 * Hash table implementation of Coalesced technique
 * This technique uses the concept of Open Addressing
 * to find first empty place for colliding element
 * by using the quadratic probing and the concept
 * of Separate Chaining to link the colliding elements
 * to each other through pointers (indices in the table).
 * The deletion of a key is performed by linking its
 * next entry to the entry that points the deleted key
 * by replacing deleted entry by the next entry.
 *
 * @author Ömer Faruk Bitikçioğlu
 */
public class HashtableCoalesced < K, V >
        implements KWHashMap < K, V > {
    // Data Fields
    private Entry < K, V > [] table;
    private static final int START_CAPACITY = 101;
    private double LOAD_THRESHOLD = 0.75;
    private int numKeys;

    // Constructor
    public HashtableCoalesced() {
        table = new Entry[START_CAPACITY];
    }

    /** Contains key-value pairs for a hash table. */
    private static class Entry < K, V > {

        /** The key */
        private final K key;

        /** The value */
        private V value;

        /** The index of the next entry with the same hash value */
        private Integer nextIndex = null;

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

    /**
     * Finds either the target key or the first empty slot in the
     * search chain using quadratic probing.
     * pre: The table is not full.
     * @param key The key of the target object
     * @return The position of the target or the first empty slot if
     * the target is not in the table.
     */
    private int find(Object key) {
        // Calculate the starting index.
        int index = key.hashCode() % table.length;
        if (index < 0)
            index += table.length; // Make it positive.

        // Check if the given key present in the table
        int foundIndex = isPresent(key);
        if (foundIndex != -1) {
            return foundIndex;
        }

        // Increment index until an empty slot is reached,
        // and bound the colliding entries
        int oldIndex = index;
        int collisions = 0;
        int c = 1; // Quadratic increase rate
        while (table[index] != null) {
            collisions++;
            index += c;
            c += 2;
            // Check for wraparound.
            if (index >= table.length)
                index = index % table.length; // Wrap around.
        }
        if (collisions != 0) {
            for (int i = 0; i < collisions-1; ++i) {
                oldIndex = table[oldIndex].nextIndex;
            }
            table[oldIndex].nextIndex = index;
        }

        return index;
    }

    /**
     * Checks if the given key is present in the table
     * @param key The key of the target object
     * @return the index of the entry if it exists,
     * otherwise it returns -1
     */
    private int isPresent(Object key) {
        // Calculate the starting index.
        int index = key.hashCode() % table.length;
        if (index < 0)
            index += table.length; // Make it positive.

        if ((table[index] != null)) {
            if (key.equals(table[index].key)){
                return index;
            } else {
                int newIndex = index;
                while (table[newIndex].nextIndex != null) {
                    newIndex = table[newIndex].nextIndex;
                    if (key.equals(table[newIndex].key)){
                        return index;
                    }
                }
            }
        }
        return -1;
    }

    /**
     * Method get for class HashtableCoalesced.
     * @param key The key being sought
     * @return the value associated with this key if found;
     * otherwise, null
     */
    public V get(Object key) {
        // Find the first table element that contains the key.
        int index = isPresent(key);
        // If the search is successful, return the value.
        if (index != -1)
            return table[index].value;
        else
            return null; // key not found.
    }

    /** Method put for class HashtableCoalesced.
     * post: This key-value pair is inserted in the
     * table and numKeys is incremented. If the key is already
     * in the table, its value is changed to the argument
     * value and numKeys is not changed. If the LOAD_THRESHOLD
     * is exceeded, the table is expanded.
     * @param key The key of item being inserted
     * @param value The value for this key
     * @return Old value associated with this key if found;
     * otherwise, null
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
                    (double) numKeys / table.length;
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

    /**
     * Expands table size when loadFactor exceeds LOAD_THRESHOLD
     * post: The size of table is doubled and is an odd integer.
     * Each undeleted entry from the original table is
     * reinserted into the expanded table.
     * The value of numKeys is reset to the number of items
     * actually inserted; numDeletes is reset to 0.
     */
    private void rehash() {
        // Save a reference to oldTable.
        Entry < K, V > [] oldTable = table;
        // Double capacity of this table.
        table = new Entry[2 * oldTable.length + 1];

        // Reinsert all items in oldTable into expanded table.
        numKeys = 0;
        for (int i = 0; i < oldTable.length; i++) {
            if (oldTable[i] != null) {
                // Insert entry in expanded table
                put(oldTable[i].key, oldTable[i].value);
            }
        }
    }

    /**
     * Remove the item with a given key value.
     * The deletion of a key is performed by linking
     * its next entry to the entry that points the
     * deleted key by replacing deleted entry by the next entry.
     *
     * @param key The key to be removed
     * @return The value associated with this key, or null
     * if the key is not in the table.
     */
    public V remove(Object key) {
        int index = isPresent(key);
        if (index == -1)
            return null;
        V oldValue = table[index].value;
        Integer nextIndex = table[index].nextIndex;
        while (nextIndex != null) {
            table[index] = table[nextIndex];
            index = nextIndex;
            nextIndex = table[index].nextIndex;
        }
        numKeys--;
        return oldValue;
    }
}

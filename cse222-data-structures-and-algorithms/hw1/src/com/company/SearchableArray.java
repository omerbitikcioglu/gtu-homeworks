package com.company;

/**
 * Searchable arrays must implement searching methods.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public interface SearchableArray<T>  {
    /**
     * @param item the item to be searched.
     * @return true if the item is found.
     */
    public boolean searchFor(T item);

    /**
     * @param item the item to be searched.
     * @return the index of the item.
     */
    public int searchForIndex(T item);
}

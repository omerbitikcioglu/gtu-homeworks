#ifndef __MYOS__SORTING_H
#define __MYOS__SORTING_H

#include <paging.h>
#include <memorymanagement.h>

namespace myos
{
    enum SortingAlgorithm
    {
        BubbleSort,
        InsertionSort,
        QuickSort
    };

    class Sorting
    {
    private:
        // Swaps the page values
        void swap(PageTableElement e1, PageTableElement e2);

        // Function to split the array into two halves during quick sort
        int partition(Paging *paging, int beg, int end);

        // Function to called recursively partition itself
        void quick_sort(Paging *paging, int low, int high);

    public:
        // Function to perform bubble sort
        void bubbleSort(Paging *paging);

        // Function to perform insertion sort
        void insertionSort(Paging *paging);

        // Function to perform quick sort
        void quickSort(Paging *paging);

        // Main sorting program
        static void sortingProgram(double arrRelativeSize,
                                   SortingAlgorithm sortAlgo,
                                   PageReplacementAlgorithm prAlgo,
                                   bool isSorted,
                                   int pm[], int physicalMemorySize,
                                   int disk[], int diskSize,
                                   int arr[], int arraySize);
    };

}

#endif
#ifndef __MYOS__PAGING_H
#define __MYOS__PAGING_H

namespace myos
{
    enum PageReplacementAlgorithm
    {
        FIFO,
        SecondChance,
        LRU
    };

    typedef struct PageTableElement
    {
        bool present = false;
        bool referanced = false;
        int *pageFrameAddress;
        int *diskAddress;
    } PageTableElement;

    // A class to store a queue
    class Queue
    {
        PageTableElement **arr; // array to store queue elements
        int capacity;           // maximum capacity of the queue
        int front;              // front points to the front element in the queue (if any)
        int rear;               // rear points to the last element in the queue
        int count;              // current size of the queue

    public:
        Queue();
        Queue(int size);
        ~Queue();

        PageTableElement *dequeue();
        void enqueue(PageTableElement *x);
        PageTableElement *peek();
        int size();
        int maxSize();
        bool isEmpty();
        bool isFull();
    };

    class RecentlyUsedPages
    {
    private:
        PageTableElement **arr;
        int capacity; // Array capacity
        int size;     // Array size

    public:
        RecentlyUsedPages();
        RecentlyUsedPages(int size);
        ~RecentlyUsedPages();

        // Finds and shifts the recently used page to the end
        void pageRecentlyUsed(PageTableElement *e);

        // Add a new page to the lru pages
        void addPage(PageTableElement *e);

        // Gets the least recently used page
        PageTableElement *getLRUPage();

        // Check if the array is full
        bool isFull();
    };

    class Paging
    {
    private:
        int pageTableSize;
        PageTableElement *pageTable;
        PageReplacementAlgorithm pageReplacementAlgorithm;
        Queue pageQueue;            // Holds the used pages in memory
        RecentlyUsedPages lruPages; // To find lru
        int **pmFreeSpaces;         // Holds the free pages in memory

        // Statistics
        int pageLoad = 0, pageWriteBack = 0;
        int hitCount = 0, missCount = 0;

        // Page Replacement
        PageTableElement *pageReplacement(int pageIndex);

        // Page Replacement Algorithms
        PageTableElement *fifo(int pageIndex);
        PageTableElement *secondChance(int pageIndex);
        PageTableElement *lru(int pageIndex);

        // Finds and returns the free page address in memory
        int *findFreeSpaceInMemory();

        // Write and load operations on disk
        void writeBackToDisk(PageTableElement *e);
        void loadPageFromDisk(int *free, int pageIndex);

        // Increments
        void incrementHitCount(int amount = 1);
        void incrementMissCount(int amount = 1);
        void incrementPageLoad(int amount = 1);
        void incrementPageWriteBack(int amount = 1);

    public:
        Paging(PageTableElement pageTable[], int pageTableSize, PageReplacementAlgorithm prAlgo, int **pmFreeSpaces, int pmSize);
        ~Paging();

        // Checks a page is present in pm, if not places it
        void checkPage(PageTableElement *e, int pageIndex);

        // Getters
        PageTableElement *getPageTable();
        int getPageTableSize();
        PageReplacementAlgorithm getPageReplacementAlgorithm();
        int getMissCount();
        int getHitCount();
        int getPageLoad();
        int getPageWriteBack();
    };
}

#endif
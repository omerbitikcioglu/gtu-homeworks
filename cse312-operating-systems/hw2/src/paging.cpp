#include <paging.h>
#include <common/types.h>

using namespace myos;
using namespace myos::common;

void printf(char *str);
void printAddress(char *addressName, size_t address);
void printDecimal(int key);

Paging::Paging(PageTableElement pageTable[], int pageTableSize,
               PageReplacementAlgorithm prAlgo, int **pmFreeSpaces,
               int pmSize) : pageQueue(pmSize), lruPages(pmSize)
{
    Paging::pageTable = pageTable;
    Paging::pageTableSize = pageTableSize;
    Paging::pageReplacementAlgorithm = prAlgo;
    Paging::pmFreeSpaces = pmFreeSpaces;
}

Paging::~Paging()
{
    delete[] pageTable;
}

void printf(char *str);

// Finds the page that needs to be replaced
PageTableElement *Paging::pageReplacement(int pageIndex)
{
    PageTableElement *pageReplaced = 0;

    switch (pageReplacementAlgorithm)
    {
    case PageReplacementAlgorithm::FIFO:
        pageReplaced = fifo(pageIndex);
        break;

    case PageReplacementAlgorithm::SecondChance:
        pageReplaced = secondChance(pageIndex);
        break;

    case PageReplacementAlgorithm::LRU:
        pageReplaced = lru(pageIndex);
        break;

    default:
        break;
    }

    return pageReplaced;
}

// Checks a page is present in pm, if not places it
void Paging::checkPage(PageTableElement *e, int pageIndex)
{
    if (e->present) // Hit
    {
        incrementHitCount();

        // // For the demo
        // printDecimal(*e->pageFrameAddress);
        // printf(" is hit since it is already in the frame.\n");

        switch (pageReplacementAlgorithm)
        {
        case PageReplacementAlgorithm::SecondChance:
            e->referanced = true;
            break;
        case PageReplacementAlgorithm::LRU:
            lruPages.pageRecentlyUsed(e);
            break;
        default:
            break;
        }
    }
    else // Miss
    {
        incrementMissCount();

        PageTableElement *replacedElement = 0;
        replacedElement = pageReplacement(pageIndex);

        // // For the demo
        // if (replacedElement != 0)
        // {
        //     printDecimal(*e->pageFrameAddress);
        //     printf(" is missed so it is replaced with ");
        //     printDecimal(*replacedElement->diskAddress);
        //     printf(" in the frame.\n");
        // }
        // else
        // {
        //     printDecimal(*e->pageFrameAddress);
        //     printf(" is missed so it is placed to a free space in memory\n");
        // }
    }
}

void printDecimal(int key);
void printAddress(char *addressName, size_t address);

int *Paging::findFreeSpaceInMemory()
{
    int *free = 0;
    bool found = 0;
    int pmSize = pageQueue.maxSize(); // PM and queue has the same size
    for (size_t i = 0; i < pmSize && !found; i++)
    {
        if (pmFreeSpaces[i] != 0)
        {
            free = pmFreeSpaces[i];
            pmFreeSpaces[i] = 0; // The free space is allocated
            found = 1;
        }
    }
    if (!found)
        printf("Fatal Error: Couldn't find free space in memory!\n");

    return free;
}

// Write back the page to the disk
void Paging::writeBackToDisk(PageTableElement *e)
{
    e->present = false;
    e->referanced = false;
    *(e->diskAddress) = *(e->pageFrameAddress); // Write the value to the disk
    e->pageFrameAddress = e->diskAddress;
    incrementPageWriteBack();
}

// Load the page from disk to the free space
void Paging::loadPageFromDisk(int *free, int pageIndex)
{
    *free = *(pageTable[pageIndex].diskAddress); // Write the value from disk
    pageTable[pageIndex].pageFrameAddress = free;
    pageTable[pageIndex].present = true;
    pageTable[pageIndex].referanced = false;

    if (pageReplacementAlgorithm == PageReplacementAlgorithm::FIFO ||
        pageReplacementAlgorithm == PageReplacementAlgorithm::SecondChance)
        pageQueue.enqueue(&pageTable[pageIndex]);
    else
        lruPages.addPage(&pageTable[pageIndex]);

    incrementPageLoad();
}

PageTableElement *Paging::fifo(int pageIndex)
{
    int *free;
    PageTableElement *pageToBeReplaced = 0;
    // If the physical memory is full, do page replacement
    if (pageQueue.isFull())
    {
        // Oldest page is the front of the queue
        pageToBeReplaced = pageQueue.dequeue();
        free = pageToBeReplaced->pageFrameAddress;
        writeBackToDisk(pageToBeReplaced);
    }
    // If the physical memory has free space
    else
        free = findFreeSpaceInMemory();

    loadPageFromDisk(free, pageIndex);
    return pageToBeReplaced;
}

PageTableElement *Paging::secondChance(int pageIndex)
{
    int *free;
    PageTableElement *pageToBeReplaced = 0;

    // If the physical memory is full, do page replacement
    if (pageQueue.isFull())
    {
        // Find the oldest and not referanced page
        bool found = 0;
        for (size_t i = 0; i < pageQueue.size() && !found; i++)
        {
            pageToBeReplaced = pageQueue.dequeue();
            if (pageToBeReplaced->referanced == true)
            {
                pageToBeReplaced->referanced = false;
                pageQueue.enqueue(pageToBeReplaced);
            }
            else
                found = 1;
        }

        // Every page used its second chance, normal FIFO
        if (!found)
            pageToBeReplaced = pageQueue.dequeue();

        free = pageToBeReplaced->pageFrameAddress;
        writeBackToDisk(pageToBeReplaced);
    }
    // If the physical memory has free space
    else
        free = findFreeSpaceInMemory();

    loadPageFromDisk(free, pageIndex);
    return pageToBeReplaced;
}

PageTableElement *Paging::lru(int pageIndex)
{
    int *free;
    PageTableElement *pageToBeReplaced = 0;

    // If the physical memory is full, do page replacement
    if (lruPages.isFull())
    {
        // Find least recently used page in memory
        pageToBeReplaced = lruPages.getLRUPage();
        free = pageToBeReplaced->pageFrameAddress;
        writeBackToDisk(pageToBeReplaced);
    }
    else
        free = findFreeSpaceInMemory();

    loadPageFromDisk(free, pageIndex);
    return pageToBeReplaced;
}

void Paging::incrementHitCount(int amount)
{
    hitCount += amount;
}

void Paging::incrementMissCount(int amount)
{
    missCount += amount;
}

void Paging::incrementPageLoad(int amount)
{
    pageLoad += amount;
}

void Paging::incrementPageWriteBack(int amount)
{
    pageWriteBack += amount;
}

PageTableElement *Paging::getPageTable()
{
    return pageTable;
}

int Paging::getPageTableSize()
{
    return pageTableSize;
}

PageReplacementAlgorithm Paging::getPageReplacementAlgorithm()
{
    return pageReplacementAlgorithm;
}

int Paging::getMissCount()
{
    return missCount;
}

int Paging::getHitCount()
{
    return hitCount;
}

int Paging::getPageLoad()
{
    return pageLoad;
}

int Paging::getPageWriteBack()
{
    return pageWriteBack;
}

#include <paging.h>
#include <common/types.h>

using namespace myos;
using namespace myos::common;

RecentlyUsedPages::RecentlyUsedPages() {}

RecentlyUsedPages::RecentlyUsedPages(int size)
{
    RecentlyUsedPages::arr = new PageTableElement *[size];
    RecentlyUsedPages::capacity = size;
    RecentlyUsedPages::size = 0;
}

RecentlyUsedPages::~RecentlyUsedPages()
{
    delete[] arr;
}

// Finds and shifts the recently used page to the end
void RecentlyUsedPages::pageRecentlyUsed(PageTableElement *e)
{
    PageTableElement *temp;
    bool found = false;
    for (size_t i = 0; i < size && !found; i++)
    {
        if (arr[i] == e)
        {
            found = true;
            // Shift pages
            temp = arr[i];
            size_t j;
            for (j = i; j < size - 1; j++)
                arr[j] = arr[j + 1];

            arr[j] = temp;
        }
    }
}

// Add a new page to the lru pages
void RecentlyUsedPages::addPage(PageTableElement *e)
{
    bool found = false;
    for (size_t i = 0; i < capacity && !found; i++)
    {
        if (arr[i] == 0)
        {
            found = true;
            arr[i] = e;
            size++;
        }
    }
}

// Gets the least recently used page
PageTableElement *RecentlyUsedPages::getLRUPage()
{
    PageTableElement *lruPage = arr[0];

    // Delete the lru page from the array
    size_t i;
    for (i = 0; i < size - 1; i++)
        arr[i] = arr[i + 1];
    arr[i] = 0;
    size--;

    return lruPage;
}

bool RecentlyUsedPages::isFull()
{
    return size == capacity;
}

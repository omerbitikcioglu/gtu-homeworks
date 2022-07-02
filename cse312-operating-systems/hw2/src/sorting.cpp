#include <sorting.h>
#include <common/types.h>

using namespace myos;
using namespace myos::common;

void printDecimal(int key);
void printf(char *str);

void printArray(char *str, size_t n, int *arr)
{
	printf(str);
	printf(": [");
	for (size_t i = 0; i < n; i++)
	{
		printDecimal(*(arr + i));
		if (i != n - 1)
		{
			printf(", ");
		}
	}
	printf("]\n");
}

void printAddress(char *addressName, size_t address);

void printStuff(SortingAlgorithm sortAlgo, Paging paging,
				int *unsortedArray, int arraySize,
				int *pm, int pmSize, int *disk, int diskSize)
{
	PageTableElement *pageTable = paging.getPageTable();
	PageReplacementAlgorithm prAlgo = paging.getPageReplacementAlgorithm();

	// Print out starting and ending addresses of PM and Disk
	printAddress("pm", (size_t)pm);
	printAddress("pm_end", (size_t)(pm + pmSize));
	printAddress("disk", (size_t)disk);
	printAddress("disk_end", (size_t)(disk + diskSize));
	printf("\n");

	// Print sorting algorithm
	printf("Sorting Algorithm: ");
	switch (sortAlgo)
	{
	case SortingAlgorithm::BubbleSort:
		printf("BubbleSort\n");
		break;

	case SortingAlgorithm::InsertionSort:
		printf("Insertion Sort\n");
		break;

	case SortingAlgorithm::QuickSort:
		printf("Quick Sort\n");
		break;

	default:
		break;
	}

	// Print pr algorithm
	printf("Page Replacement Algorithm: ");
	switch (paging.getPageReplacementAlgorithm())
	{
	case PageReplacementAlgorithm::FIFO:
		printf("FIFO\n");
		break;

	case PageReplacementAlgorithm::SecondChance:
		printf("SecondChance\n");
		break;

	case PageReplacementAlgorithm::LRU:
		printf("LRU\n");
		break;

	default:
		break;
	}

	// Print the unsorted array
	printf("Unsorted Array: [");
	for (size_t i = 0; i < arraySize; i++)
	{
		printDecimal(unsortedArray[i]);
		if (i != arraySize - 1)
		{
			printf(", ");
		}
	}
	printf("]\n");

	// Print the sorted array
	printf("Sorted Array: [");
	for (size_t i = 0; i < arraySize; i++)
	{
		printDecimal(*(pageTable[i].pageFrameAddress));
		if (i != arraySize - 1)
		{
			printf(", ");
		}
	}
	printf("]\n");

	printArray("Physical Memory", pmSize, pm);
	printArray("Disk", arraySize, disk);

	printf("Hit count: ");
	printDecimal(paging.getHitCount());
	printf("\n");

	printf("Miss count: ");
	printDecimal(paging.getMissCount());
	printf("\n");

	printf("Page load: ");
	printDecimal(paging.getPageLoad());
	printf("\n");

	printf("Page write back: ");
	printDecimal(paging.getPageWriteBack());
	printf("\n");
}

// This is the method of sorting by which the array element
// are interchanged within its relative values
void Sorting::bubbleSort(Paging *paging)
{
	PageTableElement *pageTable = paging->getPageTable();
	PageReplacementAlgorithm prAlgo = paging->getPageReplacementAlgorithm();

	int i, j;
	int arraySize = paging->getPageTableSize();
	for (i = 0; i < arraySize - 1; i++)
	{
		for (j = 0; j < arraySize - i - 1; j++)
		{
			paging->checkPage(&pageTable[j], j);
			paging->checkPage(&pageTable[j + 1], j + 1);

			if (*pageTable[j].pageFrameAddress > *pageTable[j + 1].pageFrameAddress)
				swap(pageTable[j], pageTable[j + 1]);
		}
	}
}

void Sorting::swap(PageTableElement e1, PageTableElement e2)
{
	int value1 = *(e1.pageFrameAddress);
	int value2 = *(e2.pageFrameAddress);

	*(e1.pageFrameAddress) = value2;
	*(e2.pageFrameAddress) = value1;
}

// Function is used to perform insertion sort
void Sorting::insertionSort(Paging *paging)
{
	PageTableElement *pageTable = paging->getPageTable();
	PageReplacementAlgorithm prAlgo = paging->getPageReplacementAlgorithm();

	int i, j, key;
	int arraySize = paging->getPageTableSize();

	for (i = 1; i < arraySize; i++)
	{
		paging->checkPage(&pageTable[i], i);
		key = *(pageTable[i].pageFrameAddress);

		paging->checkPage(&pageTable[i - 1], i - 1);

		for (j = i; key < *(pageTable[j - 1].pageFrameAddress) && j > 0; j--)
		{
			if (j != i)
			{
				paging->checkPage(&pageTable[j], j);
				paging->checkPage(&pageTable[j - 1], j - 1);
			}

			*(pageTable[j].pageFrameAddress) = *(pageTable[j - 1].pageFrameAddress);
		}
		*(pageTable[j].pageFrameAddress) = key;
	}
}

// This function is used to perform the quick sort
void Sorting::quickSort(Paging *paging)
{
	// For quick sort
	quick_sort(paging, 0, paging->getPageTableSize() - 1);
}

// This function performs the partition changing in the array
// by the quick sort method
void Sorting::quick_sort(Paging *paging, int low, int high)
{
	int pivot;

	if (low < high)
	{
		pivot = partition(paging, low, high);
		quick_sort(paging, low, pivot - 1);
		quick_sort(paging, pivot + 1, high);
	}
}

// Function to perfrom the partition in the array for quick sort
int Sorting::partition(Paging *paging, int low, int high)
{
	PageTableElement *pageTable = paging->getPageTable();

	paging->checkPage(&pageTable[high], high);
	int pivot = *pageTable[high].pageFrameAddress;

	int i = (low - 1); // Index of smaller element and indicates the right position of pivot found so far

	for (int j = low; j <= high - 1; j++)
	{
		paging->checkPage(&pageTable[j], j);
		// If current element is smaller than the pivot
		if (*pageTable[j].pageFrameAddress < pivot)
		{
			i++; // increment index of smaller element
			paging->checkPage(&pageTable[i], i);
			swap(pageTable[i], pageTable[j]);
		}
	}
	paging->checkPage(&pageTable[i + 1], i + 1);
	paging->checkPage(&pageTable[high], high);
	swap(pageTable[i + 1], pageTable[high]);

	return (i + 1); // Pivot position
}

/* Main array sorting program
 *
 * Array size is relative to physical memory size.
 * if arrRelativeSize = 2 then, 2*physicalMemorymSize
 *
 **/

void printf(char *str);
void printAddress(char *addressName, size_t address);
void printDecimal(int key);

void Sorting::sortingProgram(double arrRelativeSize,
							 SortingAlgorithm sortAlgo,
							 PageReplacementAlgorithm prAlgo,
							 bool isSorted,
							 int pm[], int physicalMemorySize,
							 int disk[], int diskSize,
							 int arr[], int arraySize)
{
	if (arrRelativeSize <= 1) // Array size must be bigger than physical memory size
	{
		printf("The array size must be bigger than physical memory size!");
		return;
	}
	else if (isSorted)
	{
		printf("The array is already sorted!");
		return;
	}

	// Place all the array elements to the disk and referance from a page table
	PageTableElement pageTable[arraySize];
	int *pageFrameAddress, *diskAddress;
	for (size_t i = 0; i < arraySize; i++)
	{
		pageFrameAddress = &disk[i];
		diskAddress = pageFrameAddress;
		*diskAddress = arr[i];

		// Pages are absent in memory at first
		pageTable[i] = {false, false, pageFrameAddress, diskAddress};
	}

	// Get the addresses of the free memory pages
	int **pmFreeSpaces = new int *[physicalMemorySize];
	for (size_t i = 0; i < physicalMemorySize; i++)
		pmFreeSpaces[i] = &pm[i];

	// Paging for the sorting process
	Paging sortingPaging(pageTable, arraySize, prAlgo, pmFreeSpaces, physicalMemorySize);

	// Sorting part
	Sorting sorting;
	switch (sortAlgo)
	{
	case (SortingAlgorithm::BubbleSort): // Bubble sort
		sorting.bubbleSort(&sortingPaging);
		break;

	case (SortingAlgorithm::InsertionSort): // Insertion sort
		sorting.insertionSort(&sortingPaging);
		break;

	case (SortingAlgorithm::QuickSort): // Quick sort
		sorting.quickSort(&sortingPaging);
		break;

	default:
		break;
	}

	printStuff(sortAlgo, sortingPaging, arr, arraySize, pm, physicalMemorySize, disk, diskSize);
}

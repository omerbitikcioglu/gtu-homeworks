#include <paging.h>

using namespace myos;

// Default ctor
Queue::Queue()
{
}

// Constructor to initialize a queue
Queue::Queue(int size)
{
    arr = new PageTableElement *[size];
    capacity = size;
    front = 0;
    rear = -1;
    count = 0;
}

// Destructor to free memory allocated to the queue
Queue::~Queue()
{
    delete[] arr;
}

// Utility function to dequeue the front element
PageTableElement *Queue::dequeue()
{
    // check for queue underflow
    if (isEmpty())
    {
        return 0;
    }

    PageTableElement *x = arr[front];

    front = (front + 1) % capacity;
    count--;

    return x;
}

// Utility function to add an item to the queue
void Queue::enqueue(PageTableElement *item)
{
    // Check for queue overflow
    if (isFull())
    {
        return;
    }

    rear = (rear + 1) % capacity;
    arr[rear] = item;
    count++;
}

// Utility function to return the front element of the queue
PageTableElement *Queue::peek()
{
    if (isEmpty())
    {
        return 0;
    }
    return arr[front];
}

// Utility function to return the size of the queue
int Queue::size()
{
    return count;
}

// Utility function to return the capacity of the queue
int Queue::maxSize()
{
    return capacity;
}

// Utility function to check if the queue is empty or not
bool Queue::isEmpty()
{
    return (size() == 0);
}

// Utility function to check if the queue is full or not
bool Queue::isFull()
{
    return (size() == capacity);
}
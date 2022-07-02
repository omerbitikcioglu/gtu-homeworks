#include <common/types.h>
#include <gdt.h>
#include <memorymanagement.h>
#include <hardwarecommunication/interrupts.h>
#include <syscalls.h>
#include <hardwarecommunication/pci.h>
#include <drivers/driver.h>
#include <drivers/keyboard.h>
#include <drivers/mouse.h>
#include <drivers/vga.h>
#include <drivers/ata.h>
#include <gui/desktop.h>
#include <gui/window.h>
#include <multitasking.h>
#include <paging.h>
#include <sorting.h>
#include <paging.h>
#include <drivers/amd_am79c973.h>
#include <net/etherframe.h>
#include <net/arp.h>
#include <net/ipv4.h>
#include <net/icmp.h>
#include <net/udp.h>
#include <net/tcp.h>

using namespace myos;
using namespace myos::common;
using namespace myos::drivers;
using namespace myos::hardwarecommunication;
using namespace myos::gui;
using namespace myos::net;

void printf(char *str)
{
    static uint16_t *VideoMemory = (uint16_t *)0xb8000;

    static uint8_t x = 0, y = 0;

    for (int i = 0; str[i] != '\0'; ++i)
    {
        switch (str[i])
        {
        case '\n':
            x = 0;
            y++;
            break;
        default:
            VideoMemory[80 * y + x] = (VideoMemory[80 * y + x] & 0xFF00) | str[i];
            x++;
            break;
        }

        if (x >= 80)
        {
            x = 0;
            y++;
        }

        if (y >= 25)
        {
            for (y = 0; y < 25; y++)
                for (x = 0; x < 80; x++)
                    VideoMemory[80 * y + x] = (VideoMemory[80 * y + x] & 0xFF00) | ' ';
            x = 0;
            y = 0;
        }
    }
}

void printDecimal(int key)
{
    char *dec = "0123456789";

    // Find the digits of the key
    int digits = 1;
    for (int i = 10; key / i != 0; i *= 10)
    {
        digits++;
    }

    // Create a string to hold the decimal number
    char *foo = new char[digits + 1];

    // Fill the decimal string
    int digitIndex, j = 1;
    for (int i = digits - 1; i >= 0; i--)
    {
        digitIndex = (key / j) % 10;

        foo[i] = dec[digitIndex];

        j *= 10;
    }
    foo[digits] = '\0';

    printf(foo);
}

void printfHex(uint8_t key)
{
    char *foo = "00";
    char *hex = "0123456789ABCDEF";
    foo[0] = hex[(key >> 4) & 0xF];
    foo[1] = hex[key & 0xF];
    printf(foo);
}

void printfHex16(uint16_t key)
{
    printfHex((key >> 8) & 0xFF);
    printfHex(key & 0xFF);
}

void printfHex32(uint32_t key)
{
    printfHex((key >> 24) & 0xFF);
    printfHex((key >> 16) & 0xFF);
    printfHex((key >> 8) & 0xFF);
    printfHex(key & 0xFF);
}

void printAddress(char *addressName, size_t address)
{
    printf(addressName);
    printf(": 0x");
    printfHex((address >> 24) & 0xFF);
    printfHex((address >> 16) & 0xFF);
    printfHex((address >> 8) & 0xFF);
    printfHex((address)&0xFF);
    printf(" ");
}

// Naive iterative solution to calculate `pow(x, n)`
long power(int x, unsigned n)
{
    // initialize result by 1
    long pow = 1L;

    // multiply `x` exactly `n` times
    for (int i = 0; i < n; i++)
    {
        pow = pow * x;
    }

    return pow;
}

static unsigned long int next = 1;

int rand(void) // RAND_MAX assumed to be 32767
{
    next = next * 1103515245 + 12345;
    return (unsigned int)(next / 65536) % 32768;
}

void srand(unsigned int seed)
{
    next = seed;
}

class PrintfKeyboardEventHandler : public KeyboardEventHandler
{
public:
    void OnKeyDown(char c)
    {
        char *foo = " ";
        foo[0] = c;
        printf(foo);
    }
};

class MouseToConsole : public MouseEventHandler
{
    common::int8_t x, y;

public:
    MouseToConsole()
    {
        uint16_t *VideoMemory = (uint16_t *)0xb8000;
        x = 40;
        y = 12;
        VideoMemory[80 * y + x] = (VideoMemory[80 * y + x] & 0x0F00) << 4 | (VideoMemory[80 * y + x] & 0xF000) >> 4 | (VideoMemory[80 * y + x] & 0x00FF);
    }

    virtual void OnMouseMove(int xoffset, int yoffset)
    {
        static uint16_t *VideoMemory = (uint16_t *)0xb8000;
        VideoMemory[80 * y + x] = (VideoMemory[80 * y + x] & 0x0F00) << 4 | (VideoMemory[80 * y + x] & 0xF000) >> 4 | (VideoMemory[80 * y + x] & 0x00FF);

        x += xoffset;
        if (x >= 80)
            x = 79;
        if (x < 0)
            x = 0;
        y += yoffset;
        if (y >= 25)
            y = 24;
        if (y < 0)
            y = 0;

        VideoMemory[80 * y + x] = (VideoMemory[80 * y + x] & 0x0F00) << 4 | (VideoMemory[80 * y + x] & 0xF000) >> 4 | (VideoMemory[80 * y + x] & 0x00FF);
    }
};

class PrintfUDPHandler : public UserDatagramProtocolHandler
{
public:
    void HandleUserDatagramProtocolMessage(UserDatagramProtocolSocket *socket, common::uint8_t *data, common::uint16_t size)
    {
        char *foo = " ";
        for (int i = 0; i < size; i++)
        {
            foo[0] = data[i];
            printf(foo);
        }
    }
};

class PrintfTCPHandler : public TransmissionControlProtocolHandler
{
public:
    bool HandleTransmissionControlProtocolMessage(TransmissionControlProtocolSocket *socket, common::uint8_t *data, common::uint16_t size)
    {
        char *foo = " ";
        for (int i = 0; i < size; i++)
        {
            foo[0] = data[i];
            printf(foo);
        }

        if (size > 9 && data[0] == 'G' && data[1] == 'E' && data[2] == 'T' && data[3] == ' ' && data[4] == '/' && data[5] == ' ' && data[6] == 'H' && data[7] == 'T' && data[8] == 'T' && data[9] == 'P')
        {
            socket->Send((uint8_t *)"HTTP/1.1 200 OK\r\nServer: MyOS\r\nContent-Type: text/html\r\n\r\n<html><head><title>My Operating System</title></head><body><b>My Operating System</b> http://www.AlgorithMan.de</body></html>\r\n", 184);
            socket->Disconnect();
        }

        return true;
    }
};

void sysprintf(char *str)
{
    asm("int $0x80"
        :
        : "a"(4), "b"(str));
}

void taskA()
{
    while (true)
        sysprintf("A");
}

void taskB()
{
    while (true)
        sysprintf("B");
}

typedef void (*constructor)();
extern "C" constructor start_ctors;
extern "C" constructor end_ctors;
extern "C" void callConstructors()
{
    for (constructor *i = &start_ctors; i != &end_ctors; i++)
        (*i)();
}

extern "C" void kernelMain(const void *multiboot_structure, uint32_t /*multiboot_magic*/)
{
    printf("Sorting program will be executed soon...\n");

    // Determine sorting paramaters here
    SortingAlgorithm sortAlgo = SortingAlgorithm::BubbleSort;
    PageReplacementAlgorithm prAlgo = PageReplacementAlgorithm::LRU;

    uint32_t *memupper = (uint32_t *)(((common::size_t)multiboot_structure) + 8);
    size_t heap = 10 * 1024 * 1024; // 10 MB
    MemoryManager memoryManager(heap, (*memupper) * 1024 - heap - 10 * 1024);

    // Sizes for memory, disk and their array representations
    const size_t physicalMemorySize = 1024; // Bits, 1024 = 1KB, 96 = 3 int
    const size_t diskSize = physicalMemorySize * 1024;

    const size_t physicalArraySize = physicalMemorySize / 32;
    const size_t diskArraySize = diskSize / 32;

    // Dynamically allocate PM space and Disk for the program
    // We will assuma that they are integer arrays from now on
    int *pm = new int[physicalArraySize];
    int *disk = new int[diskArraySize];

    int arr[] = {80, 6, 12, 94, 4, 72, 45, 61,
                 56, 93, 58, 1, 45, 43, 59, 29,
                 76, 55, 10, 35, 29, 96, 33, 42,
                 77, 75, 20, 20, 74, 91, 29, 77,
                 26, 3, 75, 36, 51, 50, 100, 39,
                 56, 44, 42, 51, 34, 83, 87, 59,
                 27, 66, 9, 21, 80, 17, 8, 83,
                 95, 88, 16, 13, 99, 62, 28, 13};
    // int arr[] = {80, 6, 12, 94, 4};
    int arrSize = (sizeof(arr) / sizeof(int));

    double arrayRelativeSize = (double)arrSize / (double)physicalArraySize;

    // Call the main program
    Sorting::sortingProgram(arrayRelativeSize, sortAlgo, prAlgo, false,
                            pm, physicalArraySize, disk, diskSize, arr, arrSize);
}

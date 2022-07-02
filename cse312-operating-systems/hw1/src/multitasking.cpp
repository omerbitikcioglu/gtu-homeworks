
#include <multitasking.h>

using namespace myos;
using namespace myos::common;


Task::Task(GlobalDescriptorTable *gdt, void entrypoint())
{
    cpustate = (CPUState*)(stack + 4096 - sizeof(CPUState));
    
    cpustate -> eax = 0;
    cpustate -> ebx = 0;
    cpustate -> ecx = 0;
    cpustate -> edx = 0;

    cpustate -> esi = 0;
    cpustate -> edi = 0;
    cpustate -> ebp = 0;
    
    /*
    cpustate -> gs = 0;
    cpustate -> fs = 0;
    cpustate -> es = 0;
    cpustate -> ds = 0;
    */
    
    // cpustate -> error = 0;    
   
    // cpustate -> esp = ;
    cpustate -> eip = (uint32_t)entrypoint;
    cpustate -> cs = gdt->CodeSegmentSelector();
    // cpustate -> ss = ;
    cpustate -> eflags = 0x202;
}

Task::~Task()
{
}

        
TaskManager::TaskManager()
{
    numTasks = 0;
    currentTask = -1;
}

TaskManager::~TaskManager()
{
}

bool TaskManager::AddTask(Task* task)
{
    if(numTasks >= 256)
        return false;
    tasks[numTasks++] = task;
    return true;
}

// Terminate the task
bool TaskManager::RemoveTask(Task* task) {
    if (numTasks <= 0)
        return false;
    for (uint32_t i = 0; i < numTasks; i++)
    {
        if (tasks[i] == task)
        {
            for (uint32_t j = i; j < numTasks-1; j++)
            {
                tasks[j] = tasks[j+1];
            }
            numTasks--;
            return true;
        }
    }
}

// Yield the cpu resources
void TaskManager::YieldTask(Task* task) {
    task -> cpustate -> eax = 0;
    task -> cpustate -> ebx = 0;
    task -> cpustate -> ecx = 0;
    task -> cpustate -> edx = 0;

    task -> cpustate -> esi = 0;
    task -> cpustate -> edi = 0;
    task -> cpustate -> ebp = 0; 
}

// Wait for the given task to terminate
void TaskManager::JoinTask(Task* task) {
    for (uint32_t i = 0; i < numTasks; i++)
    {
        while (tasks[i] == task); // Wait for the change
        return;
    }
}

CPUState* TaskManager::Schedule(CPUState* cpustate)
{
    if(numTasks <= 0)
        return cpustate;
    
    if(currentTask >= 0)
        tasks[currentTask]->cpustate = cpustate;
    
    if(++currentTask >= numTasks)
        currentTask %= numTasks;
    return tasks[currentTask]->cpustate;
}

    
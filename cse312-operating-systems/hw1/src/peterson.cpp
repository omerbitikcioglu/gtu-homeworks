#include <peterson.h>

using namespace myos;

int PetersonAlgorithm::turn = 0;
int PetersonAlgorithm::interested[2] = {0,0};

void PetersonAlgorithm::enter_region(int process) {
    int other;

    other = 1 - process;
    interested[process] = true;
    turn = process;
    while (turn == process && interested[other] == true);
}

void PetersonAlgorithm::leave_region(int process) {
    interested[process] = false;
}

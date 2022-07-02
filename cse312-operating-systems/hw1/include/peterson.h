namespace myos {
    class PetersonAlgorithm {
        static int turn;
        static int interested[2];
    public:
        static void enter_region(int taskId);
        static void leave_region(int taskId);
    };
}
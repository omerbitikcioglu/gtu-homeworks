package com.company;

import java.util.Iterator;
import java.util.Random;

public class Main {

    private static int NUM_OF_ENTRIES = 97;
    private static int BOUND = 10000;

    public static void main(String[] args) {
        //part1();
        part2();
    }

    private static void part2() {
        HashtableChainLinkedList<Integer,Integer> chainLL = new HashtableChainLinkedList<>();
        HashtableChainTreeSet<Integer,Integer> chainTree = new HashtableChainTreeSet<>();
        HashtableCoalesced<Integer,Integer> coalesced = new HashtableCoalesced<>();

        int randKey, randVal;
        long t, t1=0, t2=0, t3=0;
        System.out.println("Adding items...");
        for (int i = 0; i < NUM_OF_ENTRIES; ++i) {
            Random rand = new Random();
            randKey = rand.nextInt(BOUND);
            randVal = rand.nextInt(BOUND);

            t = System.nanoTime();
            chainLL.put(randKey, randVal);
            t1 += (System.nanoTime()-t);

            t = System.nanoTime();
            chainTree.put(randKey, randVal);
            t2 += (System.nanoTime()-t);

            t = System.nanoTime();
            coalesced.put(randKey, randVal);
            t3 += (System.nanoTime()-t);
        }
        t1 /= NUM_OF_ENTRIES;
        t2 /= NUM_OF_ENTRIES;
        t3 /= NUM_OF_ENTRIES;
        System.out.println("Chain LL: " +t1+ " Chain TreeSet: " +t2+ " Coalesced: " +t3);

        t1=0; t2=0; t3=0;
        System.out.println("\nRemoving items...(Existing/non-existing randomly)");
        for (int i = 0; i < NUM_OF_ENTRIES; ++i) {
            Random rand = new Random();
            randKey = rand.nextInt(BOUND);

            t = System.nanoTime();
            chainLL.remove(randKey);
            t1 += (System.nanoTime()-t);

            t = System.nanoTime();
            chainTree.remove(randKey);
            t2 += (System.nanoTime()-t);

            t = System.nanoTime();
            coalesced.remove(randKey);
            t3 += (System.nanoTime()-t);

        }
        t1 /= NUM_OF_ENTRIES;
        t2 /= NUM_OF_ENTRIES;
        t3 /= NUM_OF_ENTRIES;
        System.out.println("Chain LL: " +t1+ " Chain TreeSet: " +t2+ " Coalesced: " +t3);

        t1=0; t2=0; t3=0;
        System.out.println("\nAccessing items...(Existing/non-existing randomly)");
        for (int i = 0; i < NUM_OF_ENTRIES; ++i) {
            Random rand = new Random();
            randKey = rand.nextInt(BOUND);

            t = System.nanoTime();
            chainLL.get(randKey);
            t1 += (System.nanoTime()-t);

            t = System.nanoTime();
            chainTree.get(randKey);
            t2 += (System.nanoTime()-t);

            t = System.nanoTime();
            coalesced.get(randKey);
            t3 += (System.nanoTime()-t);

        }
        t1 /= NUM_OF_ENTRIES;
        t2 /= NUM_OF_ENTRIES;
        t3 /= NUM_OF_ENTRIES;
        System.out.println("Chain LL: " +t1+ " Chain TreeSet: " +t2+ " Coalesced: " +t3);

    }

    private static void part1() {
        // Create a HashMap with randomly generated keys and values.
        CustomHashMap<Integer,Integer> hm = new CustomHashMap<>();
        int randKey, randVal;
        for (int i = 0; i < NUM_OF_ENTRIES; ++i) {
            Random rand = new Random();
            randKey = rand.nextInt(100);
            randVal = rand.nextInt(100);
            hm.put(randKey, randVal);
        }

        hm.put(100,50);
        hm.put(34,56);
        hm.put(244,524);

        Iterator<Integer> iter = hm.iterator();
        while (iter.hasNext()){
            System.out.println(iter.next());
        }
        System.out.println();

        iter = hm.iterator(34);
        System.out.println(iter.next());
    }
}

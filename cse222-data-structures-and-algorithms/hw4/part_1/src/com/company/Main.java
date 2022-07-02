package com.company;

import java.util.Iterator;

public class Main {

    public static void main(String[] args) {
		MyPriorityQueue<Integer> myHeap = new MyPriorityQueue<>();
	    myHeap.offer(15);
	    myHeap.offer(20);
	    myHeap.offer(40);
	    myHeap.offer(36);
	    myHeap.offer(72);

	    // Search for existing element
		int valInTheList = 20;
	    if(myHeap.search(valInTheList)){
            System.out.println("Value " + valInTheList + " is in the heap. Passed!");
        } else System.out.println("Failed!");

	    // Search for non-existing element
		int valNotInTheList = 230;
		if(!myHeap.search(valNotInTheList)){
			System.out.println("Value " + valNotInTheList + " is not in the heap. Passed!");
		} else System.out.println("Failed!");

		// Merge with another heap
		MyPriorityQueue<Integer> anotherHeap = new MyPriorityQueue<>();
		anotherHeap.offer(55);
		anotherHeap.offer(96);
		anotherHeap.offer(59);
		myHeap.merge(anotherHeap);
		if (myHeap.search(55) && myHeap.search(96) && myHeap.search(59)) {
			System.out.println("Two heaps are merged successfully. Passed!");
		}

		// Remove 1st largest element which is 96
		myHeap.removeIthLargest(1);
		if (!myHeap.search(96)){
			System.out.println("Largest element removed successfully. Passed!");
		}

		// Remove 3rd largest element which is 55
		myHeap.removeIthLargest(3);
		if (!myHeap.search(55)){
			System.out.println("3th largest element removed successfully. Passed!");
		}

		// Remove smallest element which is 6th largest element is 15
		myHeap.removeIthLargest(6);
		if (!myHeap.search(15)){
			System.out.println("6th largest element removed successfully. Passed!");
		}

		// Check if the rest of the items are removed or not
		if (myHeap.search(20) && myHeap.search(40) && myHeap.search(36) && myHeap.search(72) && myHeap.search(59)){
			System.out.println("Rest of the elements stays safe. Passed!");
		}
    }
}

package com.company;

import com.company.users.Customer;
import com.company.users.User;

/**
 * Automation System
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class AutomationSystem {
    private final String companyName;
    private MutableArray<Branch> branches = new MutableArray<>(Branch.class);
    private MutableArray<User> users = new MutableArray<>(User.class);
    private int numOfCustomers = 0;

    /**
     * Constructor method for AutomationSystem.
     *
     * @param companyName name of the company that uses the system.
     */
    public AutomationSystem(String companyName) {
        this.companyName = companyName;
    }

    /**
     * Getter for branches of the system.
     *
     * @return the branches of the system.
     */
    public MutableArray<Branch> getBranches() {
        return branches;
    }

    /**
     * Getter for users of the system.
     *
     * @return the users of the system.
     */
    public MutableArray<User> getUsers() {
        return users;
    }

    /**
     * Getter for number of customers of the system.xç
     *
     * @return the number of the customers on this system.
     */
    public int getNumOfCustomers() {
        return numOfCustomers;
    }

    /**
     * Add a new user to the system.
     *
     * @param newUser the new user to be added.
     */
    public void addUser(User newUser) {
        users.addItem(newUser);

        if(newUser instanceof Customer) {
            numOfCustomers++;
        }
    }

    /**
     * Removes the given user from the system.
     *
     * @param userToBeRemoved the user to be removed from the system.
     */
    public void removeUser(User userToBeRemoved) {
        users.removeItem(userToBeRemoved);
    }


}

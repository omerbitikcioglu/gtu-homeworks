package com.company.users;

import com.company.AutomationSystem;
import com.company.Branch;
import com.company.products.Furniture;

/**
 * Admin class for administrators of the automation system.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class Admin extends User {
    protected AutomationSystem system;

    /**
     * Constructor for the Admin class
     *
     * @param name name of the admin.
     * @param surname surname of the admin.
     * @param email email of the admin.
     * @param password password of the admin.
     * @param system the system that the administrator manages.
     */
    public Admin(String name, String surname, String email, String password, AutomationSystem system) {
        super(name, surname, email, password);
        this.system = system;
    }

    /**
     * Administrators can add new branches to the system.
     *
     * @param newBranch branch to be added to the system.
     */
    public void addBranchToSystem(Branch newBranch) {
        system.getBranches().addItem(newBranch);
    }

    /**
     * Administrators can remove a branch from the system.
     *
     * @param branchToBeRemoved branch to be removed.
     */
    public void removeBranchFromSystem(Branch branchToBeRemoved) {
        system.getBranches().removeItem(branchToBeRemoved);
    }

    /**
     * Administrators can add new branch employees to the branches.
     *
     * @param branch the branch that new employee to be added.
     * @param newEmployee the new employee to be added.
     */
    public void addBranchEmployeeToBranch(Branch branch, BranchEmployee newEmployee) {
        branch.getEmployees().addItem(newEmployee);
        newEmployee.setBranch(branch);
    }

    /**
     * Administrators can remove a branch employee from a branch.
     *
     * @param branch the branch that the employee to be removed
     * @param employeeToBeRemoved the employee to be removed
     */
    public void removeBranchEmployeeFromBranch(Branch branch, BranchEmployee employeeToBeRemoved) {
        branch.getEmployees().removeItem(employeeToBeRemoved);
    }

    /**
     * Checks if any product in the branches needs to be supplied.
     * If the stock is less than or equal to 5, it will inform the administrator.
     * Otherwise it will say everything is okay.
     */
    public void queryForSupply() {
        int numOfBranches = system.getBranches().getNumOfElements();
        Branch currentBranch;
        int numOfGoods;
        boolean isFound = false;

        try {
            for(int i = 0; i < numOfBranches; ++i) {
                currentBranch = system.getBranches().getItem(i);
                numOfGoods = currentBranch.getGoods().getNumOfElements();

                for(int j = 0; j < numOfGoods; ++j) {
                    Furniture currentItem = currentBranch.getGoods().getItem(j);
                    if (currentItem.getStock() <= 5) {
                        isFound = true;
                        System.out.println("Stock of " + currentItem.getName() + " is 0, " +
                                "consider supplying some.");
                    }
                }
            }
            if (!isFound){
                System.out.println("All stocks are good!");
            }
        } catch (Exception e) {
            System.out.println(e);
        }
    }
}

package com.company;

import com.company.products.Furniture;
import com.company.users.BranchEmployee;
import com.company.users.User;

/**
 * Branch class for the branches of the company.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class Branch{
    private final String branchName;
    private int earnings;
    private MutableArray<BranchEmployee> employees = new MutableArray<>(BranchEmployee.class);
    private MutableArray<Furniture> goods = new MutableArray<>(Furniture.class);

    /**
     * Constructor of the branch class.
     *
     * @param branchName name of the branch.
     */
    public Branch(String branchName) {
        this.branchName = branchName;
    }

    /**
     * Getter of the branchName property.
     *
     * @return name of the branch.
     */
    public String getBranchName() {
        return branchName;
    }

    /**
     * Getter for the employees of the branch.
     *
     * @return the employees of the system.
     */
    public MutableArray<BranchEmployee> getEmployees() {
        return employees;
    }

    /**
     * Getter for the goods of the branch.
     *
     * @return the array of goods of the system.
     */
    public MutableArray<Furniture> getGoods() {
        return goods;
    }

    /**
     * Getter for earnings of the branch.
     *
     * @return the amount of earning of the branch.
     */
    public int getEarnings() {
        return earnings;
    }

    /**
     * Setter for the earning of the branch.
     *
     * @param earnings the money that branch has.
     */
    public void setEarnings(int earnings) {
        this.earnings = earnings;
    }

    @Override
    public boolean equals(Object obj) {
        if(this == obj) return true;
        else if(obj instanceof Branch) {
            Branch branchObj = (Branch) obj;
            return this.getBranchName().compareTo(branchObj.getBranchName()) == 0;
        } else {
            return false;
        }
    }
}

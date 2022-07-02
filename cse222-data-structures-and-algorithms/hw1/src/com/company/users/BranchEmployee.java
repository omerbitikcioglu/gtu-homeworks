package com.company.users;

import com.company.AutomationSystem;
import com.company.Branch;
import com.company.Order;
import com.company.products.Furniture;

import java.time.LocalDate;

/**
 * Branch Employee for the branches of the company.
 * They manage products, stocks, sales, and orders of their branch.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class BranchEmployee extends User {
    /**
     * The branch that employee works for.
     */
    Branch branch;

    /**
     * Constructor of the BranchEmployee class.
     *
     * @param name name of the branch employee.
     * @param surname surname of the branch employee.
     * @param email email of the branhc employee.
     * @param password password of the branch employee.
     */
    public BranchEmployee(String name, String surname, String email, String password) {
        super(name, surname, email, password);
    }

    /**
     * Setter for the branch field of the branch employee.
     *
     * @param branch the branch that employee works for.
     */
    public void setBranch(Branch branch) {
        this.branch = branch;
    }

    /**
     * Getter for the branch that employee works for.
     *
     * @return the branch employee works for.
     */
    public Branch getBranch() {
        return branch;
    }

    /**
     * Branch employee can inquire about the product stock.
     *
     * @param product the product to be search for its stock info.
     */
    public void inquireProductStock(Furniture product) {
        try {
            int productStock;

            if (branch.getGoods().searchFor(product)) {
                productStock = product.getStock();
            } else {
                productStock = 0;
            }

            System.out.println(branch.getBranchName() + " has " + productStock + " " + product.getName());
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    /**
     * Informs the manager about stock information.
     */
    public void informManager() {
        System.out.println("The stock information sent to the manager.");
    }

    /**
     * BranchEmployee adds a new product to his/her branch.
     *
     * @param product the product to be added to the branch.
     * @param amount the amount of the product to be added.
     */
    public void addProduct(Furniture product, int amount) {
        // Search for product to be added if it exists in the array.
        // If exists, just increase the stock,
        // otherwise add new item with given stock to the array.
        try {
            if (branch.getGoods().searchFor(product)) {
                product.setStock(product.getStock() + amount);
            } else {
                product.setStock(amount);
                branch.getGoods().addItem(product);
            }
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    /**
     * BranchEmployee removes the amount of product from the branch.
     *
     * @param product the product to be removed.
     * @param amount the amount of product to be removed.
     */
    public void removeProduct(Furniture product, int amount) {
        try {
            int stock = product.getStock();

            if (stock > amount) {
                product.setStock(stock - amount);
            } else if (stock == amount) {
                branch.getGoods().removeItem(product);
                product.setStock(0);
            } else {
                System.out.println("Not enough item in the stock!");
            }
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    /**
     * BranchEmployee sells the amount of item to the customer.
     *
     * @param customer the customer buys the product.
     * @param product the product to be sold.
     * @param amount the amount of product to be sold.
     */
    public void sell(Customer customer, Furniture product, int amount) {
        try {
            int earnings = branch.getEarnings();

            if (amount > product.getStock()) {
                System.out.println("Stock is not enough for this item!");
                informManager();
            } else {
                branch.setEarnings(earnings + product.getPrice());
                removeProduct(product, amount);

                //Add this sale to the previous orders of the customer
                Order newOrder = new Order(product.getName(), product.getPrice()*amount, LocalDate.now());
                customer.addOrder(newOrder);
            }
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    /**
     * BranchEmployee can view order history of a customer by entering its customer number.
     *
     * @param system the system that branch employee uses.
     * @param customerNumber the customer number of the customer to be viewed.
     */
    public void viewPreviousOrders(AutomationSystem system, int customerNumber) {
        Customer customer = (Customer) system.getUsers().getItem(customerNumber-1000);
        customer.viewOrders();
    }
}

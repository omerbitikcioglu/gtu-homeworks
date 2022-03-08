package com.company.users;

import com.company.AutomationSystem;
import com.company.Branch;
import com.company.MutableArray;
import com.company.Order;
import com.company.products.Furniture;

import java.text.MessageFormat;
import java.time.LocalDate;
import java.util.Scanner;

/**
 * Customer class.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class Customer extends User {
    private int customerNumber;
    private AutomationSystem system;
    private String address;
    private String phoneNum;
    private MutableArray<Order> orders = new MutableArray<>(Order.class);

    /**
     * Constructor of Customer class.
     *
     * @param name the name of the customer.
     * @param surname the surname of the customer.
     * @param email the email of the customer.
     * @param password the password of the customer.
     */
    public Customer(String name, String surname, String email, String password) {
        super(name, surname, email, password);
    }

    /**
     * Getter for special number of customer.
     *
     * @return the customer number.
     */
    public int getCustomerNumber() {
        return customerNumber;
    }

    /**
     * Adds the given order to the customer's order history.
     *
     * @param order the order to be added to the order history.
     */
    public void addOrder(Order order) {
        orders.addItem(order);
    }

    /**
     * Customer subscribes to the system.
     * Customer number set to hash code of the customer object.
     * Because hash code is unique for every distinct object.
     *
     * @param system the system to be subscribed.
     */
    public void subscribe(AutomationSystem system) {
        try {
            this.customerNumber = system.getUsers().searchForIndex(this) + 1000;
            System.out.println(customerNumber);
            this.system = system;
            informUserAboutSubscription();
        } catch (Exception e) {
            System.out.println(e);
        }
    }


    /**
     * Informs the customer about his/her customer number.
     */
    private void informUserAboutSubscription() {
        System.out.println("Subscription is successful!");
        System.out.println("Your customer number is: " + this.customerNumber);
    }

    /**
     * Customers can search for a specific product by entering its name.
     *
     * @param modelName the name of the model to be searched.
     */
    public void searchProducts(String modelName) {
        // Take the name of the product
        /*System.out.println("Enter the model you look for: ");
        Scanner sc = new Scanner(System.in);
        String modelName = sc.next();*/

        try {
            MutableArray<Branch> branches = system.getBranches();
            MutableArray<Furniture> goodsOfTheBranch;
            MutableArray<Furniture> found = new MutableArray<>(Furniture.class);

            // Search for the product in the system
            int numOfBranches = branches.getNumOfElements();
            int numOfGoods;
            for(int i = 0; i < numOfBranches; ++i) {
                goodsOfTheBranch = branches.getItem(i).getGoods();
                numOfGoods = goodsOfTheBranch.getNumOfElements();

                for(int j = 0; j < numOfGoods; ++j) {
                    Furniture currentFurniture = goodsOfTheBranch.getItem(j);
                    if(currentFurniture.getName().contains(modelName)){
                        found.addItem(currentFurniture);
                    }
                }
            }
            seeProducts(found);
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    /**
     * Lists the given furniture list with names containing their model, color and price.
     *
     * @param goodsToBeListed the furnitures to be listed to the customer.
     */
    public void seeProducts(MutableArray<Furniture> goodsToBeListed) {
        try {
            Furniture item;
            for(int i = 0; i < goodsToBeListed.getNumOfElements(); ++i) {
                item = goodsToBeListed.getItem(i);
                System.out.println(MessageFormat.format("ITEM {0}: NAME: {1}, PRICE: {2}", i+1, item.getName(), item.getPrice()));
            }
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    /**
     * Let's you see which branch has the product.
     *
     * @param product the product to be searched.
     */
    public void seeWhereTheProduct(Furniture product) {
        try {
            System.out.println("You can find " + product.getName() + " in the branches below:");

            int numOfBranches = system.getBranches().getNumOfElements();
            for(int i = 0; i < numOfBranches; ++i) {
                Branch currentBranch = system.getBranches().getItem(i);

                if (currentBranch.getGoods().searchFor(product)) {
                    System.out.println(currentBranch.getBranchName());
                }
            }
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    /**
     * Lets the customer shop online by using automation system.
     * Customer needs to supply her/his address and phone number.
     *
     * @param branch the branch that customer buy the product from.
     * @param product the product that is going to be sold.
     */
    public void shopOnline(Branch branch, Furniture product) {
        try {
            Scanner sc = new Scanner(System.in);

            if(branch.getGoods().searchFor(product)){
                System.out.println("Enter your address:");
                this.address = sc.nextLine();

                System.out.println("Enter your phone number:");
                this.phoneNum = sc.nextLine();

                // Add to the order history
                Order newOrder = new Order(product.getName(), product.getPrice(), LocalDate.now());
                orders.addItem(newOrder);

                // Remove item from branch
                branch.getGoods().removeItem(product);

                System.out.println("Your furniture will be delivered soon!");
            } else {
                System.out.println("Sorry, we don't have this product right now!");
            }
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    /**
     * This method is only for testing purposes.
     * Original method needs user interaction.
     *
     * @param branch the branch that customer buy the product from.
     * @param product the product that is going to be sold.
     */
    public void shopOnlineTest(Branch branch, Furniture product) {
        try {
            if(branch.getGoods().searchFor(product)){
                System.out.println("Enter your address:");
                this.address = "Test Adress";
                System.out.println(this.address);

                System.out.println("Enter your phone number:");
                this.phoneNum = "Test Phone Number";
                System.out.println(this.phoneNum);

                Order newOrder = new Order(product.getName(), product.getPrice(), LocalDate.now());
                orders.addItem(newOrder);

                System.out.println("Your furniture will be delivered soon!");
            } else {
                System.out.println("Sorry, we don't have this product right now!");
            }
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    /**
     * Lets the user see his/her order history.
     */
    public void viewOrders() {
        try {
            int numOfOrders = orders.getNumOfElements();

            for(int i = 0; i < numOfOrders; ++i) {
                System.out.println(orders.getItem(i).toString());
            }
        } catch (Exception e) {
            System.out.println(e);
        }
    }
}

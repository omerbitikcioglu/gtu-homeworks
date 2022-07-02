package com.company;

import java.io.*;
import java.util.Hashtable;
import java.util.Scanner;

/**
 * This class represents the whole application.
 *
 * @author Ömer Faruk Bitikçioğlu
 */
public class ShoppingApp {
    private final Hashtable<Integer, AbstractUser> users = new Hashtable<>();
    private final Hashtable<String, Trader> traders = new Hashtable<>();
    private final CategoryTree categoryTree = new CategoryTree();
    private int traderID = 10000000, traderPassword = 100000; // Arbitrary values

    /**
     * Constructs the shopping app and fills the users with given file
     * @param usersFile The file that users will be loaded
     */
    public ShoppingApp(String usersFile) {
        try {
            FileReader reader = new FileReader(usersFile);
            BufferedReader bufferedReader = new BufferedReader(reader);
            // Add existing users to the app from users.txt
            String line;
            while ((line = bufferedReader.readLine()) != null) {
                String[] userData = line.split(" ");
                Integer id = Integer.parseInt(userData[0]);
                String password = userData[1];
                String name = userData[3];
                if (userData[2].equals("CUSTOMER")) {
                    register(id, password, UserType.CUSTOMER, name);
                } else if (userData[2].equals("TRADER")) {
                    register(id, password, UserType.TRADER, name);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Registers the given user to the app.
     * If the id is already exists, register fails.
     */
    public void register(Integer id, String password, UserType userType, String name) {
        AbstractUser newUser;
        if (userType == UserType.CUSTOMER) {
            newUser = new Customer(id, password, name);
        } else {
            newUser = new Trader(id, password, name, this);
        }
        if (users.putIfAbsent(id, newUser) == null) {
            System.out.println("Register complete!");
            addUserToDb(id, password, userType, name);
        } else {
            System.out.println("User already exist!");
        }
    }

    /**
     * The user logs in with her/his id and password
     * @param id The id of the user to be logged in
     * @param password The password of the user to be logged in
     */
    public void login(Integer id, String password) {
        AbstractUser user = users.get(id);
        if (user != null) {
            if (user.checkPassword(password)) {
                user.menu();
            } else {
                System.err.println("Wrong Password!");
            }
        } else {
            System.err.println("Wrong ID!");
        }
    }

    /**
     * Checks if the given id matches a user in the app
     * @param id The id to be searched
     * @return true if the given id matches a user
     */
    public boolean isUser(Integer id) {
        return users.get(id) != null;
    }

    /**
     * Add a category with its dept to the category tree of the app
     * @param categoryName the name of the category to be added
     * @param depth the depth of the category to be added
     */
    public void addCategory(String categoryName, int depth) {
        categoryTree.addCategory(categoryName, depth);
    }

    /**
     * Adds a new products for the name of trader
     * @param traderName The name of the trader holds the product
     */
    public void addProduct(String traderName) {
        Scanner sc = new Scanner(System.in);
        System.out.println("Add Product");
        // Take id, name, category, price, discounted price
        // and description of the product from user
        System.out.print("ID: ");
        String id = sc.nextLine();
        System.out.print("Name: ");
        String name = sc.nextLine();
        // Select category
        System.out.print("Category: ");
        // TODO: Category hierarchy is missing, fix it
        String category = sc.nextLine();
        System.out.print("Price: ");
        int price;
        try {
            price = Integer.parseInt(sc.nextLine());
        } catch (NumberFormatException e) {
            price = 0;
        }
        System.out.print("Discounted Price: ");
        int dPrice;
        try {
            dPrice = Integer.parseInt(sc.nextLine());
        } catch (NumberFormatException e) {
            dPrice = 0;
        }
        System.out.print("Description: ");
        String description = sc.nextLine();
        // Add these information to the products.txt
        try {
            FileWriter writer = new FileWriter("products.txt", true);
            writer.write(id + ";" + name + ";" + category + ";" + price + ";" + dPrice + ";" + description + ";" + traderName + "\n");
            writer.close();
            System.out.println("New product added!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Splits the given row and puts related data to the products.txt
     * pre: row consist of product information separated with semicolons.
     * @param row The row to be split to products information. Usually this
     *            data comes from row of a csv file.
     * @throws IOException If products.txt is not available
     */
    public void putProductData(String row) throws IOException {
        FileWriter writer = new FileWriter("products.txt"); // output
        String[] productData = row.split(";");
        String traderName;
        int i;
        for (i = 0; i < productData.length-1; ++i) {
            writer.write(productData[i] + ";");
            // Fill the category tree
            if (i == 2) {
                String hierarchy = productData[i];
                hierarchy = hierarchy.substring(4,hierarchy.length()-4);
                String[] categories = hierarchy.split(">>");
                for (int j = 0; j < categories.length; ++j) {
                    addCategory(categories[j], j);
                }
            }
        }
        traderName = productData[i];
        writer.write(traderName + "\n");
        writer.close();
        createTraderUser(traderName);
    }

    /**
     * Creates a new trader user with the given name and arbitrary values
     * @param traderName The name of the trader to be added
     */
    private void createTraderUser(String traderName) {
        if (traders.get(traderName) == null) {
            if (!isUser(traderID)) {
                Trader newTrader = new Trader(traderID, Integer.toString(traderPassword), traderName, this);
                traders.put(traderName, newTrader);
                register(traderID, String.valueOf(traderPassword), UserType.TRADER, traderName);
                ++traderID; ++traderPassword;
            }
        }
    }

    /**
     * Adds the given user to user.txt
     * @param id The id of the user to be added
     * @param password The password of the user to be added
     * @param userType The user type of the user to be added
     * @param name The name of the user to be added
     */
    private void addUserToDb(Integer id, String password, UserType userType, String name) {
        try {
            FileWriter writer = new FileWriter("users.txt", true);
            writer.write(id.toString() + " " + password + " " + userType + " " + name + "\n");
            writer.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

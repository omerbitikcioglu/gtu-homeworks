package com.company;

import java.io.*;
import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * Trader is the user that sells their products on the app.
 *
 * @author Ömer Faruk Bitikçioğlu
 */
public class Trader extends AbstractUser {
    private final ShoppingApp app;

    //private Shop shop = new Shop();

    /** Constructor of Trader class */
    public Trader(Integer id, String password, String name, ShoppingApp app) {
        super(id, password, UserType.TRADER, name);
        this.app = app;
    }

    /**
     * Menu of the Trader user.
     * Trader can add/remove/edit products on their shop.
     * Trader can see the orders of customers and accept or decline them.
     */
    @Override
    public void menu() {
        Scanner sc = new Scanner(System.in);
        boolean showMenu = true;
        int selection;
        while (showMenu) {
            System.out.println("TRADER MENU");
            System.out.println("1) Add product");
            System.out.println("2) Remove product");
            System.out.println("3) Edit product");
            System.out.println("4) See orders");
            System.out.println("0) Log out");
            System.out.print("Select: ");
            try {
                selection = sc.nextInt();
            } catch (InputMismatchException e) {
                selection = -1;
                sc.nextLine();
            }
            switch (selection) {
                case 1: addProduct(); break;
                case 2: removeProduct(); break;
                case 3: editProduct(); break;
                case 4: seeOrders(); break;
                case 5: showMenu = false; break;
                default:
                    System.err.println("Wrong input!"); break;
            }
        }

    }

    private void addProduct() {
        app.addProduct(this.getName());
    }

    private void removeProduct() {
        Scanner sc = new Scanner(System.in);
        System.out.print("Enter product ID to be removed: ");
        String id = sc.nextLine();
        String productLine;
        try {
            FileReader reader = new FileReader("products.txt");
            BufferedReader productsReader = new BufferedReader(reader);
            while ((productLine = productsReader.readLine()) != null) {
                String[] productDetails = productLine.split(";");
                if (id.equals(productDetails[0])) continue;
                else app.putProductData(productLine);
            }
            
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    private void editProduct() {
    }

    private void seeOrders() {
    }
}

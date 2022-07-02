package com.company;

import java.io.*;
import java.util.InputMismatchException;
import java.util.Scanner;

public class Main {

    // Constants
    private static final int ID_LEN = 8;
    private static final int PASS_LEN = 6;
    private static final int GARBAGE_ID = -1;

    private static ShoppingApp app;

    public static void main(String[] args) {
        app = new ShoppingApp("users.txt");
        readCsvFile();
	    auth();
    }

    /**
     * Reads the csv file and fills products.txt.
     * Creates trader users for each different trader on the table.
     * Fills the category tree of app.
     */
    private static void readCsvFile() {
        String row; // Each row of csv file
        boolean firstRow = true;
        try {
            FileReader reader = new FileReader("e-commerce-samples.csv"); // source
            BufferedReader csvReader = new BufferedReader(reader);
            while ((row = csvReader.readLine()) != null) {
                if (!firstRow) { // Ignore the first row with headings
                    app.putProductData(row);
                } else {
                    firstRow = false;
                }
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /** Authentication menu */
    private static void auth() {
        Scanner sc = new Scanner(System.in);
        int selection;
        boolean showMenu = true;
        while (showMenu) {
            System.out.println("\nE-Shopping App");
            System.out.println("1-Login");
            System.out.println("2-Register");
            System.out.println("0-Exit");
            System.out.print("Select: ");
            try {
                selection = Integer.parseInt(sc.nextLine());
            } catch (InputMismatchException e) {
                sc.nextLine(); // skip the wrong input
                selection = -1; // garbage value
            }
            switch (selection) {
                case 1: login(); break;
                case 2: register(); break;
                case 0: showMenu = false; break;
                default:
                    System.err.println("Wrong Input!");
            }
        }
    }

    /** Users login to the system with their unique id and password */
    private static void login() {
        Scanner sc = new Scanner(System.in);
        System.out.println("\nLogin");
        // Get ID
        System.out.print("ID: ");
        int id;
        try {
            id = Integer.parseInt(sc.nextLine());
        } catch (InputMismatchException e) {
            id = GARBAGE_ID;
            sc.nextLine(); // skip
        }
        // Get Password
        System.out.print("Password: ");
        String password = sc.nextLine();
        // Login
        app.login(id, password);
    }

    /**
     * Non-registered users can register to the app by using this method.
     * Pre: The given ID and password must be valid and the given ID must
     * be unique (shouldn't exist in the users.txt)
     * Expected valid ID is 8 digit integer, password is 6 character string.
     * Post: The user is created and added to the users.txt (db)
     */
    private static void register() {
        Scanner sc = new Scanner(System.in);
        boolean showRegister = true;
        while(showRegister) {
            System.out.println("\nRegister (ID must be 8 digit number, Password must be 6 character)");
            // Get ID
            System.out.print("ID: ");
            int id;
            try {
                id = Integer.parseInt(sc.nextLine());
            } catch (InputMismatchException e) {
                id = GARBAGE_ID;
                sc.nextLine();
            }
            // Take Password
            System.out.print("Password: ");
            String password = sc.nextLine();
            // Take User Type
            System.out.println("User Type: ");
            System.out.println("1-) Customer");
            System.out.println("2-) Trader");
            System.out.print("Select: ");
            int selection;
            try {
                selection = Integer.parseInt(sc.nextLine());
            } catch (InputMismatchException e) {
                selection = -1;
                sc.nextLine();
            }
            // Take name
            System.out.print("Name: ");
            String name = sc.nextLine();
            // Check if the values are valid and register valid user
            if (id == GARBAGE_ID) {
                System.err.println("Id must be 8 digit number!");
            }
            else if(Integer.toString(id).length() != ID_LEN || password.length() != PASS_LEN) {
                System.err.println("ID must be 8 digit, and Password must be 6 character!");
            } else if (selection == -1) {
                System.err.println("Input mismatched!");
            } else {
                switch (selection) {
                    case 1:
                        app.register(id, password, UserType.CUSTOMER, name);
                        showRegister = false;
                        break;
                    case 2:
                        app.register(id, password, UserType.TRADER, name);
                        showRegister = false;
                        break;
                    default:
                        System.err.println("Invalid selection!");
                        break;
                }
            }
        }
    }
}

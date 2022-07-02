package com.company;

import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * Abstract user class.
 *
 * @author Ömer Faruk Bitikçioğlu
 */
public abstract class AbstractUser implements IUser {
    private final Integer id; // unique 8 digit number
    private String password; // 6 character string
    private final UserType userType; // Type of the user can be trader or customer
    private String name;

    /**
     * Constructor for AbstractUser class
     * @param id The id of the user
     * @param password The password of the user
     * @param userType The type of the user
     * @param name The name of the user
     */
    public AbstractUser(Integer id, String password, UserType userType, String name) {
        this.id = id;
        this.password = password;
        this.userType = userType;
        this.name = name;
    }

    /**
     * Getter for the user id
     * @return the id of the user
     */
    public Integer getId() {
        return id;
    }

    public boolean checkPassword(String password) {
        return this.password.equals(password);
    }

    /**
     * Garbage menu method with no ability to perform anything.
     * If a new user type added to the program with no menu method overridden,
     * this will be used by the user class which extends AbstractUser.
     */
    @Override
    public void menu() {
        Scanner sc = new Scanner(System.in);
        int selection; // selected menu option
        boolean showMenu = true;
        while (showMenu) {
            System.out.println("Menu");
            System.out.println("------------");
            System.out.println("1-) Exit");
            System.out.print("Selection: ");
            try {
                selection = sc.nextInt();
            } catch (InputMismatchException e) {
                sc.next();
                selection = -1;
            }
            if (selection == 1) {
                showMenu = false;
            } else {
                System.err.println("Invalid selection! Try again.");
            }
        }
    }

    public String getName() {
        return name;
    }
}

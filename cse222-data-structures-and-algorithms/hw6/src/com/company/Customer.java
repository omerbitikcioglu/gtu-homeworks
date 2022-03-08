package com.company;

/**
 * Customer of the e-shopping application.
 *
 * @author Ömer Faruk Bitikçioğlu
 */
public class Customer extends AbstractUser {
    public Customer(Integer id, String password, String name) {
        super(id, password, UserType.CUSTOMER, name);
    }

    @Override
    public void menu() {
        System.out.println("CUSTOMER MENU");
    }
}

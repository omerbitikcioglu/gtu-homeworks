package com.company.users;

/**
 * Abstract class for Users.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I%, %G%
 * @since 1.0
 */
public abstract class User {
    private final String name, surname, email, password;

    /**
     * Constructor of the User class.
     *
     * @param name the name of the user.
     * @param surname the surname of the user.
     * @param email the email of the user.
     * @param password the password of the user.
     */
    public User(String name, String surname, String email, String password) {
        this.name = name;
        this.surname = surname;
        this.email = email;
        this.password = password;
    }

    /**
     * Checks if two users are equal or not.
     *
     * @param obj the object to be compared.
     * @return true if the two objects are the same.
     */
    @Override
    public boolean equals(Object obj) {
        if(this == obj) return true;
        else if(obj instanceof User) {
            User userObj = (User) obj;
            return this.email.compareTo(userObj.email) == 0 &&
                    this.password.compareTo(userObj.password) == 0;
        } else {
            return false;
        }
    }

    /**
     * Overridden toString method for User class.
     *
     * @return the string format of the user.
     */
    @Override
    public String toString() {
        return "Name: " + this.name + ", Surname: " + this.surname + ", Email: " + this.email + ", Password: " + this.password;
    }
}

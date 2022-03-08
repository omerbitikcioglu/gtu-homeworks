package com.company.users;

/**
 * The user that type is unknown
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class UnknownUser extends User{
    /**
     * Constructor of the UnknownUser class.
     *
     * @param name     the name of the user.
     * @param surname  the surname of the user.
     * @param email    the email of the user.
     * @param password the password of the user.
     */
    public UnknownUser(String name, String surname, String email, String password) {
        super(name, surname, email, password);
    }
}

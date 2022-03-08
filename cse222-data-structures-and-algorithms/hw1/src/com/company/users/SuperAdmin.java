package com.company.users;

import com.company.AutomationSystem;

/**
 * Admins that implementing SuperAdmin can add another admin to the system.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class SuperAdmin extends Admin {
    /**
     * Constructor for the SuperAdmin class
     *
     * @param name name of the super admin.
     * @param surname surname of the super admin.
     * @param email email of the super admin.
     * @param password password of the super admin.
     * @param system the system that the administrator manages.
     */
    public SuperAdmin(String name, String surname, String email, String password, AutomationSystem system) {
        super(name, surname, email, password, system);
    }

    /**
     * SuperAdmins can add another admins to the system.
     *
     * @param newAdmin new admin user to be added to the system
     */
    public void addAdminToSystem(Admin newAdmin){
        system.addUser(newAdmin);
    }

    /**
     * SuperAdmins can remove another admins from the system.
     *
     * @param adminToBeRemoved the admin to be removed
     */
    public void removeAdminFromSystem(Admin adminToBeRemoved){
        system.removeUser(adminToBeRemoved);
    }
}

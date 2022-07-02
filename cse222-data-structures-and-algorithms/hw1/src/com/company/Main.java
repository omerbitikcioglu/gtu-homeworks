package com.company;

import com.company.enums.*;
import com.company.products.Furniture;
import com.company.products.Bookcase;
import com.company.products.OfficeCabinet;
import com.company.products.OfficeChair;
import com.company.products.MeetingTable;
import com.company.users.*;

import java.util.Scanner;

/**
 * The main class of the automation system.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class Main {
    private static AutomationSystem system;
    private static SuperAdmin systemAdmin;
    private static Scanner sc = new Scanner(System.in);

    /**
     * The main method of the automation system.
     * It will be used as a driver method.
     *
     * It contains a menu with two options.
     * Option 1 shows the user testing results of the program.
     * Option 2 lets the user use the program as her/his own.
     *
     * @param args the command line arguments.
     */
    public static void main(String[] args) {
        initSystem();
        System.out.print("Welcome to the Automation System!\n" +
                            "1-) Run test cases\n" +
                            "2-) Use it on your own\n" +
                            "Select: ");
        int selected = sc.nextInt();
        switch (selected) {
            case 1: runTests(); break;
            case 2: useSystem(); break;
        }
    }

    private static void initSystem() {
        // Initialize the automation system and add a system admin to it
        system = new AutomationSystem("Omer's Place");
        systemAdmin = new SuperAdmin("Omer", "Bitikcioglu", "omer@test.com", "123456", system);
        system.addUser(systemAdmin);

        // Add four different branches to the system initially
        Branch branch1 = new Branch("Branch One");
        systemAdmin.addBranchToSystem(branch1);

        Branch branch2 = new Branch("Branch Two");
        systemAdmin.addBranchToSystem(branch2);

        Branch branch3 = new Branch("Branch Three");
        systemAdmin.addBranchToSystem(branch3);

        Branch branch4 = new Branch("Branch Four");
        systemAdmin.addBranchToSystem(branch4);
    }

    private static void runTests() {

        runAdminTests();
        runCustomerTests();
        runEmployeeTests();

    }

    private static void runEmployeeTests() {
        int numOfTests = 0;
        System.out.println("\n-------------------\n" + "BranchEmployee Tests\n" + "-------------------");

        // Add a branch employee to the 2nd branch for testing purposes
        Branch testBranch = system.getBranches().getItem(1);
        BranchEmployee testEmployee = new BranchEmployee("Test", "Employee", "employee@test.com", "test123");
        systemAdmin.addBranchEmployeeToBranch(testBranch, testEmployee);

        // Add a furniture to the testBranch for testing
        Furniture testProduct = new OfficeChair(50, OfficeChairModel.CLASSIC, OfficeChairColor.GRAY);
        testEmployee.addProduct(testProduct,5);

        // BranchEmployee can inquire about stock information of a product
        printTestHeader(++numOfTests);

        testEmployee.inquireProductStock(testProduct);

        // BranchEmployee can inform the managers
        printTestHeader(++numOfTests);

        testEmployee.informManager();

        // BranchEmployee can add & remove products to/from branch

        // BranchEmployee can add products to the branch

        // BranchEmployee can add an existing product to the branch
        printTestHeader(++numOfTests);
        testAddProduct(testProduct,testEmployee,2);

        // BranchEmployee can add a non existing product to the branch
        printTestHeader(++numOfTests);
        Furniture newTestProduct = new MeetingTable(500, MeetingTableModel.MODERN, MeetingTableColor.BEIGE);
        testAddProduct(newTestProduct, testEmployee, 1);

        // BranchEmployee can remove products from the branch

        // Remove existing number of products
        printTestHeader(++numOfTests);
        testRemoveProduct(testProduct, testEmployee, 3);

        // Remove all stock of the product
        printTestHeader(++numOfTests);
        testRemoveProduct(testProduct, testEmployee,4);

        // Remove non-existing number of product
        printTestHeader(++numOfTests);
        testRemoveProduct(testProduct, testEmployee, 1);

        // Remove more than existing product
        printTestHeader(++numOfTests);
        testRemoveProduct(newTestProduct, testEmployee, 5);

        // BranchEmployee can sell products
        Customer testCustomer = new Customer("Test", "Customer", "test@customer.com", "123456");
        system.addUser(testCustomer);
        testCustomer.subscribe(system);

        // Sell existing number of product
        printTestHeader(++numOfTests);
        testSale(testEmployee,testCustomer,newTestProduct,1);

        // Sell non-existing number of product
        printTestHeader(++numOfTests);
        testSale(testEmployee,testCustomer,newTestProduct,5);

        // BranchEmployee can view previous orders of customer by using customer number
        printTestHeader(++numOfTests);
        testEmployee.viewPreviousOrders(system, testCustomer.getCustomerNumber());

    }

    private static void testSale(BranchEmployee testEmployee, Customer testCustomer, Furniture testProduct, int amount) {
        String testBranchName = testEmployee.getBranch().getBranchName();
        String testProductName = testProduct.getName();
        int earningsBefore = testEmployee.getBranch().getEarnings();
        int earningsAfter;

        System.out.println("Earnings of " + testBranchName + " before: " + earningsBefore);

        testEmployee.sell(testCustomer, testProduct, amount);
        earningsAfter = testEmployee.getBranch().getEarnings();

        System.out.println("Earnings of " + testBranchName + " after selling " + amount + " " + testProductName + ": " + earningsAfter);

        if(earningsBefore + amount*testProduct.getPrice() == earningsAfter) {
            printTestResult("Selling furniture test", TestResult.PASSED);
        } else {
            printTestResult("Selling furniture test", TestResult.FAILED);
        }
    }

    private static void testRemoveProduct(Furniture testProduct, BranchEmployee testEmployee, int amount) {
        int amountBefore = testProduct.getStock();
        int amountAfter;

        System.out.println("Number of " + testProduct.getName() + " before: " + amountBefore);

        testEmployee.removeProduct(testProduct, amount);
        amountAfter = testProduct.getStock();

        System.out.println("After " + amount + " of " + testProduct.getName() + " removed: " + amountAfter);

        if(amountBefore - amount == amountAfter) {
            printTestResult("Removing product test", TestResult.PASSED);
        } else {
            printTestResult("Removing product test", TestResult.FAILED);
        }
    }

    private static void testAddProduct(Furniture testProduct, BranchEmployee testEmployee, int amount) {
        int amountBefore = testProduct.getStock();
        int amountAfter;

        System.out.println("Number of " + testProduct.getName() + " before: " + amountBefore);

        testEmployee.addProduct(testProduct, amount);
        amountAfter = testProduct.getStock();

        System.out.println("After " + amount + " of " + testProduct.getName() + " added: " + amountAfter);

        if(amountBefore + amount == amountAfter) {
            printTestResult("Adding product test", TestResult.PASSED);
        } else {
            printTestResult("Adding product test", TestResult.FAILED);
        }
    }

    private static void runCustomerTests() {
        int numOfTests = 0;
        System.out.println("\n-------------------\n" + "Customer Tests\n" + "-------------------");

        Customer testCustomer = new Customer("Test", "Customer", "customer@test.com", "test123");
        system.addUser(testCustomer);

        // Customer can subscribe to system and get a customer number
        printTestHeader(++numOfTests);

        testCustomer.subscribe(system);

        // Customer can search for a product and see the list of products they searched
        printTestHeader(++numOfTests);

        // Add a branch employee to the first branch for testing
        BranchEmployee testEmployee = new BranchEmployee("Test", "Employee", "employee@test.com", "test123");
        system.addUser(testEmployee);
        Branch testBranch = system.getBranches().getItem(0);

        systemAdmin.addBranchEmployeeToBranch(testBranch, testEmployee);

        // Add a bookcase to the branch for testing
        Furniture testProduct = new Bookcase(100, BookcaseModel.WOODEN);
        testEmployee.addProduct(testProduct, 1);

        testCustomer.searchProducts("BOOKCASE");

        // Customer can se which store has the product they looking for
        printTestHeader(++numOfTests);

        testCustomer.seeWhereTheProduct(testProduct);

        // Customer can shop online with her/his address and phone number

        // Test with existing product in the testBranch
        printTestHeader(++numOfTests);

        testCustomer.shopOnlineTest(testBranch, testProduct);

        // Test with non existing product in the testBranch
        printTestHeader(++numOfTests);

        Furniture nonExisting = new OfficeCabinet(250, OfficeCabinetModel.CARD);
        testCustomer.shopOnlineTest(testBranch,nonExisting);

        // Customer can view his/her previous orders
        printTestHeader(++numOfTests);

        testCustomer.viewOrders();
    }

    private static void runAdminTests() {
        int numOfTests = 0;
        System.out.println("\n-------------------\n" + "Admin Tests\n" + "-------------------");

        // SuperAdmin can add & remove Admin to/ from the system

        // SuperAdmin can add Admin to the system
        printTestHeader(++numOfTests);

        Admin newAdmin;

        newAdmin = new Admin("John", "Lennon", "jl@test.com", "john123", system);
        systemAdmin.addAdminToSystem(newAdmin);

        if (system.getUsers().searchFor(newAdmin)) {
            printTestResult("SuperAdmin can add admin to system", TestResult.PASSED);
            System.out.println("Added admin: " + newAdmin.toString());
        } else {
            printTestResult("Admin can add branches", TestResult.FAILED);
        }

        // SuperAdmin can remove Admin from the system
        printTestHeader(++numOfTests);

        systemAdmin.removeAdminFromSystem(newAdmin);

        if(!(system.getUsers().searchFor(newAdmin))) {
            printTestResult("SuperAdmin can remove admin from system", TestResult.PASSED);
        } else {
            printTestResult("SuperAdmin can remove admin from system", TestResult.FAILED);
        }

        // Added admin can use its administrative methods

        // Admin can add branch to the system
        printTestHeader(++numOfTests);

        Branch newBranch;

        newBranch = new Branch("Uskudar");
        systemAdmin.addBranchToSystem(newBranch);

        if (system.getBranches().searchFor(newBranch)) {
            printTestResult("Admin can add branches", TestResult.PASSED);
            System.out.println("Added branch name: " + newBranch.getBranchName());
        } else {
            printTestResult("Admin can add branches", TestResult.FAILED);
        }

        // Admin can remove branch from the system
        printTestHeader(++numOfTests);

        systemAdmin.removeBranchFromSystem(newBranch);

        if (!(system.getBranches().searchFor(newBranch))) {
            printTestResult("Admin can remove branches", TestResult.PASSED);
        } else {
            printTestResult("Admin can remove branches", TestResult.FAILED);
        }

        // Admin can add/remove branch employee to/from a branch

        // Admin can add branch employee to a branch
        printTestHeader(++numOfTests);

        BranchEmployee newEmployee;

        newEmployee = new BranchEmployee("Test","Employee", "employee@test.com", "test123");
        Branch branch1 = system.getBranches().getItem(0);
        systemAdmin.addBranchEmployeeToBranch(branch1, newEmployee);

        if(branch1.getEmployees().searchFor(newEmployee)) {
            printTestResult("Admin can add branch employee to the system", TestResult.PASSED);
            System.out.println("Added branch employee: " + newEmployee.toString());
        } else {
            printTestResult("Admin can add branch employee to the system", TestResult.FAILED);
        }

        // Admin can remove branch employee from a branch
        printTestHeader(++numOfTests);

        systemAdmin.removeBranchEmployeeFromBranch(branch1, newEmployee);

        if(!(branch1.getEmployees().searchFor(newEmployee))) {
            printTestResult("Admin can remove branch employee from a branch", TestResult.PASSED);
        } else {
            printTestResult("Admin can remove branch employee from a branch", TestResult.FAILED);
        }

        // Admin can query for products that need to be supplied
        printTestHeader(++numOfTests);

        // Add an employe for testing
        newEmployee = new BranchEmployee("Test","Employee", "employee@test.com", "test123");
        systemAdmin.addBranchEmployeeToBranch(branch1, newEmployee);

        // Add a product for testing
        Furniture newProduct = new OfficeChair(150, OfficeChairModel.CREATIVE, OfficeChairColor.ORANGE);
        Furniture anotherProduct = new OfficeChair(500, OfficeChairModel.LUXURY, OfficeChairColor.GRAY);

        // Add first product enough amount of stock. Second product is need to be supplied more.
        newEmployee.addProduct(newProduct, 10); // Amount is greater than 5
        newEmployee.addProduct(anotherProduct, 2); // Amount is less than 5

        systemAdmin.queryForSupply();
    }

    private static void printTestResult(String s, TestResult result) {
        System.out.println(s + ": " + result.toString());
    }

    private static void printTestHeader(int testNum){
        System.out.println("\nTest " + testNum + "\n----------");
    }

    private static void useSystem() {
        int selected;
        superAdminMenu(systemAdmin);

//        System.out.println("1-) Login");
//        System.out.println("2-) Register");
//        System.out.print("Select: ");
//        selected = sc.nextInt();
//
//        switch (selected) {
//            case 1: loginToSystem(); break;
//            case 2: registerToSystem(); break;
//        }
    }

    private static void loginToSystem() {
        String email;
        String password;

        System.out.print("Email: ");
        email = sc.next();

        System.out.print("Password: ");
        password = sc.next();

        try {
            User unknownUser = new UnknownUser("Unknown", "Unknown", email, password);
            User userToLogin;
            int userIndex = system.getUsers().searchForIndex(unknownUser);
            userToLogin = system.getUsers().getItem(userIndex);

            if(userToLogin instanceof SuperAdmin) {
                superAdminMenu((SuperAdmin) userToLogin);
            } else if(userToLogin instanceof Admin) {
                adminMenu((Admin) userToLogin);
            } else if (userToLogin instanceof BranchEmployee) {
                employeeMenu((BranchEmployee) userToLogin);
            } else {
                customerMenu((Customer) userToLogin);
            }
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    private static void superAdminMenu(SuperAdmin superAdminUser) {
        boolean showMenu = true;
        while (showMenu){
            System.out.println("Super Admin Menu");
            System.out.println("1-) Add Branch");
            System.out.println("2-) Remove Branch");
            System.out.println("3-) Add Branch Employee");
            System.out.println("4-) Remove Branch Employee");
            System.out.println("5-) Add Admin");
            System.out.println("6-) Remove Admin");
            System.out.println("7-) Query for Supply");
            System.out.println("0-) Exit");

            System.out.print("Select: ");
            int selected = sc.nextInt();

            switch (selected) {
                case 1: addBranch(superAdminUser); break;
                case 2: removeBranch(superAdminUser); break;
                case 3: addBranchEmployee(superAdminUser); break;
                case 4: removeBranchEmployee(superAdminUser); break;
                case 5: queryForSupply(superAdminUser); break;
                default: showMenu = false;
            }
        }
    }

    private static void adminMenu(Admin adminUser) {
        boolean showMenu = true;
        while (showMenu) {
            System.out.println("Admin Menu");
            System.out.println("1-) Add Branch");
            System.out.println("2-) Remove Branch");
            System.out.println("3-) Add Branch Employee");
            System.out.println("4-) Remove Branch Employee");
            System.out.println("5-) Query for Supply");
            System.out.println("0-) Exit");

            System.out.print("Select: ");
            int selected = sc.nextInt();

            switch (selected) {
                case 1: addBranch(adminUser); break;
                case 2: removeBranch(adminUser); break;
                case 3: addBranchEmployee(adminUser); break;
                case 4: removeBranchEmployee(adminUser); break;
                case 5: queryForSupply(adminUser); break;
                default: showMenu = false;
            }
        }
    }
    private static void addBranch(Admin adminUser) {
        System.out.print("Enter the name of branch: ");
        String branchName = sc.next();

        Branch newBranch = new Branch(branchName);
        adminUser.addBranchToSystem(newBranch);
        System.out.println(branchName + " added to system!");
    }

    private static void removeBranch(Admin adminUser) {
        System.out.print("Enter the name of branch: ");
        String branchName = sc.next();
        Branch branchToBeRemoved = new Branch(branchName);

        if(system.getBranches().searchFor(branchToBeRemoved)) {
            adminUser.removeBranchFromSystem(branchToBeRemoved);
            System.out.println(branchName + " is removed!");
        } else {
            System.out.println("Branch is not found!");
        }
    }

    private static void addBranchEmployee(Admin adminUser) {
        System.out.print("Enter the name of branch: ");
        String branchName = sc.next();
        Branch branch = new Branch(branchName);

        // TODO
        //adminUser.addBranchEmployeeToBranch();
    }

    private static void removeBranchEmployee(Admin adminUser) {
    }

    private static void queryForSupply(Admin adminUser) {
    }

    private static void employeeMenu(BranchEmployee employeeUser) {
    }

    private static void customerMenu(Customer customerUser) {
    }

    private static void registerToSystem() {
        String name;
        String surname;
        String email;
        String password;

        System.out.print("Name: ");
        name = sc.next();

        System.out.print("Surname: ");
        surname = sc.next();

        System.out.print("Email: ");
        email = sc.next();

        System.out.print("Password: ");
        password = sc.next();

        Customer newUser = new Customer(name, surname, email, password);
        system.addUser(newUser);
    }
}

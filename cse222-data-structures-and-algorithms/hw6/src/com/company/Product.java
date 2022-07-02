package com.company;

import java.util.LinkedList;

/**
 * Product class holds the information of products.
 *
 * @author Ömer Faruk Bitikçioğlu
 */
public class Product {
    private String id;
    private String name;
    private String category;
    private int price;
    private int discountedPrice;
    private String description;

    /**
     * Constructor of Product class
     * @param id the ID of the product
     * @param name the name of the product
     * @param category the category tree of the product
     * @param price the price of the product
     * @param discountedPrice the discounted price of the product
     * @param description the description of the product
     */
    public Product(String id, String name, String category, int price, int discountedPrice, String description) {
        this.id = id;
        this.name = name;
        this.category = category;
        this.price = price;
        this.discountedPrice = discountedPrice;
        this.description = description;
    }

    public String getId() {
        return id;
    }
}

package com.company.products;

/**
 * Furniture class.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public abstract class Furniture {
    private String name;
    private int price;
    private int stock;

    /**
     * Constructor of Furniture class.
     *
     * @param price the price of the furniture.
     * @param name the name of the furniture
     */
    public Furniture(int price, String name) {
        this.price = price;
        this.name = name;
    }

    /**
     * Gets the name of the furniture.
     *
     * @return the name of the furniture.
     */
    public String getName() {
        return name;
    }

    /**
     * Gets the price of the furniture.
     *
     * @return the price of the furniture.
     */
    public int getPrice() {
        return price;
    }

    public void setPrice(int updatedPrice) {this.price = updatedPrice;}

    /**
     * Gets the stock information of the furniture.
     *
     * @return the stock information of the furniture.
     */
    public int getStock() {
        return stock;
    }

    /**
     * Sets the stock information of the furniture.
     *
     * @param stock the amount of stock to be set for the furniture.
     */
    public void setStock(int stock) {
        this.stock = stock;
    }

    @Override
    public boolean equals(Object obj) {
        if(this == obj) return true;
        else if(obj instanceof Furniture) {
            Furniture furnitureObj = (Furniture) obj;
            return this.name.compareTo(furnitureObj.name) == 0;
        } else {
            return false;
        }
    }
}

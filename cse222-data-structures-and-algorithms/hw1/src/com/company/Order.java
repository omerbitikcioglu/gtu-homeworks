package com.company;

import java.time.LocalDate;

/**
 * Order information hold by this class.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class Order {
    private final String productName;
    private final int amountPaid;
    private final LocalDate date;

    /**
     * @param productName the name of the product customer bought
     * @param amountPaid the amount of money customer paid for this order
     * @param date the date of the order
     */
    public Order(String productName, int amountPaid, LocalDate date) {
        this.productName = productName;
        this.amountPaid = amountPaid;
        this.date = date;
    }

    @Override
    public String toString() {
        return productName + ", bought for $" + amountPaid + ", on " + date;
    }
}

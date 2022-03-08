package com.company.products;

import com.company.enums.OfficeChairColor;
import com.company.enums.OfficeChairModel;

/**
 * OfficeChair class for office chairs.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class OfficeChair extends Furniture {
    private final OfficeChairModel model;
    private final OfficeChairColor color;

    /**
     * Constructor of OfficeChair class.
     *
     * @param color the color of the office chair.
     * @param price the price of the office chair.
     * @param model the model of the office chair.
     */
    public OfficeChair(int price, OfficeChairModel model, OfficeChairColor color) {
        super(price, color.toString() + " " + model.toString() + " OFFICE CHAIR");
        this.model = model;
        this.color = color;
    }
}

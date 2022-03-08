package com.company.products;

import com.company.enums.BookcaseModel;

/**
 * Bookcase class for bookcases.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class Bookcase extends Furniture {
    private final BookcaseModel model;

    /**
     * Constructor of Bookcase class.
     *
     * @param price the price of the bookcase.
     * @param model the model of the bookcase.
     */
    public Bookcase(int price, BookcaseModel model) {
        super(price, model.toString() + " BOOKCASE");
        this.model = model;
    }
}

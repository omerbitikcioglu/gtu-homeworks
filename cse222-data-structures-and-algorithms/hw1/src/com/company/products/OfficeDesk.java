package com.company.products;

import com.company.enums.OfficeDeskColor;
import com.company.enums.OfficeDeskModel;

/**
 * OfficeDesk class for office chairs.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class OfficeDesk extends Furniture {
    private final OfficeDeskModel model;
    private final OfficeDeskColor color;

    /**
     * Constructor of OfficeCabinet class.
     *
     * @param color the color of the office desk.
     * @param price the price of the office desk.
     * @param model the model of the office desk.
     */
    public OfficeDesk(int price, OfficeDeskModel model, OfficeDeskColor color) {
        super(price, color.toString() + " " + model.toString() + " OFFICE DESK");
        this.model = model;
        this.color = color;
    }

}

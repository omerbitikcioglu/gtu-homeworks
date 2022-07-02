package com.company.products;

import com.company.enums.OfficeCabinetModel;

/**
 * OfficeCabinet class for office cabinets.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class OfficeCabinet extends Furniture {
    private final OfficeCabinetModel model;

    /**
     * Constructor of OfficeCabinet class.
     *
     * @param price the price of the office cabinet.
     * @param model the model of the office cabinet.
     */
    public OfficeCabinet(int price, OfficeCabinetModel model) {
        super(price, model.toString() + " OFFICE CABINET");
        this.model = model;
    }
}

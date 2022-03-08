package com.company.products;

import com.company.enums.MeetingTableColor;
import com.company.enums.MeetingTableModel;

/**
 * MeetingTable class for meeting tables.
 *
 * @author Ömer Faruk Bitikçioğlu
 * @version %I% %G%
 * @since 1.0
 */
public class MeetingTable extends Furniture {
    private final MeetingTableModel model;
    private final MeetingTableColor color;

    /**
     * Constructor of MeetingTable class.
     * @param price price of the meeting table.
     * @param model model of the meeting table.
     * @param color color of the meeting table.
     */
    public MeetingTable(int price, MeetingTableModel model, MeetingTableColor color) {
        super(price, color.toString() + " " + model.toString() + " MEETING TABLE");
        this.model = model;
        this.color = color;
    }
}

CLASS zcl_work_order_info_jb DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_info_jb IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    " ztp_customer

*    INSERT ztp_customer
*        FROM @( VALUE #(
*        customer_id = `1`
*        name = `JOSE`
*        address = `GUAYAQUIL`
*        phone = `099999999`
*          ) ).
*
*
*    IF sy-subrc = 0.
*      out->write( 'Customer Inserted' ).
*    ELSE.
*      out->write( 'Customer not Inserted' ).
*    ENDIF.



    " ztp_technician

*     INSERT ztp_technician
*        FROM TABLE @( VALUE #(
*        ( technician_id = `1`  name = `JOSE-TECH`
*        specialty = `MAINTENANCE MACHINE` )
*        ( technician_id = `2`  name = `LUIS-TECH`
*        specialty = `MAINTENANCE PRINTER` )
*          ) ).
*
*    IF sy-subrc = 0.
*      out->write( 'technician Inserted' ).
*    ELSE.
*      out->write( 'technician not Inserted' ).
*    ENDIF.


    " ztp_priority

    INSERT ztp_priority FROM TABLE @( VALUE #(
      ( priority_code = `A` priority_description = `High` )
      ( priority_code = `B` priority_description = `Low`  )
    ) ).

    IF sy-subrc = 0.
      out->write( 'priority Inserted' ).
    ELSE.
      out->write( 'priority not Inserted' ).
    ENDIF.


   " ztp_status

    INSERT ztp_status FROM TABLE @( VALUE #(
      ( status_code = `PE` status_description = `Pending` )
      ( status_code = `CO` status_description = `Completed`  )
    ) ).

    IF sy-subrc = 0.
      out->write( 'status Inserted' ).
    ELSE.
      out->write( 'status not Inserted' ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

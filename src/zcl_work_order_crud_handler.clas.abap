CLASS zcl_work_order_crud_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA: mt_workorder        TYPE TABLE OF ztp_work_order.

    METHODS:
      create_work_order
        IMPORTING
                  iv_workorderid   TYPE zde_work_order_id_jb
                  iv_customerid    TYPE zde_customer_id_jb
                  iv_technicianid  TYPE zde_technician_id_jb
                  iv_creation_date TYPE d
                  iv_status        TYPE string
                  iv_priority      TYPE string
                  iv_description   TYPE string
        RETURNING VALUE(rv_valid)  TYPE abap_bool,

      update_work_order
        IMPORTING
                  iv_workorderid   TYPE zde_work_order_id_jb
                  iv_customerid    TYPE zde_customer_id_jb
                  iv_technicianid  TYPE zde_technician_id_jb
                  iv_creation_date TYPE d
                  iv_status        TYPE string
                  iv_priority      TYPE string
                  iv_description   TYPE string
        RETURNING VALUE(rv_valid)  TYPE abap_bool,

      delete_work_order
        IMPORTING iv_workorderid  TYPE zde_work_order_id_jb
        RETURNING VALUE(rv_valid) TYPE abap_bool,

      read_work_order
        IMPORTING iv_workorderid      TYPE zde_work_order_id_jb
        RETURNING VALUE(rv_workorder) TYPE ztp_work_order.

  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_work_order_crud_handler IMPLEMENTATION.

  METHOD create_work_order.


    DATA(lo_order_validator) = NEW zcl_work_order_validator_jb( ).

    rv_valid = lo_order_validator->validate_create_order(
                            iv_customer_id = iv_customerid
                            iv_technician_id   = iv_technicianid
                            iv_priority        = iv_priority
                        ).


    IF rv_valid = abap_false.
      "  work order exists
      RETURN rv_valid.
    ENDIF.

    IF rv_valid = abap_true.
      "  work order no exists

      INSERT ztp_work_order
      FROM @( VALUE #(
      work_order_id = iv_workorderid
      customer_id = iv_customerid
      technician_id = iv_technicianid
      creation_date = iv_creation_date
      status = iv_status
      priority = iv_priority
      description = iv_description   ) ).

      IF sy-subrc = 0.
        rv_valid = abap_true.
      ELSE.
        rv_valid = abap_false.
      ENDIF.

      RETURN rv_valid.
    ENDIF.

  ENDMETHOD.

  METHOD update_work_order.

    DATA(lo_order_validator) = NEW zcl_work_order_validator_jb( ).

    rv_valid = lo_order_validator->validate_update_order(
                            iv_work_order_id = iv_workorderid
                            iv_status        = iv_status
                        ).


    IF rv_valid = abap_false.
      "  work order no update
      RETURN rv_valid.
    ENDIF.

    rv_valid = lo_order_validator->validate_create_order(
                             iv_customer_id = iv_customerid
                             iv_technician_id   = iv_technicianid
                             iv_priority        = iv_priority
                         ).

    IF rv_valid = abap_false.
      "  work order no update
      RETURN rv_valid.
    ENDIF.

    IF rv_valid = abap_true.
      "  work order update

      UPDATE ztp_work_order
      SET   customer_id = @iv_customerid,
            technician_id = @iv_technicianid,
            creation_date = @iv_creation_date,
            status   = @iv_status,
            priority = @iv_priority,
            description = @iv_description
      WHERE work_order_id = @iv_workorderid.

      IF sy-subrc = 0.
        rv_valid = abap_true.
      ELSE.
        rv_valid = abap_false.
      ENDIF.

      RETURN rv_valid.
    ENDIF.

  ENDMETHOD.

  METHOD delete_work_order.

    DATA(lo_order_validator) = NEW zcl_work_order_validator_jb( ).

    DATA: ls_workorder TYPE ztp_work_order.

    rv_valid = lo_order_validator->validate_delete_order(
                            iv_work_order_id = iv_workorderid
                        ).

    IF rv_valid = abap_false.
      "  work order no delete
      RETURN rv_valid.
    ENDIF.

    IF rv_valid = abap_true.
      "  work order delete

      SELECT SINGLE FROM ztp_work_order
      FIELDS *
       WHERE work_order_id = @iv_workorderid INTO @ls_workorder.

      IF sy-subrc = 0.

        DELETE ztp_work_order FROM @ls_workorder.

        IF sy-subrc = 0.
          rv_valid = abap_true.
        ELSE.
          rv_valid = abap_false.
        ENDIF.

      ENDIF.

      RETURN rv_valid.

    ENDIF.

  ENDMETHOD.

  METHOD read_work_order.

    SELECT SINGLE FROM ztp_work_order
    FIELDS *
    WHERE work_order_id = @iv_workorderid INTO @rv_workorder.

    RETURN rv_workorder.

  ENDMETHOD.


ENDCLASS.

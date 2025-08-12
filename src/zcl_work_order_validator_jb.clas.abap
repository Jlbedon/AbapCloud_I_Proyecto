CLASS zcl_work_order_validator_jb DEFINITION PUBLIC  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      validate_create_order
        IMPORTING
                  iv_customer_id   TYPE zde_customer_id_jb
                  iv_technician_id TYPE zde_technician_id_jb
                  iv_priority      TYPE string
        RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_update_order
        IMPORTING
                  iv_work_order_id TYPE zde_work_order_id_jb
                  iv_status        TYPE string
        RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_delete_order
        IMPORTING
                  iv_work_order_id TYPE zde_work_order_id_jb
        RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_status_and_priority
        IMPORTING
                  iv_status       TYPE string
                  iv_priority     TYPE string
        RETURNING VALUE(rv_valid) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_valid_status   TYPE string VALUE 'PE CO', " Example statuses: Pending, Completed
      c_valid_priority TYPE string VALUE 'A B'. " Example priorities: High, Low


    METHODS check_customer_exists
      IMPORTING iv_customer_id  TYPE zde_customer_id_jb
      RETURNING VALUE(rv_valid) TYPE abap_bool.

    METHODS check_technician_exists
      IMPORTING iv_technician_id TYPE zde_technician_id_jb
      RETURNING VALUE(rv_valid)  TYPE abap_bool.

    METHODS check_order_exists
      IMPORTING iv_work_order_id TYPE zde_work_order_id_jb
      RETURNING VALUE(rv_valid)  TYPE abap_bool.

    METHODS check_order_history
      IMPORTING iv_work_order_id TYPE zde_work_order_id_jb
      RETURNING VALUE(rv_valid)  TYPE abap_bool.

    METHODS check_order_status
      IMPORTING iv_work_order_id TYPE zde_work_order_id_jb
      RETURNING VALUE(rv_valid)  TYPE abap_bool.

ENDCLASS.



CLASS zcl_work_order_validator_jb IMPLEMENTATION.

  METHOD validate_create_order.

    " Check if customer exists
    DATA(lv_customer_exists) = check_customer_exists( iv_customer_id ).
    IF lv_customer_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if technician exists
    DATA(lv_technician_exists) = check_technician_exists( iv_technician_id ).
    IF lv_technician_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if priority is valid
    FIND iv_priority IN c_valid_priority.

    IF sy-subrc <> 0.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_update_order.

    " Check if the work order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists = abap_false.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is editable (e.g., Pending)

    FIND iv_status IN c_valid_status.

    IF sy-subrc <> 0.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_delete_order.

    " Check if the order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists = abap_false.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is "PE" (Pending)

    DATA(lv_order_status) = check_order_status( iv_work_order_id ).
    IF lv_order_status = abap_false.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order has a history (i.e., if it has been modified before)
    DATA(lv_has_history) = check_order_history( iv_work_order_id ).
    IF lv_has_history = abap_true.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_status_and_priority.

    " Validate the status value
    FIND iv_status IN c_valid_status.

    IF sy-subrc <> 0.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validate the priority value
    FIND iv_priority IN c_valid_priority.

    IF sy-subrc <> 0.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD check_customer_exists.

    " VALIDAR SI EXISTE CUSTOMER
    rv_valid = abap_false.

    DATA: gt_customer TYPE STANDARD TABLE OF ztp_customer.

    SELECT FROM ztp_customer
    FIELDS *
    WHERE customer_id = @iv_customer_id
    INTO TABLE @gt_customer.

    IF sy-subrc = 0.
      rv_valid = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD check_technician_exists.

    " VALIDAR SI EXISTE technician
    rv_valid = abap_false.

    DATA: gt_technician TYPE STANDARD TABLE OF ztp_technician.

    SELECT FROM ztp_technician
    FIELDS *
    WHERE technician_id = @iv_technician_id
    INTO TABLE @gt_technician.

    IF sy-subrc = 0.
      rv_valid = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD check_order_exists.

    " VALIDAR SI EXISTE ORDER
    rv_valid = abap_false.

    DATA: gt_work_order TYPE STANDARD TABLE OF ztp_work_order.

    SELECT FROM ztp_work_order
    FIELDS *
    WHERE work_order_id = @iv_work_order_id
    INTO TABLE @gt_work_order.

    IF sy-subrc = 0.
      rv_valid = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD check_order_status.

    " VALIDAR STATUS ORDER
    rv_valid = abap_false.

    DATA: gt_work_order TYPE STANDARD TABLE OF ztp_work_order.

    SELECT FROM ztp_work_order
    FIELDS *
    WHERE work_order_id = @iv_work_order_id
    AND status = 'PE'
    INTO TABLE @gt_work_order.

    IF sy-subrc = 0.
      rv_valid = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD check_order_history.

    " VALIDAR SI EXISTE ORDER HISTORY
    rv_valid = abap_false.

    DATA: gt_work_orderhst TYPE STANDARD TABLE OF ztp_workorderhst.

    SELECT FROM ztp_workorderhst
    FIELDS *
    WHERE work_order_id = @iv_work_order_id
    INTO TABLE @gt_work_orderhst.

    IF sy-subrc = 0.
      rv_valid = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

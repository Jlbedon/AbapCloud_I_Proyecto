CLASS zcl_work_order_crud_test_jb DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    DATA: lv_workorderid   TYPE zde_work_order_id_jb,
          lv_customer_id   TYPE zde_customer_id_jb,
          lv_technicianid  TYPE zde_technician_id_jb,
          lv_creation_date TYPE d,
          lv_status        TYPE string,
          lv_priority      TYPE string,
          lv_description   TYPE string.

    METHODS:
      test_create_work_order IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out,
      test_read_work_order IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out,
      test_update_work_order IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out,
      test_delete_work_order IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_crud_test_jb IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    lv_workorderid = '1'.
    lv_customer_id = '1'.
    lv_technicianid = '1'.
    lv_creation_date = '20250811'.
    lv_status = 'PE'.
    lv_priority = 'A'.
    lv_description = 'DESCRIPCION ORDER'.

*    test_create_work_order( out ).
*    test_update_work_order( out ).
*    test_delete_work_order( out ).
     test_read_work_order( out ).


  ENDMETHOD.

  METHOD test_create_work_order.


    DATA(lo_crud_workorder) = NEW zcl_work_order_crud_handler( ).

    DATA(rv_valid) = lo_crud_workorder->create_work_order(
    iv_workorderid = lv_workorderid
    iv_customerid = lv_customer_id
    iv_technicianid   = lv_technicianid
    iv_priority        = lv_priority
    iv_creation_date   = lv_creation_date
    iv_status          = lv_status
    iv_description     = lv_description
  ).


    IF rv_valid = abap_true.
        ir_out->write( |Work Order: { lv_workorderid } Inserted | ).
    ELSE.
      ir_out->write( |Work Order: { lv_workorderid } Not Inserted | ).
    ENDIF.

  ENDMETHOD.

  METHOD test_update_work_order.


    DATA(lo_crud_workorder) = NEW zcl_work_order_crud_handler( ).

    DATA(rv_valid) = lo_crud_workorder->update_work_order(
    iv_workorderid = lv_workorderid
    iv_customerid = lv_customer_id
    iv_technicianid   = lv_technicianid
    iv_priority        = lv_priority
    iv_creation_date   = lv_creation_date
    iv_status          = lv_status
    iv_description     = lv_description
  ).


    IF rv_valid = abap_true.
      ir_out->write( |Work Order: { lv_workorderid } Update | ).

    ELSE.
      ir_out->write( |Work Order: { lv_workorderid } Not Update | ).
    ENDIF.

  ENDMETHOD.

  METHOD test_delete_work_order.

    DATA(lo_crud_workorder) = NEW zcl_work_order_crud_handler( ).

    DATA(rv_valid) = lo_crud_workorder->delete_work_order( iv_workorderid = lv_workorderid  ).


    IF rv_valid = abap_true.
      ir_out->write( |Work Order: { lv_workorderid } Delete | ).
    ELSE.
      ir_out->write( |Work Order: { lv_workorderid } Not Delete | ).
    ENDIF.

  ENDMETHOD.

  METHOD test_read_work_order.

    DATA(lo_crud_workorder) = NEW zcl_work_order_crud_handler( ).

    DATA(rv_workorder) = lo_crud_workorder->read_work_order( iv_workorderid = lv_workorderid  ).

    IF rv_workorder IS INITIAL.
      ir_out->write( |Work Order: { lv_workorderid } Not Exists | ).

    ELSE.
      ir_out->write( rv_workorder ).

    ENDIF.

  ENDMETHOD.


ENDCLASS.

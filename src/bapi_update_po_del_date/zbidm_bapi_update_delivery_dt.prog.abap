*&---------------------------------------------------------------------*
*& Report  ZTESTE_BIDM_042
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zbidm_bapi_update_delivery_dt.


DATA:  l_ponumber  TYPE          bapimepoheader-po_number      ,
       i_schedule  TYPE TABLE OF bapimeposchedule WITH HEADER LINE, " declara uma tabela e estrutura ao mesmo tempo
       i_schedulex TYPE TABLE OF bapimeposchedulx WITH HEADER LINE,
       i_return    TYPE TABLE OF bapiret2         WITH HEADER LINE.

l_ponumber                = '4500000000'.
i_schedule-po_item        = '00010'.
i_schedule-sched_line     = '0001'.
i_schedule-delivery_date  = '20200721'.
i_schedulex-po_item       = i_schedule-po_item.
i_schedulex-sched_line    = i_schedule-sched_line.
i_schedulex-po_itemx      = 'X'.
i_schedulex-sched_linex   = 'X'.
i_schedulex-delivery_date = 'X'.

APPEND: i_schedule,
        i_schedulex.

CALL FUNCTION 'BAPI_PO_CHANGE'
  EXPORTING
    purchaseorder  = l_ponumber
    no_messaging   = 'X' " evita imprimir pedido
    no_message_req = 'X' " evita imprimir pedido
  TABLES
    return         = i_return
    poschedule     = i_schedule
    poschedulex    = i_schedulex.

BREAK-POINT.
READ TABLE i_return WITH KEY type = 'E'.
IF sy-subrc IS INITIAL.

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .

ELSE.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

ENDIF.

*----------------------------------------------------------------------*
***INCLUDE ZPP025_O01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'ZPP002'.
  SET TITLEBAR 'ZTITULO'.
ENDMODULE.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV  OUTPUT
*&---------------------------------------------------------------------*
MODULE create_alv OUTPUT.

  CASE p_etiq.
    WHEN 'OP' OR 'OPKOI' OR 'OPPIN' OR 'OPMAG' OR 'EPSON'.
      go_print->create_alv(
      CHANGING
        c_table = go_print->ct_op
    ).

    WHEN 'FICOSA'.
      go_print->create_alv(
       CHANGING
         c_table = go_print->ct_ficosa
     ).

    WHEN 'MAGNA' OR 'HUF' OR 'BEZEL_SE'.
      go_print->create_alv(
      CHANGING
       c_table = go_print->ct_magna
      ).
  ENDCASE.

ENDMODULE.                 " CREATE_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&  Include           ZPP025_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------
* Tabelas e workareas p/ reimpressão de etiquetas
*----------------------------------------------------------------------
DATA: t_magna  TYPE TABLE OF magna_ty,
      t_op     TYPE TABLE OF op_type,
      t_ficosa TYPE TABLE OF ficosa_ty,
      t_etiq   TYPE TABLE OF zst_etiq_lg.

DATA: s_magna  TYPE magna_ty,
      s_op     TYPE op_type,
      s_ficosa TYPE ficosa_ty,
      s_etiq   TYPE zst_etiq_lg.

DATA v_psmng   TYPE co_psmng.
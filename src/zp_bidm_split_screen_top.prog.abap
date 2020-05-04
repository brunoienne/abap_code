*&---------------------------------------------------------------------*
*& Include ZTESTE_BIDM_054TOP                                PoolMóds.        ZTESTE_BIDM_054
*&
*&---------------------------------------------------------------------*

PROGRAM  zteste_bidm_054.

*---------------------------------------------------------------------*
* Globais
*---------------------------------------------------------------------*
DATA: v_usrname     TYPE ad_namtext,
      v_cont_pai    TYPE REF TO cl_gui_custom_container,
      v_easy_split  TYPE REF TO cl_gui_easy_splitter_container,
      v_cont_up     TYPE REF TO cl_gui_container,
      v_cont_down   TYPE REF TO cl_gui_container,
      v_alv_up    TYPE REF TO cl_gui_alv_grid,
      v_alv_down    TYPE REF TO cl_gui_alv_grid,
      w_layout_path TYPE lvc_s_layo.

DATA: t_rows        TYPE lvc_t_row,
      t_hdrows      TYPE lvc_t_row.

DATA: t_motorista   TYPE TABLE OF ztb_motorista_20,
      t_veiculos    TYPE TABLE OF ztb_veiculos_20.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME.
PARAMETERS: p_matnr TYPE mara-matnr.
SELECTION-SCREEN: END OF BLOCK b01.

START-OF-SELECTION.
  PERFORM zf_select_data.
  CALL SCREEN 9001.

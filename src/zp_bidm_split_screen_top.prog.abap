PROGRAM  zsd027.

*---------------------------------------------------------------------*
* Tabelas
*---------------------------------------------------------------------*
TABLES: j_1bnfdoc, vbrk, s031, vbak.

*--------------------------------------------------------------------
* Tipos
*--------------------------------------------------------------------
TYPES:
BEGIN OF aux_type,
      matnr TYPE vbrp-matnr,
      fkimg TYPE vbrp-fkimg,
      netwr TYPE vbrp-netwr,
      spmon TYPE spmon,
END OF aux_type,

BEGIN OF tot_type,
  netwr TYPE vbrp-netwr,
  spmon TYPE spmon,
END OF tot_type,

BEGIN OF relat_ty,
  matnr TYPE matnr,
  bismt TYPE bismt,
END OF relat_ty,

BEGIN OF mara_ty,
  matnr TYPE matnr,
  bismt TYPE bismt,
END OF mara_ty,

BEGIN OF vbrk_ty,
  vbeln TYPE vbrk-vbeln,
  fkdat TYPE vbrk-fkdat,
END OF vbrk_ty,

BEGIN OF vbrp_ty,
  vbeln TYPE vbrp-vbeln,
  posnr TYPE vbrp-posnr,
  matnr TYPE vbrp-matnr,
  fkimg TYPE vbrp-fkimg,
  netwr TYPE vbrp-netwr,
  aubel TYPE vbrp-aubel,
END OF vbrp_ty,

BEGIN OF vbak_ty,
  vbeln TYPE vbak-vbeln,
  vkgrp TYPE vbak-vkgrp,
END OF vbak_ty.


*--------------------------------------------------------------------
* Field-symbol
*--------------------------------------------------------------------
FIELD-SYMBOLS:
      <dyn_table>         TYPE STANDARD TABLE,
      <dyn_wa>            TYPE any,
      " Field symbols Total
      <dyn_table_t>       TYPE STANDARD TABLE,
      <dyn_wa_t>          TYPE any.

*---------------------------------------------------------------------*
* Globais
*---------------------------------------------------------------------*
DATA: v_cont_pai            TYPE REF TO cl_gui_custom_container,
      v_easy_split          TYPE REF TO cl_gui_easy_splitter_container,
      v_cont_up             TYPE REF TO cl_gui_container,
      v_cont_down           TYPE REF TO cl_gui_container,
      v_alv_orig            TYPE REF TO cl_gui_alv_grid,
      v_alv_path            TYPE REF TO cl_gui_alv_grid,
      w_layout_path         TYPE lvc_s_layo,

      gv_data_inicial       TYPE            oiu_pr_wcdvlh-eff_from_dt,
      gv_data_final         TYPE            oiu_pr_wcdvlh-eff_from_dt,
      gv_data_processamento TYPE            oiu_pr_wcdvlh-eff_from_dt,
      gv_cliente            TYPE            string,
      gv_months             TYPE            i.


*---------------------------------------------------------------------*
* Tabelas internas
*---------------------------------------------------------------------*
DATA: gt_relat             TYPE TABLE OF   relat_ty,
      gt_aux               TYPE TABLE OF   aux_type,
      gt_tot               TYPE TABLE OF   tot_type,
      gt_vbrk              TYPE TABLE OF   vbrk_ty,
      gt_vbrp              TYPE TABLE OF   vbrp_ty,
      gt_mara              TYPE TABLE OF   mara_ty,
      gt_vbak              TYPE TABLE OF   vbak_ty,
      gt_t247              TYPE TABLE OF   t247,
      gt_fieldcat          TYPE lvc_t_fcat,
      gt_fieldcat_tot      TYPE lvc_t_fcat.

*--------------------------------------------------------------------
* Estruturas
*--------------------------------------------------------------------
DATA: gs_aux            TYPE            aux_type,
      gs_tot            TYPE            tot_type,
      gs_vbrk           TYPE            vbrk_ty,
      gs_vbrp           TYPE            vbrp_ty,
      gs_relat          TYPE            relat_ty,
      gs_mara           TYPE            mara_ty,
      gs_vbak           TYPE            vbak_ty,
      gs_t247           TYPE            t247.

*--------------------------------------------------------------------
* Ranges
*--------------------------------------------------------------------
DATA: lr_periodo        TYPE            range_t_dats,
      ls_periodo        TYPE            range_s_dats.

*---------------------------------------------------------------------*
* Objetos
*---------------------------------------------------------------------*
DATA: go_struct         TYPE REF TO     cl_abap_structdescr.


*----------------------------------------------------------------------
* Tela de Seleção
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME.

SELECT-OPTIONS: s_spmon  FOR s031-spmon OBLIGATORY.
PARAMETERS: p_parid  TYPE  j_1bnfdoc-parid OBLIGATORY.
SELECT-OPTIONS: s_bukrs FOR vbrk-bukrs,
                s_vkgrp FOR vbak-vkgrp.

SELECTION-SCREEN: END OF BLOCK b01.

START-OF-SELECTION.
  PERFORM zf_select_data.
  PERFORM zf_processing_data.
  IF gt_aux[] IS NOT INITIAL.
    CALL SCREEN 9001.
  ELSE.
    MESSAGE s000(cl) WITH text-e00 DISPLAY LIKE 'E'.
  ENDIF.

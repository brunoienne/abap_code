*&---------------------------------------------------------------------*
*&  Include           ZWM_ETIQUETAS_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS: slis.

TABLES: zcontrol_etq, ztb_etiq_seyon, ztbpp_tenpao,ztb_etiq_magna.

TYPES: BEGIN OF final_type,
  sel,
  aufnr  LIKE zcontrol_etq-aufnr,
  vol_de  LIKE zcontrol_etq-vol_de,
  vol_ate  LIKE zcontrol_etq-vol_ate,
  matnr  LIKE zcontrol_etq-matnr,
  qtde  LIKE zcontrol_etq-qtde,
  END OF final_type.

*>>> BIdM
TYPES: BEGIN OF magna_type.
        INCLUDE STRUCTURE ztb_etiq_magna.
TYPES: sel,
END OF magna_type.


TYPES:
  BEGIN OF auspx_type,
    matnr                 TYPE            ausp-objek,
  END OF auspx_type.

DATA: gv_matnr TYPE matnr,
      gv_psmng TYPE co_psmng,
      gv_bstmi TYPE bstmi,
      gv_tot_imp TYPE i.

DATA: gv_formname TYPE tdsfname,
      gv_funcao TYPE rs38l_fnam.

DATA: ti_etiq TYPE TABLE OF zst_etiq_lg.

DATA: wa_etiq TYPE zst_etiq_lg.

DATA: gv_atinn TYPE atinn,
      gv_atwrt TYPE atwrt,
      v_qtimpetq     TYPE p DECIMALS 4,
      v_qtimpetq_fn  TYPE p DECIMALS 4.

DATA: i_alv TYPE STANDARD TABLE OF zcontrol_etq.
DATA: alv_container TYPE REF TO cl_gui_docking_container.
DATA: alv_grid TYPE REF TO cl_gui_alv_grid.
DATA: t_layout       TYPE slis_layout_alv.
DATA: t_fieldcat     TYPE slis_t_fieldcat_alv.

DATA: ls_grid_settings  TYPE            lvc_s_glay.

DATA: variant TYPE disvariant.
DATA: repid TYPE sy-repid.
DATA: t_print        TYPE slis_print_alv.
DATA: t_final        TYPE STANDARD TABLE OF final_type.
DATA: wa_final       TYPE final_type.

DATA: it_ztb_magna     TYPE TABLE OF magna_type,
      it_ztb_magna_aux TYPE TABLE OF magna_type,
      wa_etiq_magna    TYPE          magna_type.


DATA: gs_parametros       TYPE            zst_etiquetas.  " <<< FEdM - 04.01.2017


*---------------------------------------------------------------------*
* Objetos
*---------------------------------------------------------------------*
DATA: go_tenpao           TYPE REF TO     zcl_tenpao.

*---------------------------------------------------------------------*
* Tela de seleção
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK sel1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (12) text-002 FOR FIELD p_aufnr.
PARAMETERS: p_aufnr LIKE afpo-aufnr.
*    SELECTION-SCREEN COMMENT 30(18) text-003 FOR FIELD p_etiq.
*    PARAMETERS: p_etiq TYPE n LENGTH 3.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK sel1.

SELECTION-SCREEN BEGIN OF BLOCK sel2 WITH FRAME TITLE text-004.
* >>> FEdM - 09.06.2015
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_orp RADIOBUTTON GROUP opt DEFAULT 'X' USER-COMMAND p01.
SELECTION-SCREEN COMMENT (30) text-p01 FOR FIELD p_orp.
PARAMETERS: p_flib AS CHECKBOX MODIF ID fli.
SELECTION-SCREEN COMMENT (30) text-m01 FOR FIELD p_flib.
PARAMETERS p_cct  NO-DISPLAY DEFAULT 'X'."AS CHECKBOX MODIF ID p02.
*SELECTION-SCREEN COMMENT (30) text-p02 FOR FIELD p_cct.
SELECTION-SCREEN END OF LINE.
* <<< FEdM - 09.06.2015
PARAMETERS: p_pin RADIOBUTTON GROUP opt.          " <<< BIdM - 30.01.2020
PARAMETERS: p_epson       RADIOBUTTON GROUP opt.  " <<< FEdM - 04.01.2017
PARAMETERS: p_lg  RADIOBUTTON GROUP opt,          " LG
            p_fic RADIOBUTTON GROUP opt,          " FICOSA    " <<< FEdM - 17.01.2017
            p_koito RADIOBUTTON GROUP opt,        " KOITO     " <<< FEdM - 09.04.2018
            p_stanl RADIOBUTTON GROUP opt,        " STANLEY   " <<< BIdM - 24.10.2019
            p_yaz RADIOBUTTON GROUP opt,          " YAZAKI
            p_seoyon  RADIOBUTTON GROUP opt,      " SEOYON
            p_magna   RADIOBUTTON GROUP opt,      " MAGNA
            p_tenpao  RADIOBUTTON GROUP opt,      " TENPAO
            p_etiqad  RADIOBUTTON GROUP opt.      " AD        " <<< BIdM - 12.06.2019
SELECTION-SCREEN END OF BLOCK sel2.

SELECTION-SCREEN BEGIN OF BLOCK sel3 WITH FRAME TITLE text-008.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_reimp AS CHECKBOX USER-COMMAND rep.
SELECTION-SCREEN COMMENT 4(11) text-007 FOR FIELD p_reimp.
SELECTION-SCREEN COMMENT 30(18) text-003 FOR FIELD p_etiq.
PARAMETERS: p_etiq TYPE n LENGTH 3.
SELECTION-SCREEN COMMENT 60(22) text-010 FOR FIELD p_etiq.
PARAMETERS: p_qtlg TYPE n LENGTH 3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK sel3.

* >>> FEdM - 16.03.2017
SELECTION-SCREEN BEGIN OF BLOCK sel4 WITH FRAME TITLE text-009.
PARAMETERS:     p_datum   TYPE            sydatum
                                            DEFAULT sy-datum
                                            OBLIGATORY
                                            MODIF ID sey.
SELECT-OPTIONS: s_seq     FOR             ztb_etiq_seyon-seq
                                            MODIF ID sey.
SELECTION-SCREEN END OF BLOCK sel4.
* <<< FEdM - 16.03.2017

* >>> FEdM - 16.03.2017
SELECTION-SCREEN BEGIN OF BLOCK sel5 WITH FRAME TITLE text-011.
PARAMETERS:     p_datten  TYPE            sydatum
                                            DEFAULT sy-datum
                                            OBLIGATORY
                                            MODIF ID ten.
SELECT-OPTIONS: s_seqnr   FOR             ztbpp_tenpao-seqnr
                                            MODIF ID ten.
SELECTION-SCREEN END OF BLOCK sel5.

SELECTION-SCREEN BEGIN OF BLOCK sel6 WITH FRAME TITLE text-012.
PARAMETERS: p_print       TYPE            rsposel-device
                                            OBLIGATORY
                                            MODIF ID zeb.
PARAMETERS: p_darkn       TYPE            zde_zebra_darkness
                                            MODIF ID zeb.
*>>> BIdM 24.01.2020
PARAMETERS: p_prts        TYPE            n LENGTH 2
                                            MODIF ID zeb,
            p_slws        TYPE            n LENGTH 2
                                            MODIF ID zeb.
*<<< BIdM 24.01.2020

SELECTION-SCREEN END OF BLOCK sel6.
* <<< FEdM - 16.03.2017

*>>> BIdM - 16.01.2020
SELECTION-SCREEN BEGIN OF BLOCK sel7 WITH FRAME TITLE text-013.
SELECT-OPTIONS: s_rod         FOR           ztb_etiq_magna-rodada
                                            NO-EXTENSION
                                            MODIF ID mag.
SELECT-OPTIONS: s_dat         FOR            sy-datum
                                            DEFAULT sy-datum
                                            NO INTERVALS
                                            NO-EXTENSION
                                            MODIF ID mag.
SELECT-OPTIONS: s_seqm    FOR               ztb_etiq_magna-seq
                                            NO-EXTENSION
                                            MODIF ID mag.
SELECTION-SCREEN END OF BLOCK sel7.
*<<< BIdM - 16.01.2020

*>>> BIdM - 17.01.2020
SELECTION-SCREEN BEGIN OF BLOCK sel8 WITH FRAME TITLE text-014.
PARAMETERS: p_rodada TYPE n LENGTH 3 OBLIGATORY MODIF ID mgn,
            p_qtop   TYPE n LENGTH 3 OBLIGATORY MODIF ID mgn.
SELECTION-SCREEN END OF BLOCK sel8.
*<<< BIdM - 17.01.2020

DEFINE busca_dados.

  break psilva.

  select single psmng matnr
      into (gv_psmng, gv_matnr)
      from afpo
     where aufnr = &1.

  select single bstmi
    into gv_bstmi
    from marc
   where matnr = gv_matnr.
END-OF-DEFINITION.

DEFINE preenche_tb.
  data: ll_count type i.
  do &2 times.
    ll_count = ll_count + 1.
    wa_etiq-aufnr = &1.
    wa_etiq-nro_etiq = ll_count.
    wa_etiq-qtd_tot_etiq = &2.
    wa_etiq-matnr = &3.
    wa_etiq-psmng = &4.
    wa_etiq-atwrt = &5.
    if ll_count = &2.
      wa_etiq-qtd_etfn = &6.
    endif.
    append wa_etiq to ti_etiq.

  enddo.
END-OF-DEFINITION.

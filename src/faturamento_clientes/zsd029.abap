*&-----------------------------------------------------------------&*
*                             ClipTech                              *
*&-----------------------------------------------------------------&*
* Programa ..: ZSD029                                               *
* Transação..: ZSD029                                               *
* Módulo.....: SD                                                   *
* Funcional..:                                                      *
*&---------------------------------------------------------------- *&
* Descrição .: Relatório de faturamento de clientes                 *
*&-----------------------------------------------------------------*&
* Objetivo ..:                                                      *
*&-----------------------------------------------------------------*&
*                           HISTÓRICO                               *
*&-----------------------------------------------------------------*&
* DATA       | REQUEST    | RESPONSÁVEL | DESCRIÇÃO                 *
* 26/08/2020 | ER1K934116 | BIMORAIS    |                           *
*&-----------------------------------------------------------------&*

REPORT  zsd029.

*--------------------------------------------------------------------
* Tabelas Banco de Dados
*--------------------------------------------------------------------
TABLES: vbrk, s031, vbak.

*--------------------------------------------------------------------
* Constantes
*--------------------------------------------------------------------
CONSTANTS:
      c_a(01)             TYPE            c             VALUE 'A',
      c_mark              TYPE            c             VALUE 'X'.

*--------------------------------------------------------------------
* Tipos
*--------------------------------------------------------------------
TYPES:

 BEGIN OF doc_ty,
   docnum TYPE j_1bnfdoc-docnum,
   nftype TYPE j_1bnfdoc-nftype,
   direct TYPE j_1bnfdoc-direct,
   bukrs  TYPE j_1bnfdoc-bukrs,
   docdat TYPE j_1bnfdoc-docdat,
   parid  TYPE j_1bnfdoc-parid,
   cancel TYPE j_1bnfdoc-cancel,
END OF doc_ty,

BEGIN OF lin_ty,
  docnum TYPE j_1bnflin-docnum,
  refitm TYPE j_1bnflin-refitm,
  itmnum TYPE j_1bnflin-itmnum,
  refkey TYPE vbrp-vbeln,
  matkl  TYPE j_1bnflin-matkl,
  nfnett TYPE j_1bnflin-nfnett,
  netwr  TYPE j_1bnflin-netwr,
  cfop   TYPE j_1bnflin-cfop,
  netfre TYPE j_1bnflin-netfre,
END OF lin_ty,

BEGIN OF vbrk_ty,
  vbeln TYPE vbrk-vbeln,
  kunag TYPE vbrk-kunag,
END OF vbrk_ty,

BEGIN OF vbrp_ty,
  vbeln TYPE vbrp-vbeln,
  posnr TYPE vbrp-posnr,
END OF vbrp_ty,

BEGIN OF kna1_ty,
  kunnr TYPE kna1-kunnr,
  name1 TYPE kna1-name1,
END OF kna1_ty,

BEGIN OF aux_ty,
  kunag TYPE vbrk-kunag,
  netwr TYPE vbrp-netwr,
  spmon TYPE spmon,
END OF aux_ty,

BEGIN OF relat_type,
  kunnr TYPE kna1-kunnr,
  name1 TYPE kna1-name1,
END OF relat_type.

*--------------------------------------------------------------------
* Tabelas Internas
*--------------------------------------------------------------------
DATA: gt_aux               TYPE TABLE OF   aux_ty,
      gt_vbrk              TYPE TABLE OF   vbrk_ty,
      gt_vbrp              TYPE TABLE OF   vbrp_ty,
      gt_kna1              TYPE TABLE OF   kna1_ty,
      gt_t247              TYPE TABLE OF   t247,
      gt_lin               TYPE TABLE OF   lin_ty,
      gt_doc               TYPE TABLE OF   doc_ty,
      gt_baj               TYPE TABLE OF   j_1baj,
      gt_stx               TYPE TABLE OF   j_1bnfstx,
      gt_fieldcat          TYPE            lvc_t_fcat.


*--------------------------------------------------------------------
* Workareas
*--------------------------------------------------------------------
DATA: gs_aux               TYPE            aux_ty,
      gs_vbrk              TYPE            vbrk_ty,
      gs_vbrp              TYPE            vbrp_ty,
      gs_kna1              TYPE            kna1_ty,
      gs_lin               TYPE            lin_ty,
      gs_doc               TYPE            doc_ty,
      gs_baj               TYPE            j_1baj,
      gs_stx               TYPE            j_1bnfstx,
      gs_t247              TYPE            t247.

*--------------------------------------------------------------------
* Variaveis
*--------------------------------------------------------------------
DATA: v_variant             TYPE           disvariant,
      gv_data_inicial       TYPE           oiu_pr_wcdvlh-eff_from_dt,
      gv_data_final         TYPE           oiu_pr_wcdvlh-eff_from_dt,
      gv_data_processamento TYPE           oiu_pr_wcdvlh-eff_from_dt,
      gv_periodo_inicial    TYPE           string,
      gv_periodo_final      TYPE           string,
      gv_months             TYPE           i.

*--------------------------------------------------------------------
* Objetos
*--------------------------------------------------------------------
DATA: go_struct         TYPE REF TO     cl_abap_structdescr.

*--------------------------------------------------------------------
* Ranges
*--------------------------------------------------------------------
DATA: lr_periodo        TYPE            range_t_dats,
      ls_periodo        TYPE            range_s_dats.

DATA: gr_cfop           TYPE RANGE OF   j_1bnflin-cfop.

*--------------------------------------------------------------------
* Field-symbol
*--------------------------------------------------------------------
FIELD-SYMBOLS:
     <dyn_table>        TYPE STANDARD TABLE,
     <dyn_wa>           TYPE any.

*----------------------------------------------------------------------
* Tela de Seleção
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.

SELECT-OPTIONS: s_spmon  FOR s031-spmon OBLIGATORY.
SELECT-OPTIONS: s_bukrs FOR vbrk-bukrs OBLIGATORY,
                s_vkgrp FOR vbak-vkgrp.

SELECTION-SCREEN END OF BLOCK b01.
PARAMETERS: p_varian      TYPE            slis_vari.

*----------------------------------------------------------------------
* Eventos de tela
*----------------------------------------------------------------------
AT SELECTION-SCREEN.
  IF p_varian IS INITIAL.
    CLEAR v_variant.
  ELSE.
    v_variant-report  = sy-repid.
    v_variant-variant = p_varian.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varian.
  PERFORM zf_variant_f4 USING p_varian.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM zf_select_data.
  PERFORM zf_processing_data.
  PERFORM zf_print_data.


*&-----------------------------------------------------------------*&
*   Declaração de FORMs
*&-----------------------------------------------------------------*&

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
FORM zf_select_data .

  DATA: lt_doc   TYPE TABLE OF doc_ty,
        lt_kna1  TYPE TABLE OF kna1_ty.

  PERFORM zf_determina_periodo.

  SELECT docnum
         nftype
         direct
         bukrs
         docdat
         parid
         cancel
    FROM j_1bnfdoc INTO TABLE gt_doc
    WHERE docdat IN lr_periodo
      AND bukrs  IN s_bukrs.

  IF sy-subrc EQ 0.
    DELETE gt_doc WHERE direct EQ '1' AND NOT
                      ( nftype EQ 'ZD' OR nftype EQ 'ZE' ).
  ENDIF.

  CHECK gt_doc[] IS NOT INITIAL.

  SELECT docnum
         refitm
         itmnum
         refkey
         matkl
         nfnett
         netwr
         cfop
         netfre
    FROM j_1bnflin INTO TABLE gt_lin
    FOR ALL ENTRIES IN gt_doc
    WHERE docnum EQ gt_doc-docnum.

  PERFORM zf_select_parameters
       TABLES gr_cfop.

  DELETE gt_lin WHERE cfop NOT IN gr_cfop.

  CHECK gt_lin[] IS NOT INITIAL.

  SELECT vbeln posnr
    FROM vbrp INTO TABLE gt_vbrp
    FOR ALL ENTRIES IN gt_lin
    WHERE vbeln EQ gt_lin-refkey
      AND posnr EQ gt_lin-refitm
      AND vkgrp IN s_vkgrp.

  IF gt_vbrp[] IS NOT INITIAL.

    SELECT vbeln kunag
      FROM vbrk INTO TABLE gt_vbrk
      FOR ALL ENTRIES IN gt_vbrp
      WHERE vbeln EQ gt_vbrp-vbeln.

    SELECT kunnr name1
      FROM kna1 INTO TABLE gt_kna1
      FOR ALL ENTRIES IN gt_vbrk
      WHERE kunnr EQ gt_vbrk-kunag.

  ENDIF.

  SELECT * INTO TABLE gt_stx FROM j_1bnfstx
    FOR ALL ENTRIES IN gt_lin
    WHERE docnum EQ gt_lin-docnum
      AND itmnum EQ gt_lin-itmnum.

  IF sy-subrc EQ 0.

    SELECT * FROM j_1baj INTO TABLE gt_baj
      FOR ALL ENTRIES IN gt_stx
      WHERE taxtyp EQ gt_stx-taxtyp.

  ENDIF.

  lt_doc[]  = gt_doc[].
  SORT lt_doc BY parid.
  DELETE ADJACENT DUPLICATES FROM lt_doc COMPARING parid.

  SELECT kunnr name1
    INTO TABLE lt_kna1
    FROM kna1
    FOR ALL ENTRIES IN lt_doc
    WHERE kunnr EQ lt_doc-parid.

  APPEND LINES OF lt_kna1 TO gt_kna1.
  DELETE ADJACENT DUPLICATES FROM gt_kna1 COMPARING kunnr.


ENDFORM.                    " ZF_SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSING_DATA
*&---------------------------------------------------------------------*
FORM zf_processing_data .

  DATA: lt_aux   TYPE TABLE OF aux_ty,
        ls_aux   TYPE aux_ty,
        lv_mes   TYPE c LENGTH 2,
        lv_icst  TYPE j_1bnflin-netwr,
        lv_ipi   TYPE j_1bnflin-netwr,
        lv_calc  TYPE p DECIMALS 2.

  FIELD-SYMBOLS: <fs_month>   TYPE any,
                 <fs_total>   TYPE any,
                 <fs_kunnr>   TYPE any,
                 <fs_name1>   TYPE any.

  SORT: gt_doc  BY bukrs docnum,
        gt_lin  BY docnum,
        gt_vbrp BY vbeln posnr,
        gt_stx  BY docnum itmnum taxtyp,
        gt_baj  BY taxtyp,
        gt_kna1 BY kunnr,
        gt_t247 BY mnr.

  DELETE gt_lin WHERE matkl EQ 'MT'.
  DELETE gt_lin WHERE cfop EQ '5902AA' OR cfop EQ '6902AA'.


  PERFORM zf_tabela_dinamica.

  LOOP AT gt_doc INTO gs_doc.

    CHECK gs_doc-nftype NE 'ZF'.
    CHECK gs_doc-cancel EQ space.

    READ TABLE gt_lin TRANSPORTING NO FIELDS
      WITH KEY docnum = gs_doc-docnum BINARY SEARCH.

    IF sy-subrc EQ 0.

      LOOP AT gt_lin INTO gs_lin FROM sy-tabix
                                 WHERE docnum EQ gs_doc-docnum.

        CLEAR gs_vbrp.
        READ TABLE gt_vbrp INTO gs_vbrp
          WITH KEY vbeln = gs_lin-refkey(10)
                   posnr = gs_lin-refitm BINARY SEARCH.

        CHECK sy-subrc IS INITIAL.

        READ TABLE gt_stx TRANSPORTING NO FIELDS
          WITH KEY docnum = gs_lin-docnum
                   itmnum = gs_lin-itmnum BINARY SEARCH.

        IF sy-subrc EQ 0.

          LOOP AT gt_stx INTO gs_stx FROM sy-tabix
                                     WHERE docnum EQ gs_lin-docnum
                                       AND itmnum EQ gs_lin-itmnum.
            CLEAR gs_baj.
            READ TABLE gt_baj INTO gs_baj
              WITH KEY taxtyp = gs_stx-taxtyp BINARY SEARCH.

            IF sy-subrc EQ 0.

              CASE gs_baj-taxgrp.
                WHEN 'ICST'.
                  lv_icst = lv_icst + gs_stx-taxval.
                WHEN 'IPI'.
                  lv_ipi = lv_ipi + gs_stx-taxval.
              ENDCASE.

            ENDIF.

          ENDLOOP.

          lv_calc = gs_lin-netwr + lv_icst + lv_ipi.

          IF gs_doc-nftype = 'YV'.
            lv_calc = gs_lin-nfnett.
          ENDIF.

          IF gs_doc-nftype EQ 'ZD' OR
             gs_doc-nftype EQ 'ZE' OR
             gs_doc-nftype EQ 'ZV'.
            lv_calc = lv_calc * -1.
          ENDIF.

          lv_calc = lv_calc + gs_lin-netfre.

        ENDIF.

        gs_aux-kunag = gs_doc-parid.
        gs_aux-netwr = lv_calc.
        gs_aux-spmon = gs_doc-docdat+0(6).
        COLLECT gs_aux INTO gt_aux. CLEAR gs_aux.

        CLEAR: lv_calc, lv_ipi, lv_icst.

      ENDLOOP.

    ENDIF.

  ENDLOOP.

  CHECK gt_aux[] IS NOT INITIAL.

  lt_aux[] = gt_aux[].
  SORT:gt_aux BY kunag spmon,
       lt_aux BY kunag.
  DELETE ADJACENT DUPLICATES FROM lt_aux COMPARING kunag.

  LOOP AT lt_aux INTO ls_aux.

    gv_data_processamento = gv_data_inicial.

    DO gv_months TIMES.

      READ TABLE gt_aux INTO gs_aux
        WITH KEY kunag = ls_aux-kunag
                 spmon = gv_data_processamento+0(6) BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        lv_mes = sy-index.

        UNASSIGN <fs_month>.
        ASSIGN COMPONENT lv_mes OF STRUCTURE <dyn_wa> TO <fs_month>.
        IF <fs_month> IS ASSIGNED.
          <fs_month> = gs_aux-netwr.
        ENDIF.

        UNASSIGN <fs_total>.
        ASSIGN COMPONENT 'TOT' OF STRUCTURE <dyn_wa> TO <fs_total>.
        IF <fs_total> IS ASSIGNED.
          <fs_total> = <fs_total> + gs_aux-netwr.
        ENDIF.

      ENDIF.

      CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
        EXPORTING
          months  = 1
          olddate = gv_data_processamento
        IMPORTING
          newdate = gv_data_processamento.

    ENDDO.

    CLEAR gs_kna1.
    READ TABLE gt_kna1 INTO gs_kna1
      WITH KEY kunnr = ls_aux-kunag BINARY SEARCH.

    UNASSIGN <fs_kunnr>.
    ASSIGN COMPONENT 'KUNNR' OF STRUCTURE <dyn_wa> TO <fs_kunnr>.
    IF <fs_kunnr> IS ASSIGNED.
      <fs_kunnr> = ls_aux-kunag.
    ENDIF.

    UNASSIGN <fs_name1>.
    ASSIGN COMPONENT 'NAME1' OF STRUCTURE <dyn_wa> TO <fs_name1>.
    <fs_name1> = gs_kna1-name1.

    APPEND <dyn_wa> TO <dyn_table>. CLEAR <dyn_wa>.

  ENDLOOP.


ENDFORM.                    " ZF_PROCESSING_DATA

*&---------------------------------------------------------------------*
*&      Form  ZF_PRINT_DATA
*&---------------------------------------------------------------------*
FORM zf_print_data .

  DATA: lt_fcat           TYPE            slis_t_fieldcat_alv,
        lw_lout           TYPE            slis_layout_alv.

  IF <dyn_table>[] IS NOT INITIAL.

    lw_lout-zebra              = c_mark.
    lw_lout-expand_all         = c_mark.
    lw_lout-colwidth_optimize  = c_mark.

    PERFORM zf_preenche_fcat TABLES lt_fcat.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_callback_top_of_page = 'TOP_OF_PAGE'
        is_layout              = lw_lout
        it_fieldcat            = lt_fcat
        i_save                 = c_a
        is_variant             = v_variant
      TABLES
        t_outtab               = <dyn_table>
      EXCEPTIONS
        program_error          = 1
        OTHERS                 = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ELSE.

    MESSAGE s000(cl) WITH text-e00 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM.                    " ZF_PRINT_DATA

*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHE_FCAT
*&---------------------------------------------------------------------*
*       Preenchimento da tabela Field catalog
*----------------------------------------------------------------------*
FORM zf_preenche_fcat
  TABLES   pt_fieldcat.

  DATA: ls_fcat   TYPE      lvc_s_fcat,
        ls_slis   TYPE      slis_fieldcat_alv.


  LOOP AT gt_fieldcat INTO ls_fcat.

    ls_slis-fieldname      = ls_fcat-fieldname.
    ls_slis-tabname        = '<DYN_TABLE>'.
    ls_slis-datatype       = ls_fcat-datatype.
    ls_slis-inttype        = ls_fcat-inttype.
    ls_slis-intlen         = ls_fcat-intlen.
    ls_slis-ref_fieldname  = ls_fcat-ref_field.
    ls_slis-ref_tabname    = ls_fcat-ref_table.
    ls_slis-seltext_l      = ls_fcat-scrtext_l.
    ls_slis-seltext_m      = ls_fcat-scrtext_m.
    ls_slis-seltext_s      = ls_fcat-scrtext_s.
    ls_slis-decfloat_style = ls_fcat-decfloat_style.
    ls_slis-ddic_outputlen = ls_fcat-dd_outlen.
    ls_slis-key            = ls_fcat-key.
    ls_slis-do_sum         = ls_fcat-do_sum.

    APPEND ls_slis TO pt_fieldcat. CLEAR ls_slis.

  ENDLOOP.


ENDFORM.                    " ZF_PREENCHE_FCAT

*&---------------------------------------------------------------------*
*&      Form  ZF_PFSTATUS
*&---------------------------------------------------------------------*
*FORM zf_pfstatus
*  USING pw_extab          TYPE            kkblo_t_extab.
*
**  SET PF-STATUS 'ZGUI_ALV' EXCLUDING pw_extab.
*
*ENDFORM.                    "ZF_PFSTATUS

*&---------------------------------------------------------------------*
*&      Form  ZF_UCOMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM zf_ucommand
*  USING lc_ucomm          TYPE            sy-ucomm
*        ls_selfield       TYPE            slis_selfield.
*
*
**  CASE lc_ucomm.
**    WHEN .
**  ENDCASE.
*
*ENDFORM.                    "ZF_UCOMMAND

*&---------------------------------------------------------------------*
*&      Form  ZF_VARIANT_F4
*&---------------------------------------------------------------------*
FORM zf_variant_f4  USING    p_p_var.

  CONSTANTS:
    c_variant_save        VALUE 'U'.

  DATA: wa_variant        TYPE            disvariant,
        vl_exit.

  MOVE: sy-repid TO wa_variant-report.

  CLEAR v_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = wa_variant
      i_save        = c_variant_save
    IMPORTING
      e_exit        = vl_exit
      es_variant    = v_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc IS INITIAL AND vl_exit IS INITIAL.
    p_varian   = v_variant-variant.
  ELSE.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "z_variant_f4

*&---------------------------------------------------------------------*
*&      Form  ZF_TABELA_DINAMICA
*&---------------------------------------------------------------------*
FORM zf_tabela_dinamica .

  DATA: new_table         TYPE REF TO     data,
        new_line          TYPE REF TO     data.

  DATA: ls_relat TYPE relat_type,
        ls_comp  TYPE abap_compdescr,
        ls_fcat  TYPE lvc_s_fcat,
        lv_mes   TYPE c LENGTH 2.

  go_struct ?= cl_abap_datadescr=>describe_by_data( ls_relat ).

  LOOP AT go_struct->components INTO ls_comp.

    ls_fcat-fieldname = ls_comp-name.
    ls_fcat-inttype   = ls_comp-type_kind.
    ls_fcat-intlen    = ls_comp-length.

    CASE ls_comp-name.
      WHEN 'KUNNR'.
        ls_fcat-ref_field   = ls_fcat-fieldname.
        ls_fcat-ref_table   = 'KNA1'.
        ls_fcat-key         = 'X'.
      WHEN 'NAME1'.
        ls_fcat-ref_field   = ls_fcat-fieldname.
        ls_fcat-ref_table   = 'KNA1'.
        ls_fcat-key         = 'X'.

    ENDCASE.

    APPEND ls_fcat TO gt_fieldcat. CLEAR ls_fcat.

  ENDLOOP.

  gv_data_processamento = gv_data_inicial.

  DO gv_months TIMES.

    lv_mes = sy-index.

    CLEAR gs_t247.
    READ TABLE gt_t247 INTO gs_t247
      WITH KEY  mnr = gv_data_processamento+4(2)  BINARY SEARCH.

    ls_fcat-fieldname      = lv_mes.
    ls_fcat-datatype       = 'CURR'.
    ls_fcat-inttype        = 'P'.
    ls_fcat-intlen         = 15.
    ls_fcat-decfloat_style = 2.
    ls_fcat-do_sum         = 'X'.
    ls_fcat-scrtext_l      = gs_t247-ktx && '/' && gv_data_processamento+0(4).
    ls_fcat-scrtext_m      = gs_t247-ktx && '/' && gv_data_processamento+0(4).
    ls_fcat-scrtext_s      = gs_t247-ktx && '/' && gv_data_processamento+0(4).
    APPEND ls_fcat TO gt_fieldcat. CLEAR ls_fcat.

    IF sy-index EQ 1.
      gv_periodo_inicial = gs_t247-ltx && '/' && gv_data_processamento+0(4).
    ENDIF.

    IF gv_months GT 1 AND sy-index EQ gv_months.
      gv_periodo_final = gs_t247-ltx && '/' && gv_data_processamento+0(4).
    ENDIF.

    CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
      EXPORTING
        months  = 1
        olddate = gv_data_processamento
      IMPORTING
        newdate = gv_data_processamento.

  ENDDO.

  ls_fcat-fieldname      = 'TOT'.
  ls_fcat-datatype       = 'CURR'.
  ls_fcat-inttype        = 'P'.
  ls_fcat-intlen         = 15.
  ls_fcat-decfloat_style = 2.
  ls_fcat-do_sum         = 'X'.
  ls_fcat-scrtext_l      = text-001.
  ls_fcat-scrtext_m      = text-001.
  ls_fcat-scrtext_s      = text-001.
  APPEND ls_fcat TO gt_fieldcat.  CLEAR ls_fcat.

* Create dynamic internal table and assign to FS
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = gt_fieldcat
    IMPORTING
      ep_table        = new_table.

  ASSIGN new_table->* TO <dyn_table>.

* Create dynamic work area and assign to FS
  CREATE DATA new_line LIKE LINE OF <dyn_table>.
  ASSIGN new_line->* TO <dyn_wa>.

ENDFORM.                    " ZF_TABELA_DINAMICA

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page .

  DATA: wa_header TYPE slis_listheader,
        it_header TYPE slis_t_listheader.

  " Header
  wa_header-typ  = 'H'.
  wa_header-info =  text-002.
  APPEND wa_header TO it_header. CLEAR wa_header.

  " Key and value pairs
  wa_header-typ  = 'S'.
  wa_header-key  = text-003.
  IF gv_months GT 1.
    CONCATENATE gv_periodo_inicial 'até' gv_periodo_final INTO wa_header-info SEPARATED BY space.
  ELSE.
    wa_header-info = gv_periodo_inicial.
  ENDIF.
  APPEND wa_header TO it_header. CLEAR wa_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_header.

ENDFORM.                    " TOP_OF_PAGE


*&---------------------------------------------------------------------*
*&      Form  ZF_DETERMINA_PERIODO
*&---------------------------------------------------------------------*
FORM zf_determina_periodo .

  CONCATENATE s_spmon-low '01' INTO gv_data_inicial.

  ls_periodo-low    = gv_data_inicial.
  ls_periodo-sign   = 'I'.
  ls_periodo-option = 'BT'.

  IF s_spmon-high IS NOT INITIAL.

    CONCATENATE s_spmon-high '01' INTO gv_data_final.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = gv_data_final
      IMPORTING
        last_day_of_month = gv_data_final.

    ls_periodo-high  = gv_data_final.

    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from = gv_data_inicial
        i_date_to   = gv_data_final
      IMPORTING
        e_months    = gv_months.

  ELSE.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = gv_data_inicial
      IMPORTING
        last_day_of_month = gv_data_final.

    ls_periodo-high  = gv_data_final.
    gv_months = 1.

  ENDIF.

  APPEND ls_periodo TO lr_periodo. CLEAR ls_periodo.

  CALL FUNCTION 'MONTH_NAMES_GET'
    TABLES
      month_names           = gt_t247
    EXCEPTIONS
      month_names_not_found = 1
      OTHERS                = 2.

ENDFORM.                    " ZF_DETERMINA_PERIODO

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_PARAMETERS
*&---------------------------------------------------------------------*
FORM zf_select_parameters  TABLES   pr_cfop.

 DATA:
        lo_stvarv         TYPE REF TO     zcl_stvarv.

  DATA: lr_cfop           TYPE RANGE OF   j_1bnflin-cfop.

  REFRESH pr_cfop.

  CREATE OBJECT lo_stvarv.

  lo_stvarv->get_values(
    EXPORTING
      i_name   = 'ZSD_CFOP_INDUSTR'
    IMPORTING
      e_values = lr_cfop ).

  pr_cfop[] = lr_cfop[].

ENDFORM.                    " ZF_SELECT_PARAMETERS
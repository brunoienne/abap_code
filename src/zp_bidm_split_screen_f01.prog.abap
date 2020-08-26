*&---------------------------------------------------------------------*
*&  Include           ZTESTE_BIDM_054F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ZF_CRIAOO_CONT
*&---------------------------------------------------------------------*
FORM zf_criaoo_cont .

  DATA: l_repid TYPE syrepid,
        l_dynnr TYPE sydynnr.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.

  CREATE OBJECT v_cont_pai " associa custom container com custom control da tela
    EXPORTING
      dynnr                       = l_dynnr
      repid                       = l_repid
      container_name              = 'CC_9001' " nome custom control
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

*** Split do container (Up e Down)
  CREATE OBJECT v_easy_split
    EXPORTING
      parent            = v_cont_pai
      sash_position     = 70        " posicao da divisao dos containers
      link_dynnr        = l_dynnr
      link_repid        = l_repid
      name              = 'CC_MYESCONT'
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*** Armazena no container o split
  v_cont_up   = v_easy_split->top_left_container.
  v_cont_down = v_easy_split->bottom_right_container.

ENDFORM.                    " ZF_CRIAOO_CONT

*&---------------------------------------------------------------------*
*&      Form  ZF_CRIAOO_ALV
*&---------------------------------------------------------------------*
FORM zf_criaoo_alv .

  DATA:  lw_layout  TYPE lvc_s_layo,
         lt_toolbar TYPE ui_functions,
         ls_toolbar TYPE ui_func.

*** Criar o ALV TOPO
  CREATE OBJECT v_alv_orig
    EXPORTING
      i_parent          = v_cont_up " container de cima
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*** Criar o ALV RODAPE
  CREATE OBJECT v_alv_path
    EXPORTING
      i_parent          = v_cont_down " container de baixo
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  lw_layout-zebra      = abap_true.
  lw_layout-cwidth_opt = abap_true.
  CONCATENATE text-001 gv_cliente INTO lw_layout-grid_title SEPARATED BY space.

  ls_toolbar = cl_gui_alv_grid=>mc_fc_sort_asc.
  APPEND ls_toolbar TO lt_toolbar.CLEAR ls_toolbar.
  ls_toolbar = cl_gui_alv_grid=>mc_fc_sort_dsc.
  APPEND ls_toolbar TO lt_toolbar.CLEAR ls_toolbar.

  CALL METHOD v_alv_orig->set_table_for_first_display " preenche alv acima
    EXPORTING
      is_layout                     = lw_layout
      it_toolbar_excluding          = lt_toolbar[]
    CHANGING
      it_outtab                     = <dyn_table>   " tabela com dados
      it_fieldcatalog               = gt_fieldcat[] " tabela catalogos
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  w_layout_path            = lw_layout.
  w_layout_path-zebra      = abap_false.
  w_layout_path-grid_title = text-002.

  CALL METHOD v_alv_path->set_table_for_first_display " preenche alv baixo
    EXPORTING
      is_layout                     = w_layout_path
      it_toolbar_excluding          = lt_toolbar[]
    CHANGING
      it_outtab                     = <dyn_table_t>
      it_fieldcatalog               = gt_fieldcat_tot
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  " Registra eventos pro Container TOPO
*  CREATE OBJECT v_event_orig.
*  SET HANDLER   v_event_orig->handle_toolbar FOR v_alv_orig.
  CALL METHOD v_alv_orig->set_toolbar_interactive.

  " Registra eventos pro Container RODAPE
  CREATE OBJECT v_event_path.
*  SET HANDLER:
*   v_event_path->handle_user_command FOR v_alv_path,
*   v_event_path->handle_toolbar      FOR v_alv_path,
*   v_event_path->handle_menu_button  FOR v_alv_path.
  CALL METHOD v_alv_path->set_toolbar_interactive.


ENDFORM.                    " ZF_CRIAOO_ALV

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM zf_select_data .

  DATA: lt_mara TYPE TABLE OF vbrp_ty,
        lt_vbak TYPE TABLE OF vbrp_ty.

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

  SELECT vbeln fkdat FROM vbrk INTO TABLE gt_vbrk
    WHERE bukrs IN s_bukrs
      AND fkdat IN lr_periodo
      AND kunag EQ p_parid
      AND waerk EQ 'BRL'.

  CHECK gt_vbrk[] IS NOT INITIAL.

  SELECT vbeln posnr matnr fkimg netwr aubel
    FROM vbrp INTO TABLE gt_vbrp
    FOR ALL ENTRIES IN gt_vbrk
    WHERE vbeln EQ gt_vbrk-vbeln.

  CHECK gt_vbrp[] IS NOT INITIAL.

  lt_mara[] = gt_vbrp[].
  SORT lt_mara BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_mara COMPARING matnr.

  SELECT matnr bismt FROM mara INTO TABLE gt_mara
    FOR ALL ENTRIES IN lt_mara
    WHERE matnr EQ lt_mara-matnr.

  lt_vbak[] = gt_vbrp[].
  SORT lt_vbak BY aubel.
  DELETE ADJACENT DUPLICATES FROM lt_vbak COMPARING aubel.

  SELECT vbeln vkgrp FROM vbak INTO TABLE gt_vbak
    FOR ALL ENTRIES IN lt_vbak
    WHERE vbeln EQ lt_vbak-aubel
      AND vkgrp IN s_vkgrp.

  SELECT SINGLE name1 INTO gv_cliente
    FROM kna1 WHERE kunnr EQ p_parid.


ENDFORM.                    " ZF_SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSING_DATA
*&---------------------------------------------------------------------*

FORM zf_processing_data .

  DATA: lt_aux TYPE TABLE OF aux_type,
        ls_aux TYPE aux_type,
        lv_mes TYPE c LENGTH 2.

  FIELD-SYMBOLS: <fs_matnr>   TYPE any,
                 <fs_bismt>   TYPE any,
                 <fs_month>   TYPE any,
                 <fs_total>   TYPE any,
                 <fs_netwr>   TYPE any.

  SORT: gt_vbrk BY vbeln,
        gt_mara BY matnr,
        gt_vbak BY vbeln,
        gt_t247 BY mnr.

  PERFORM zf_tabela_dinamica.

  LOOP AT gt_vbrp INTO gs_vbrp.

    CLEAR gs_vbrk.
    READ TABLE gt_vbrk INTO gs_vbrk
      WITH KEY vbeln = gs_vbrp-vbeln BINARY SEARCH.

    CLEAR gs_vbak.
    READ TABLE gt_vbak INTO gs_vbak
      WITH KEY vbeln = gs_vbrp-aubel BINARY SEARCH.

    CHECK sy-subrc IS INITIAL.

    gs_aux-matnr = gs_vbrp-matnr.
    gs_aux-fkimg = gs_vbrp-fkimg.
    gs_aux-netwr = gs_vbrp-netwr.
    gs_aux-spmon = gs_vbrk-fkdat+0(6).
    COLLECT gs_aux INTO gt_aux.

  ENDLOOP.

  CHECK gt_aux[] IS NOT INITIAL.

  LOOP AT gt_aux INTO gs_aux.
    gs_tot-netwr = gs_aux-netwr.
    gs_tot-spmon = gs_aux-spmon.
    COLLECT gs_tot INTO gt_tot. CLEAR gs_tot. " Tabela com total por período
  ENDLOOP.

  lt_aux[] = gt_aux[].
  SORT: gt_aux BY matnr spmon,
        lt_aux BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_aux COMPARING matnr. " Tabela sem MATNR repetidos

  " Tabela principal
  LOOP AT lt_aux INTO ls_aux.

    gv_data_processamento = gv_data_inicial.

    DO gv_months TIMES.

      READ TABLE gt_aux INTO gs_aux
        WITH KEY matnr = ls_aux-matnr
                 spmon = gv_data_processamento+0(6) BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        lv_mes = sy-index.

        UNASSIGN <fs_month>.
        ASSIGN COMPONENT lv_mes OF STRUCTURE <dyn_wa> TO <fs_month>.
        IF <fs_month> IS ASSIGNED.
          <fs_month> = gs_aux-fkimg.
        ENDIF.

        UNASSIGN <fs_total>.
        ASSIGN COMPONENT 'TOT' OF STRUCTURE <dyn_wa> TO <fs_total>.
        IF <fs_total> IS ASSIGNED.
          <fs_total> = <fs_total> + gs_aux-fkimg.
        ENDIF.

      ENDIF.

      CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
        EXPORTING
          months  = 1
          olddate = gv_data_processamento
        IMPORTING
          newdate = gv_data_processamento.

    ENDDO.

    CLEAR gs_mara.
    READ TABLE gt_mara INTO gs_mara
      WITH KEY matnr = ls_aux-matnr BINARY SEARCH.

    UNASSIGN <fs_matnr>.
    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <dyn_wa> TO <fs_matnr>.
    <fs_matnr> = ls_aux-matnr.

    UNASSIGN <fs_bismt>.
    ASSIGN COMPONENT 'BISMT' OF STRUCTURE <dyn_wa> TO <fs_bismt>.
    <fs_bismt> = gs_mara-bismt.

    APPEND <dyn_wa> TO <dyn_table>. CLEAR <dyn_wa>.

  ENDLOOP.


  " Tabela Total
  SORT: gt_tot BY spmon.
  gv_data_processamento = gv_data_inicial.

  DO gv_months TIMES.

    CLEAR gs_tot.
    READ TABLE gt_tot INTO gs_tot
      WITH KEY spmon = gv_data_processamento+0(6) BINARY SEARCH.

    IF sy-subrc IS INITIAL.

      lv_mes = sy-index.

      UNASSIGN <fs_netwr>.
      ASSIGN COMPONENT lv_mes OF STRUCTURE <dyn_wa_t> TO <fs_netwr>.
      <fs_netwr> = gs_tot-netwr.

      UNASSIGN <fs_total>.
      ASSIGN COMPONENT 'TOT' OF STRUCTURE <dyn_wa_t> TO <fs_total>.
      <fs_total> = <fs_total> + gs_tot-netwr.

    ENDIF.

    CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
      EXPORTING
        months  = 1
        olddate = gv_data_processamento
      IMPORTING
        newdate = gv_data_processamento.

  ENDDO.

  APPEND <dyn_wa_t> TO <dyn_table_t>. CLEAR <dyn_wa_t>.

ENDFORM.                    " ZF_PROCESSING_DATA

*&---------------------------------------------------------------------*
*&      Form  ZF_TABELA_DINAMICA
*&---------------------------------------------------------------------*
FORM zf_tabela_dinamica .

  REFRESH: gt_fieldcat, gt_fieldcat_tot.

  DATA: new_table         TYPE REF TO     data,
        new_line          TYPE REF TO     data,
" Total
        new_table_t       TYPE REF TO     data,
        new_line_t        TYPE REF TO     data.



  DATA:  ls_relat TYPE relat_ty,
         ls_comp  TYPE abap_compdescr,
         ls_fcat  TYPE lvc_s_fcat,
         lv_mes   TYPE c LENGTH 2.

  go_struct ?= cl_abap_datadescr=>describe_by_data( ls_relat ).

  LOOP AT go_struct->components INTO ls_comp.

    ls_fcat-fieldname = ls_comp-name.
    ls_fcat-inttype   = ls_comp-type_kind.
    ls_fcat-intlen    = ls_comp-length.

    CASE ls_comp-name.
      WHEN 'MATNR'.
        ls_fcat-ref_field       = ls_fcat-fieldname.
        ls_fcat-ref_table       = 'MARA'.
        ls_fcat-key             = 'X'.
      WHEN 'BISMT'.
        ls_fcat-ref_field       = ls_fcat-fieldname.
        ls_fcat-ref_table       = 'MARA'.
        ls_fcat-key             = 'X'.

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
    ls_fcat-datatype       = 'QUAN'.
    ls_fcat-inttype        = 'P'.
    ls_fcat-intlen         = 15.
    ls_fcat-scrtext_l      = gs_t247-ktx && '/' && gv_data_processamento+0(4).
    ls_fcat-scrtext_m      = gs_t247-ktx && '/' && gv_data_processamento+0(4).
    ls_fcat-scrtext_s      = gs_t247-ktx && '/' && gv_data_processamento+0(4).
    APPEND ls_fcat TO gt_fieldcat. CLEAR ls_fcat-datatype.

    ls_fcat-datatype = 'CURR'.
    ls_fcat-decfloat_style = 2.
    APPEND ls_fcat TO gt_fieldcat_tot. CLEAR ls_fcat.

    CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
      EXPORTING
        months  = 1
        olddate = gv_data_processamento
      IMPORTING
        newdate = gv_data_processamento.

  ENDDO.

  ls_fcat-fieldname      = 'TOT'.
  ls_fcat-datatype       = 'QUAN'.
  ls_fcat-inttype        = 'P'.
  ls_fcat-intlen         = 15.
  ls_fcat-scrtext_l      = text-003.
  ls_fcat-scrtext_m      = text-003.
  ls_fcat-scrtext_s      = text-003.
  APPEND ls_fcat TO gt_fieldcat.  CLEAR ls_fcat-datatype.

  ls_fcat-datatype = 'CURR'.
  ls_fcat-decfloat_style = 2.
  APPEND ls_fcat TO gt_fieldcat_tot. CLEAR ls_fcat.


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

  " Tabela Total
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = gt_fieldcat_tot
    IMPORTING
      ep_table        = new_table_t.

  ASSIGN new_table_t->* TO <dyn_table_t>.

  CREATE DATA new_line_t LIKE LINE OF <dyn_table_t>.
  ASSIGN new_line_t->* TO <dyn_wa_t>.

ENDFORM.                    " ZF_TABELA_DINAMICA

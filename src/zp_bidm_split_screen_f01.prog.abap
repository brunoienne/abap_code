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
      sash_position     = 100         " posicao da divisao dos containers
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

  DATA:  lt_fcat    TYPE lvc_t_fcat,
         l_structn  TYPE dd02l-tabname,
         lw_layout  TYPE lvc_s_layo,
         lt_toolbar TYPE ui_functions,
         lw_variant TYPE disvariant,
         l_save     TYPE c VALUE 'A'.

  DATA:
    ls_fcat               TYPE            lvc_s_fcat.

  FIELD-SYMBOLS: <f_fcat> LIKE LINE OF lt_fcat.

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
  lw_layout-grid_title = 'Lista de motoristas'.

  lw_variant-report = sy-repid.
  lw_variant-handle = '0001'. " ID do alv

  ls_fcat-col_pos   = 1.
  ls_fcat-fieldname = 'ID'.
  ls_fcat-reptext   = 'Id motorista'.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  ls_fcat-col_pos   = 2.
  ls_fcat-fieldname = 'NOME'.
  ls_fcat-reptext   = 'Nome'.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  ls_fcat-col_pos   = 3.
  ls_fcat-fieldname = 'CNH'.
  ls_fcat-reptext   = 'Cnh'.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  ls_fcat-col_pos   = 4.
  ls_fcat-fieldname = 'Categoria'.
  ls_fcat-reptext   = 'Categoria'.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  CALL METHOD v_alv_orig->set_table_for_first_display " preenche alv acima
    EXPORTING
      is_layout                     = lw_layout
      it_toolbar_excluding          = lt_toolbar
    CHANGING
      it_outtab                     = t_motorista " tabela com dados
      it_fieldcatalog               = lt_fcat     " tabela catalogos
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR lt_fcat[].

  ls_fcat-col_pos   = 1.
  ls_fcat-fieldname = 'PLACA'.
  ls_fcat-reptext   = 'Placa'.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  ls_fcat-col_pos   = 2.
  ls_fcat-fieldname = 'MARCA'.
  ls_fcat-reptext   = 'Marca'.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  ls_fcat-col_pos   = 3.
  ls_fcat-fieldname = 'MODELO'.
  ls_fcat-reptext   = 'Modelo'.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  ls_fcat-col_pos   = 4.
  ls_fcat-fieldname = 'ANO'.
  ls_fcat-reptext   = 'Ano'.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  ls_fcat-col_pos = 5.
  ls_fcat-fieldname = 'CATEGORIA'.
  ls_fcat-reptext   = 'Categoria'.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  ls_fcat-col_pos   = 6.
  ls_fcat-fieldname = 'BLOQUEADO'.
  ls_fcat-reptext   = 'Bloqueado'.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  lw_variant-handle = '0002'.

  w_layout_path            = lw_layout.
  w_layout_path-ctab_fname = 'LVC_COLOR'.
  w_layout_path-zebra      = abap_false.

  CALL METHOD v_alv_path->set_table_for_first_display " preenche alv baixo
    EXPORTING
      is_layout                     = w_layout_path
      it_toolbar_excluding          = lt_toolbar
    CHANGING
      it_outtab                     = t_veiculos
      it_fieldcatalog               = lt_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD v_alv_path->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  " Registra eventos pro Container TOPO
  CREATE OBJECT v_event_orig.
  SET HANDLER v_event_orig->double_click FOR v_alv_orig.

  CALL METHOD v_alv_orig->set_toolbar_interactive.

  " Registra eventos pro Container RODAPE
  CREATE OBJECT v_event_path.
  SET HANDLER:
   v_event_path->handle_user_command FOR v_alv_path,
   v_event_path->handle_toolbar      FOR v_alv_path,
   v_event_path->handle_menu_button  FOR v_alv_path.

  CALL METHOD v_alv_path->set_toolbar_interactive.


ENDFORM.                    " ZF_CRIAOO_ALV

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM zf_select_data .

  SELECT * FROM ztb_motorista_20 INTO TABLE t_motorista.
  SELECT * FROM ztb_veiculos_20 INTO TABLE t_veiculos.

  SELECT SINGLE b~name_text
    FROM usr21 AS a INNER JOIN adrp AS b
      ON b~persnumber = a~persnumber
    INTO v_usrname
   WHERE a~bname EQ sy-uname.

ENDFORM.                    " ZF_SELECT_DATA

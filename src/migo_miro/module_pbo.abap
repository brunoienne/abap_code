*&---------------------------------------------------------------------*
*&      Module  EXIBE_FATURAMENTO  OUTPUT
*&---------------------------------------------------------------------*
MODULE exibe_faturamento OUTPUT.

  SORT t_fatura_aux BY dvenc.

  IF o_alv_fat IS BOUND.
    o_alv_fat->refresh_table_display( ).
  ELSE.

    CREATE OBJECT o_container_fat
      EXPORTING
        container_name = 'CC_FATURA'.

    CREATE OBJECT o_alv_fat
      EXPORTING
        i_parent = o_container_fat.

    REFRESH t_fcat_fat.
    PERFORM zf_preenche_fieldcat USING:
          'ITEM'  'T_FATURA_AUX' 'Número'    '' '' '' '' '' '' CHANGING t_fcat_fat,
          'DVENC' 'T_FATURA_AUX' 'Dt.Vencto' '' '' '' '' '' '' CHANGING t_fcat_fat,
          'VDUP'  'T_FATURA_AUX' 'Valor'     '' '' '' '' '' '' CHANGING t_fcat_fat.

    w_layout-zebra      =  'X'.
    w_layout-cwidth_opt =  'X'.
    w_layout-no_toolbar =  'X'.

    o_alv_fat->set_ready_for_input( 1 ).

    CALL METHOD o_alv_fat->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
      CHANGING
        it_outtab                     = t_fatura_aux
        it_fieldcatalog               = t_fcat_fat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

ENDMODULE.                 " EXIBE_FATURAMENTO  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIBE_ITENS  OUTPUT
*&---------------------------------------------------------------------*
MODULE exibe_itens OUTPUT.

  DATA: lt_toolbar TYPE         ui_functions      ,
        lt_f4      TYPE         lvc_t_f4          ,
        ls_f4      TYPE         lvc_s_f4          .

  DATA: ls_cellrow TYPE         lvc_s_styl        ,
        ls_exclude TYPE         ui_func           ,
        ls_variant TYPE         disvariant        .

  DATA: lo_handler TYPE REF TO  lcl_event_handler .

  IF o_alv_itm IS BOUND.
    PERFORM zf_refresh_alv.
  ELSE.

    ls_variant-report       = sy-repid.
    ls_variant-username     = sy-uname.

    CREATE OBJECT o_container_itm
      EXPORTING
        container_name = 'CC_ITEM'.

    CREATE OBJECT o_alv_itm
      EXPORTING
        i_parent = o_container_itm.

    " monta fieldcat
    PERFORM zf_monta_itm_fcat.

    " preenche tabela de estilos
    PERFORM zf_preenche_celltab.

    w_layout-zebra      =  'X'.
    w_layout-cwidth_opt =  'X'.
    w_layout-stylefname = 'CELLTAB'.

    " evento de edição com ENTER
    o_alv_itm->register_edit_event(
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter
    ).

    " exlude toolbar
    PERFORM zf_fill_toolbar TABLES lt_toolbar.

    CALL METHOD o_alv_itm->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        it_toolbar_excluding          = lt_toolbar
        is_variant                    = ls_variant
        i_save                        = 'A'
      CHANGING
        it_outtab                     = t_relati_aux
        it_fieldcatalog               = t_fcat_itm
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

*    ls_f4-fieldname = 'MWSKZ'.
*    ls_f4-register  = 'X'.
*    INSERT ls_f4 INTO TABLE lt_f4. CLEAR ls_f4.

    ls_f4-fieldname = 'EBELN'.
    ls_f4-register  = 'X'.
    INSERT ls_f4 INTO TABLE lt_f4. CLEAR ls_f4.

    ls_f4-fieldname = 'LGORT'.
    ls_f4-register  = 'X'.
    INSERT ls_f4 INTO TABLE lt_f4. CLEAR ls_f4.

    o_alv_itm->register_f4_for_fields( it_f4 = lt_f4 ).

    CREATE OBJECT lo_handler.
    SET HANDLER lo_handler->handle_on_f4 FOR o_alv_itm.         " search help (F4)
    SET HANDLER lo_handler->handle_data_finished FOR o_alv_itm. " trata mudanças alv

  ENDIF.

ENDMODULE.                 " EXIBE_ITENS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  VERIFICA_VLR_UNIT  OUTPUT
*&---------------------------------------------------------------------*
*& Verificar Vlr.Unit Zerado Chamado [56116]
*&---------------------------------------------------------------------*
MODULE verifica_vlr_unit OUTPUT.

  DATA: ls_aux  TYPE ty_relati,
        lv_msg  TYPE string.

  LOOP AT t_relati_aux INTO ls_aux.

    IF ls_aux-vuncom IS INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_aux-matnr
        IMPORTING
          output = ls_aux-matnr.

      lv_msg = |O material { ls_aux-matnr }, no XML do fornecedor, está com Vlr Unitário ZERO. O sistema não permite a EM p/estoque e, valorizará com base no Vlr de Custo atual que está no DM do mesmo. Você deseja continuar?|.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Atenção'
          text_question         = lv_msg
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          icon_button_1         = '@01@'
          icon_button_2         = '@02@'
          display_cancel_button = space
          popup_type            = 'ICON_MESSAGE_INFORMATION'.

    ENDIF.

  ENDLOOP.

ENDMODULE.                 " VERIFICA_VLR_UNIT  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TRATA_CAMPOS_9200  OUTPUT
*&---------------------------------------------------------------------*
MODULE trata_campos_9201 OUTPUT.
  IF o_alv_itm IS BOUND.
    PERFORM zf_preenche_celltab.
  ENDIF.
ENDMODULE.                 " TRATA_CAMPOS_9200  OUTPUT
*&-----------------------------------------------------------------&*
*                             ClipTech                              *
*&-----------------------------------------------------------------&*
* Programa ..: ZGBXX001                                             *
* Transação..:                                                      *
* Módulo.....: XX                                                   *
* Especif....: XX001 - Modelo de ALV                                *
* Funcional..:                                                      *
*&---------------------------------------------------------------- *&
* Descrição .: Modelo de ALV            *
*&-----------------------------------------------------------------*&
* Objetivo ..:                                                      *
*&-----------------------------------------------------------------*&
*                           HISTÓRICO                               *
*&-----------------------------------------------------------------*&
* DATA       | REQUEST    | RESPONSÁVEL | DESCRIÇÃO                 *
*            |            |             |                           *
*&-----------------------------------------------------------------&*


*----------------------------------------------------------------------
* Tela de Seleção
*----------------------------------------------------------------------
PARAMETERS: p_varian      TYPE            slis_vari.


*----------------------------------------------------------------------
* Classe relatório Definição
*----------------------------------------------------------------------
CLASS lcl_relat DEFINITION.
  PUBLIC SECTION.
    METHODS:
      select_data,

      processing_data,

      output_data,

      f4_variant
        RETURNING
          value(variant)  TYPE            disvariant-variant,

      set_variant
        IMPORTING
          variant         TYPE            disvariant-variant
          report          TYPE            disvariant-report OPTIONAL,

      set_key
        IMPORTING
          lo_column_list  TYPE REF TO     cl_salv_columns_table.

  PRIVATE SECTION.
    METHODS:
      check_relat
        RETURNING
          value(subrc)    TYPE            sy-subrc,

      create_alv
        RETURNING
          value(alv)      TYPE REF TO     cl_salv_table,

      on_link_click
        FOR EVENT link_click OF cl_salv_events_table
          IMPORTING
            row
            column,

      on_double_click
        FOR EVENT double_click OF cl_salv_events_table
          IMPORTING
            row
            column,

      on_user_command
        FOR EVENT added_function OF cl_salv_events.


*--------------------------------------------------------------------
* Objetos
*--------------------------------------------------------------------
    DATA:
      co_salv             TYPE REF TO     cl_salv_table,
      co_sel              TYPE REF TO     cl_salv_selections.


*--------------------------------------------------------------------
* Tabelas Internas
*--------------------------------------------------------------------
    DATA:
      " Tabelas internas
      ct_relat              TYPE TABLE OF   relat_type.

*--------------------------------------------------------------------
* Work areas
*--------------------------------------------------------------------
    DATA:
      " Work areas
      cs_relat            TYPE            relat_type,
      cs_variant          TYPE            disvariant.


ENDCLASS.                    "lcl_relat DEFINITION


*----------------------------------------------------------------------
* Classe relatório Implementação
*----------------------------------------------------------------------
CLASS lcl_relat IMPLEMENTATION.

  " Busca variante de exibição do ALV
  METHOD f4_variant.

    CONSTANTS:
      lc_variant_save     TYPE            c  VALUE 'U'.

    DATA: ls_variant      TYPE            disvariant,
          vl_exit.

    ls_variant-report = sy-repid.

    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
      EXPORTING
        is_variant    = ls_variant
        i_save        = lc_variant_save
      IMPORTING
        e_exit        = vl_exit
        es_variant    = ls_variant
      EXCEPTIONS
        not_found     = 1
        program_error = 2
        OTHERS        = 3.

    IF sy-subrc IS INITIAL AND vl_exit IS INITIAL.
      variant   = ls_variant-variant.
      set_variant( variant = variant report = sy-repid ).
    ELSE.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "f4_variant


  " Seta variante de exibição do alv
  METHOD set_variant.

    cs_variant-variant = variant.

    IF report IS NOT INITIAL.
      cs_variant-report = report.
    ELSE.
      cs_variant-report = sy-repid.
    ENDIF.

  ENDMETHOD.                    "set_variant


  " Seleção de dados
  METHOD select_data.

  ENDMETHOD.                    "select_data


  " Tratamento dos dados selecionados
  METHOD processing_data.

    cs_relat-campo1  = 1.
    APPEND cs_relat TO ct_relat.

    cs_relat-campo1  = 2.
    APPEND cs_relat TO ct_relat.

  ENDMETHOD.                    "processing_data


  " Cria objeto ALV
  METHOD create_alv.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false
          IMPORTING
            r_salv_table = alv
          CHANGING
            t_table      = ct_relat[] ).
      CATCH cx_salv_msg .
    ENDTRY.

  ENDMETHOD.                    "create_alv


  " Saída ALV
  METHOD output_data.

    DATA: lo_lout         TYPE REF TO     cl_salv_layout,
          lo_column_list  TYPE REF TO     cl_salv_columns_table,
          lo_column       TYPE REF TO     cl_salv_column,
          lo_sum          TYPE REF TO     cl_salv_aggregations,
          lo_sort         TYPE REF TO     cl_salv_sorts,
          lo_display      TYPE REF TO     cl_salv_display_settings,
          lo_events       TYPE REF TO     cl_salv_events_table.

    DATA: ls_key          TYPE            salv_s_layout_key.

    CHECK check_relat( ) IS INITIAL.

    " Cria objeto ALV
    co_salv = create_alv( ).

    co_sel = co_salv->get_selections( ).
    co_sel->set_selection_mode( if_salv_c_selection_mode=>multiple ).

    " Eventos
    lo_events = co_salv->get_event( ).
    SET HANDLER on_link_click FOR lo_events.    " HOTSPOT click
    SET HANDLER on_double_click FOR lo_events.  " Double click row
    SET HANDLER on_user_command FOR lo_events.  " User command

    " Set pf-status
    co_salv->set_screen_status(
        report        = sy-repid
        pfstatus      = 'ZGUI_ALV'
        set_functions = 2
    ).

    " Habilita caracteristicas do ALV
    lo_display = co_salv->get_display_settings( ).
    lo_display->set_striped_pattern( abap_true ). " Zebra

    " Set layout
    lo_lout = co_salv->get_layout( ).
    lo_lout->set_initial_layout( cs_variant-variant ).
    " Habilita salvar layout de ALV
    ls_key-report = sy-repid.
    lo_lout->set_key( ls_key ).
    lo_lout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    TRY.

        " Colunas ALV
        lo_column_list  = co_salv->get_columns( ).
        lo_column_list->set_optimize( abap_true ).
        lo_column_list->set_key_fixation( abap_true ).

        " Set nome colunas
        lo_column   = lo_column_list->get_column( 'CAMPO1' ).
        lo_column->set_long_text( 'Campo1' ).

        lo_column   = lo_column_list->get_column( 'CAMPO2' ).
        lo_column->set_long_text( 'Campo2' ).

      CATCH cx_salv_not_found.
    ENDTRY.

    set_key( lo_column_list ).

    " Colunas com somatório
    lo_sum      = co_salv->get_aggregations( ).

*    TRY.
*      lo_sum->add_aggregation(
*        EXPORTING
*          columnname  =     " ALV Control: Field Name of Internal Table Field
**        aggregation = IF_SALV_C_AGGREGATION=>TOTAL    " Aggregation
*        receiving
**        value       =     " ALV: Aggregations
*      ).
**      CATCH cx_salv_data_error.    " ALV: General Error Class (Checked During Syntax Check)
**      CATCH cx_salv_not_found.    " ALV: General Error Class (Checked During Syntax Check)
**      CATCH cx_salv_existing.    " ALV: General Error Class (Checked During Syntax Check)
*    ENDTRY.

    " Exibe ALV
    co_salv->display( ).


  ENDMETHOD.                    "output_data


  " Set coluna chave
  METHOD set_key.

    DATA: lo_columns      TYPE REF TO     cl_salv_column_table.

    DATA: lt_columns      TYPE            salv_t_column_ref.

    DATA: ls_column       TYPE            salv_s_column_ref.

    lt_columns = lo_column_list->get( ).

    LOOP AT lt_columns INTO ls_column.

      CHECK ls_column-columnname  EQ  'BUKRS' OR
            ls_column-columnname  EQ  'BUPLA'.

      TRY .
          lo_columns  ?= lo_column_list->get_column( ls_column-columnname ).
        CATCH cx_salv_not_found.  " ALV: General Error Class (Checked During Syntax Check)
      ENDTRY.

      TRY .
        lo_columns->set_key( abap_true ).
      ENDTRY.

    ENDLOOP.


  ENDMETHOD.                    "set_key


  METHOD check_relat.
    CHECK ct_relat[] IS INITIAL.
    subrc = 4.
    MESSAGE s000(cl) WITH text-e00 DISPLAY LIKE 'E'.
  ENDMETHOD.                    "CHECK_RELAT

  METHOD on_link_click.
  ENDMETHOD.                    "on_link_click

  METHOD on_double_click.
  ENDMETHOD.                    "on_double_click

  METHOD on_user_command.

    DATA:
      lt_selected_rows    TYPE            salv_t_row .

    DATA:
      ls_row              LIKE LINE OF    lt_selected_rows.


    lt_selected_rows[] = co_sel->get_selected_rows( ).


  ENDMETHOD.                    "on_user_command

ENDCLASS.                    "lcl_relat IMPLEMENTATION

*--------------------------------------------------------------------
* Objetos
*--------------------------------------------------------------------
DATA: go_relat            TYPE REF TO     lcl_relat.

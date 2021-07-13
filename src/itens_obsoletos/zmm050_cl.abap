*&-----------------------------------------------------------------&*
*                             ClipTech                              *
*&-----------------------------------------------------------------&*
* Programa ..: ZMM050                                               *
* Transação..: ZMM050                                               *
* Módulo.....: MM                                                   *
* Especif....: XX001 - Modelo de ALV                                *
* Funcional..:                                                      *
*&---------------------------------------------------------------- *&
* Descrição .: Relatório de Itens Obsoletos                         *
*&-----------------------------------------------------------------*&
* Objetivo ..:                                                      *
*&-----------------------------------------------------------------*&
*                           HISTÓRICO                               *
*&-----------------------------------------------------------------*&
* DATA       | REQUEST    | RESPONSÁVEL | DESCRIÇÃO                 *
* 05/07/2021 | ER1K937612 | BIMORAIS    | Itens Obsoletos           *
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

      send_email,

      f4_variant
        RETURNING
          VALUE(variant) TYPE            disvariant-variant,

      set_variant
        IMPORTING
          variant TYPE            disvariant-variant
          report  TYPE            disvariant-report OPTIONAL,

      set_key
        IMPORTING
          lo_column_list TYPE REF TO     cl_salv_columns_table.

  PRIVATE SECTION.
    METHODS:
      set_colors,

      set_xls_header
        IMPORTING
          i_guid      TYPE        zexcel_cell_style
        CHANGING
          c_worksheet TYPE REF TO zcl_excel_worksheet,

      set_xls_footer
        IMPORTING
          i_guid      TYPE        zexcel_cell_style
          i_row       TYPE        i
        CHANGING
          c_worksheet TYPE REF TO zcl_excel_worksheet,

      set_xls_autosize
        CHANGING
          c_worksheet  TYPE REF TO zcl_excel_worksheet
          c_column_dim TYPE REF TO zcl_excel_worksheet_columndime,

      check_relat
        RETURNING
          VALUE(subrc) TYPE            sy-subrc,

      create_alv
        RETURNING
          VALUE(alv) TYPE REF TO     cl_salv_table.


*--------------------------------------------------------------------
* Objetos
*--------------------------------------------------------------------
    DATA:
      co_salv TYPE REF TO     cl_salv_table,
      co_sel  TYPE REF TO     cl_salv_selections.


*--------------------------------------------------------------------
* Tabelas Internas
*--------------------------------------------------------------------
    DATA:
      ct_aux   TYPE TABLE OF   aux_type,
      ct_s032  TYPE TABLE OF   s032_type,
      ct_makt  TYPE TABLE OF   makt,
      ct_relat TYPE TABLE OF   relat_type.

*--------------------------------------------------------------------
* Work areas
*--------------------------------------------------------------------
    DATA:
      cs_red     TYPE            lvc_s_scol,
      cs_green   TYPE            lvc_s_scol,
      cs_s032    TYPE            s032_type,
      cs_makt    TYPE            makt,
      cs_relat   TYPE            relat_type,
      cs_aux     TYPE            aux_type,
      cs_variant TYPE            disvariant.

*--------------------------------------------------------------------
* Variáveis
*--------------------------------------------------------------------
    DATA:
      cv_mov TYPE            d,
      cv_tot TYPE            wbwbest.


ENDCLASS.                    "lcl_relat DEFINITION


*----------------------------------------------------------------------
* Classe relatório Implementação
*----------------------------------------------------------------------
CLASS lcl_relat IMPLEMENTATION.

  " Busca variante de exibição do ALV
  METHOD f4_variant.

    CONSTANTS:
      lc_variant_save     TYPE            c  VALUE 'U'.

    DATA: ls_variant TYPE            disvariant,
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

    DATA: lt_aux TYPE TABLE OF aux_type.

    set_colors( ).

    IF p_yea IS NOT INITIAL.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = sy-datum
          days      = 0
          months    = 0
          signum    = '-'
          years     = 1     " subtrai 1 ano
        IMPORTING
          calc_date = cv_mov.

    ELSE.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = sy-datum
          days      = 0
          months    = 3
          signum    = '-'
          years     = 0     " subtrai 3 meses
        IMPORTING
          calc_date = cv_mov.

    ENDIF.

    " exclui Cl.Avl  = ZROH
    APPEND VALUE #( low = 'ZROH' option = 'EQ' sign = 'E' ) TO s_bklas.

    " tabela auxiliar com totais
    SELECT werks
           matnr
      SUM( mbwbest )
      SUM( wbwbest )
    FROM s032 INTO TABLE ct_aux
    WHERE   werks   EQ p_werks
     AND  ( mbwbest GT 0   OR
            wbwbest NE 0 ) AND
            bklas   IN s_bklas GROUP BY werks matnr.

    DELETE ct_aux WHERE: mbwbest LE 0.

    CHECK ct_aux[] IS NOT INITIAL.

    lt_aux[] = ct_aux[].
    SORT lt_aux BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_aux COMPARING matnr.

    SELECT werks
           matnr
           bklas
           basme
           hwaer
           letztzug
           letztabg
           letztver
           letztbew
      FROM s032 INTO TABLE ct_s032
      FOR ALL ENTRIES IN lt_aux
      WHERE werks    EQ p_werks
        AND matnr    EQ lt_aux-matnr
        AND lgort    NE space.

    SELECT * FROM makt INTO TABLE ct_makt
      FOR ALL ENTRIES IN lt_aux
      WHERE matnr EQ lt_aux-matnr
        AND spras EQ sy-langu.

  ENDMETHOD.                    "select_data


  " Tratamento dos dados selecionados
  METHOD processing_data.

    DATA: lv_pcac TYPE wbwbest.


    SORT:ct_aux  BY werks matnr,
         ct_s032 BY werks matnr ASCENDING letztbew DESCENDING,
         ct_makt BY matnr.


    LOOP AT ct_aux INTO cs_aux.

      CLEAR cs_s032.
      READ TABLE ct_s032 INTO cs_s032
        WITH KEY werks = cs_aux-werks
                 matnr = cs_aux-matnr BINARY SEARCH.

      CHECK sy-subrc IS INITIAL AND cs_s032-letztbew LT cv_mov.

      CLEAR cs_makt.
      READ TABLE ct_makt INTO cs_makt
        WITH KEY matnr = cs_s032-matnr BINARY SEARCH.

      CHECK sy-subrc IS INITIAL.

      cv_tot = cv_tot + cs_aux-wbwbest.

    ENDLOOP.


    LOOP AT ct_aux INTO cs_aux.

      CLEAR cs_s032.
      READ TABLE ct_s032 INTO cs_s032
        WITH KEY werks = cs_aux-werks
                 matnr = cs_aux-matnr BINARY SEARCH.

      CHECK sy-subrc IS INITIAL AND cs_s032-letztbew LT cv_mov.

      CLEAR cs_makt.
      READ TABLE ct_makt INTO cs_makt
        WITH KEY matnr = cs_s032-matnr BINARY SEARCH.

      CHECK sy-subrc IS INITIAL.

      cs_relat-werks    = cs_s032-werks.
      cs_relat-matnr    = cs_s032-matnr.
      cs_relat-maktx    = cs_makt-maktx.
      cs_relat-bklas    = cs_s032-bklas.
      cs_relat-mbwbest  = cs_aux-mbwbest.
      cs_relat-basme    = cs_s032-basme.
      cs_relat-vlunit   = cs_aux-wbwbest / cs_aux-mbwbest.
      cs_relat-wbwbest  = cs_aux-wbwbest.
      cs_relat-hwaer    = cs_s032-hwaer.
      cs_relat-letztzug = cs_s032-letztzug.
      cs_relat-letztabg = cs_s032-letztabg.
      cs_relat-letztver = cs_s032-letztver.
      cs_relat-letztbew = cs_s032-letztbew.
      cs_relat-pctotal  = ( cs_aux-wbwbest / cv_tot ) * 100.
      APPEND cs_relat TO ct_relat. CLEAR cs_relat.

    ENDLOOP.

    SORT ct_relat BY wbwbest DESCENDING.

    LOOP AT ct_relat ASSIGNING FIELD-SYMBOL(<fs_relat>).
      lv_pcac = lv_pcac + <fs_relat>-pctotal.
      <fs_relat>-pcacumul = lv_pcac.

      " até 70% vermelho
      IF <fs_relat>-pcacumul LE 70.
        APPEND cs_red TO <fs_relat>-color.
      ENDIF.

      " entre 70% a 80% (verde)
      IF <fs_relat>-pcacumul GT 70 AND <fs_relat>-pcacumul LE 80.
        APPEND cs_green TO <fs_relat>-color.
      ENDIF.
    ENDLOOP.


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

    DATA: lo_lout        TYPE REF TO     cl_salv_layout,
          lo_column_list TYPE REF TO     cl_salv_columns_table,
          lo_column      TYPE REF TO     cl_salv_column,
          lo_sum         TYPE REF TO     cl_salv_aggregations,
          lo_sort        TYPE REF TO     cl_salv_sorts,
          lo_display     TYPE REF TO     cl_salv_display_settings,
          lo_events      TYPE REF TO     cl_salv_events_table.

    DATA: ls_key          TYPE            salv_s_layout_key.

    CHECK check_relat( ) IS INITIAL.

    " Cria objeto ALV
    co_salv = create_alv( ).

    co_sel = co_salv->get_selections( ).
    co_sel->set_selection_mode( if_salv_c_selection_mode=>multiple ).

    " Eventos
    lo_events = co_salv->get_event( ).

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
*    lo_lout->set_default( abap_true ).

    TRY.
        " Colunas ALV
        lo_column_list  = co_salv->get_columns( ).
        lo_column_list->set_optimize( abap_true ).
        lo_column_list->set_key_fixation( abap_true ).

        " Set color column to ALV
        TRY.
            lo_column_list->set_color_column( 'COLOR' ).
          CATCH cx_salv_data_error.
        ENDTRY.

        " Set nome colunas
        lo_column   = lo_column_list->get_column( 'MBWBEST' ).
        lo_column->set_long_text( 'Estoque total' ).
        lo_column->set_medium_text( 'Estoque total' ).
        lo_column->set_short_text( 'Est.total' ).

        lo_column   = lo_column_list->get_column( 'BASME' ).
        lo_column->set_long_text( 'Unidade de medida' ).
        lo_column->set_medium_text( 'UM' ).
        lo_column->set_short_text( 'UM' ).

        lo_column   = lo_column_list->get_column( 'VLUNIT' ).
        lo_column->set_long_text( 'Valor Unitário' ).
        lo_column->set_medium_text( 'Valor Unitário' ).
        lo_column->set_short_text( 'Vlr. Unit.' ).

        lo_column   = lo_column_list->get_column( 'PCTOTAL' ).
        lo_column->set_long_text( '%' ).
        lo_column->set_medium_text( '%' ).
        lo_column->set_short_text( '%' ).

        lo_column   = lo_column_list->get_column( 'PCACUMUL' ).
        lo_column->set_long_text( '%Acum' ).
        lo_column->set_medium_text( '%Acum' ).
        lo_column->set_short_text( '%Acum' ).

      CATCH cx_salv_not_found.
    ENDTRY.

    set_key( lo_column_list ).

    " Colunas com somatório
    lo_sum      = co_salv->get_aggregations( ).

    TRY.
        lo_sum->add_aggregation(
          EXPORTING
            columnname  = 'WBWBEST'
          aggregation = if_salv_c_aggregation=>total
        ).
      CATCH cx_salv_data_error.
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
    ENDTRY.

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
    MESSAGE s000(cl) WITH TEXT-e00 DISPLAY LIKE 'E'.
  ENDMETHOD.                    "CHECK_RELAT

  METHOD set_colors.
    cs_red-color-int   = 0.
    cs_red-color-inv   = 0.
    cs_red-color-col   = 6.

    cs_green-color-int = 0.
    cs_green-color-inv = 0.
    cs_green-color-col = 5.
  ENDMETHOD.                    "set_colors

  METHOD send_email.

    DATA: lv_title TYPE            string,
          lv_file  TYPE            string,
          lv_email TYPE            string.

    DATA: lo_excel              TYPE REF TO     zcl_excel,
          lo_worksheet          TYPE REF TO     zcl_excel_worksheet,
          lo_column_dim         TYPE REF TO     zcl_excel_worksheet_columndime,
          lo_style_header_blue  TYPE REF TO     zcl_excel_style,
          lo_style_footer_yel   TYPE REF TO     zcl_excel_style,
          lo_style_body_right   TYPE REF TO     zcl_excel_style,
          lo_style_body_left    TYPE REF TO     zcl_excel_style,
          lo_style_body_date    TYPE REF TO     zcl_excel_style,
          lo_style_body_date_n  TYPE REF TO     zcl_excel_style,
          lo_style_body_dec4    TYPE REF TO     zcl_excel_style,
          lo_style_body_dec4_n  TYPE REF TO     zcl_excel_style,
          lo_style_body_color   TYPE REF TO     zcl_excel_style,
          lo_style_body_color_l TYPE REF TO     zcl_excel_style,
          lo_autofilter         TYPE REF TO     zcl_excel_autofilter,
          lo_output             TYPE REF TO     lcl_output.


    DATA: lv_style_header_guid    TYPE            zexcel_cell_style,
          lv_style_footer_guid    TYPE            zexcel_cell_style,
          lv_style_body_color     TYPE            zexcel_cell_style,
          lv_style_body_color_lef TYPE            zexcel_cell_style,
          lv_style_body_date      TYPE            zexcel_cell_style,
          lv_style_body_dec4      TYPE            zexcel_cell_style,
          lv_row                  TYPE            i VALUE 1,
          ls_area                 TYPE            zexcel_s_autofilter_area.


    CHECK check_relat( ) IS INITIAL.

    CREATE OBJECT lo_excel.
    CREATE OBJECT lo_output.

    lo_worksheet = lo_excel->get_active_worksheet( ).
    lo_worksheet->set_title( 'Itens obsoletos' ).

    " Estilo cabeçalho
    lo_style_header_blue                            = lo_excel->add_new_style( ).
    lo_style_header_blue->font->bold                = abap_true.
    lo_style_header_blue->fill->filltype            = zcl_excel_style_fill=>c_fill_solid.
    lo_style_header_blue->fill->fgcolor-rgb         = c_blue.
    lv_style_header_guid                            = lo_style_header_blue->get_guid( ).

    " Estilo rodapé (total)
    lo_style_footer_yel                             = lo_excel->add_new_style( ).
    lo_style_footer_yel->font->bold                 = abap_true.
    lo_style_footer_yel->fill->filltype             = zcl_excel_style_fill=>c_fill_solid.
    lo_style_footer_yel->fill->fgcolor-rgb          = c_yellow.
    lo_style_footer_yel->number_format->format_code = zcl_excel_style_number_format=>c_format_number_comma_sep1.
    lv_style_footer_guid                            = lo_style_footer_yel->get_guid( ).

    set_xls_header(
      EXPORTING
        i_guid      = lv_style_header_guid
      CHANGING
        c_worksheet = lo_worksheet
    ).

    "Estilo body normal
    lo_style_body_right                               = lo_excel->add_new_style( ).
    lo_style_body_right->alignment->horizontal        = zcl_excel_style_alignment=>c_horizontal_right.
    lo_style_body_right->number_format->format_code   = zcl_excel_style_number_format=>c_format_number_comma_sep1.

    lo_style_body_left                                = lo_excel->add_new_style( ).
    lo_style_body_left->alignment->horizontal         = zcl_excel_style_alignment=>c_horizontal_left.
    lo_style_body_left->number_format->format_code    = zcl_excel_style_number_format=>c_format_number_comma_sep1.

    lo_style_body_dec4_n                              = lo_excel->add_new_style( ).
    lo_style_body_dec4_n->alignment->horizontal       = zcl_excel_style_alignment=>c_horizontal_right.
    lo_style_body_dec4_n->number_format->format_code  = '#,##0.0000'.

    lo_style_body_date_n                              = lo_excel->add_new_style( ).
    lo_style_body_date_n->alignment->horizontal       = zcl_excel_style_alignment=>c_horizontal_right.
    lo_style_body_date_n->number_format->format_code  = 'dd/mm/yyyy'.


    LOOP AT ct_relat INTO cs_relat.

      FREE lo_style_body_color. CLEAR lo_style_body_color. " c/cor à direita
      lo_style_body_color                               = lo_excel->add_new_style( ).
      lo_style_body_color->fill->filltype               = zcl_excel_style_fill=>c_fill_solid.
      lo_style_body_color->alignment->horizontal        = zcl_excel_style_alignment=>c_horizontal_right.
      lo_style_body_color->number_format->format_code   = zcl_excel_style_number_format=>c_format_number_comma_sep1.

      FREE lo_style_body_color_l. CLEAR lo_style_body_color_l. " c/cor à esquerda
      lo_style_body_color_l                             = lo_excel->add_new_style( ).
      lo_style_body_color_l->fill->filltype             = zcl_excel_style_fill=>c_fill_solid.
      lo_style_body_color_l->alignment->horizontal      = zcl_excel_style_alignment=>c_horizontal_left.
      lo_style_body_color_l->number_format->format_code = zcl_excel_style_number_format=>c_format_number_comma_sep1.

      FREE lo_style_body_date. CLEAR lo_style_body_date. " c/cor format date
      lo_style_body_date                             = lo_excel->add_new_style( ).
      lo_style_body_date->fill->filltype             = zcl_excel_style_fill=>c_fill_solid.
      lo_style_body_date->alignment->horizontal      = zcl_excel_style_alignment=>c_horizontal_right.
      lo_style_body_date->number_format->format_code = 'dd/mm/yyyy'.

      FREE lo_style_body_dec4. CLEAR lo_style_body_dec4. " c/cor formato 4 decimals
      lo_style_body_dec4                             = lo_excel->add_new_style( ).
      lo_style_body_dec4->fill->filltype             = zcl_excel_style_fill=>c_fill_solid.
      lo_style_body_dec4->alignment->horizontal      = zcl_excel_style_alignment=>c_horizontal_right.
      lo_style_body_dec4->number_format->format_code = '#,##0.0000'.

      IF cs_relat-pcacumul LE 70. "vermelho
        lo_style_body_color->fill->fgcolor-rgb    = c_red.
        lo_style_body_color_l->fill->fgcolor-rgb  = c_red.
        lo_style_body_date->fill->fgcolor-rgb     = c_red.
        lo_style_body_dec4->fill->fgcolor-rgb     = c_red.
      ENDIF.

      IF cs_relat-pcacumul GT 70 AND cs_relat-pcacumul LE 80. "verde
        lo_style_body_color->fill->fgcolor-rgb    = c_green.
        lo_style_body_color_l->fill->fgcolor-rgb  = c_green.
        lo_style_body_date->fill->fgcolor-rgb     = c_green.
        lo_style_body_dec4->fill->fgcolor-rgb     = c_green.
      ENDIF.

      IF cs_relat-pcacumul GT 80."s/cor
        lv_style_body_color                         = lo_style_body_right->get_guid( ).
        lv_style_body_color_lef                     = lo_style_body_left->get_guid( ).
        lv_style_body_date                          = lo_style_body_date_n->get_guid( ).
        lv_style_body_dec4                          = lo_style_body_dec4_n->get_guid( ).
      ELSE.
        lv_style_body_color                         = lo_style_body_color->get_guid( ).
        lv_style_body_color_lef                     = lo_style_body_color_l->get_guid( ).
        lv_style_body_date                          = lo_style_body_date->get_guid( ).
        lv_style_body_dec4                          = lo_style_body_dec4->get_guid( ).
      ENDIF.

      lv_row = lv_row + 1.
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'A' ip_value = cs_relat-werks    ip_style = lv_style_body_color     ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'B' ip_value = cs_relat-matnr    ip_style = lv_style_body_color     ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'C' ip_value = cs_relat-maktx    ip_style = lv_style_body_color_lef ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'D' ip_value = cs_relat-bklas    ip_style = lv_style_body_color     ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'E' ip_value = cs_relat-mbwbest  ip_style = lv_style_body_color     ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'F' ip_value = cs_relat-basme    ip_style = lv_style_body_color_lef ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'G' ip_value = cs_relat-vlunit   ip_style = lv_style_body_dec4      ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'H' ip_value = cs_relat-wbwbest  ip_style = lv_style_body_color     ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'I' ip_value = cs_relat-hwaer    ip_style = lv_style_body_color_lef ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'J' ip_value = cs_relat-letztzug ip_style = lv_style_body_date      ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'K' ip_value = cs_relat-letztabg ip_style = lv_style_body_date      ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'L' ip_value = cs_relat-letztver ip_style = lv_style_body_date      ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'M' ip_value = cs_relat-letztbew ip_style = lv_style_body_date      ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'N' ip_value = cs_relat-pctotal  ip_style = lv_style_body_color     ).
      lo_worksheet->set_cell( ip_row = lv_row ip_column = 'O' ip_value = cs_relat-pcacumul ip_style = lv_style_body_color     ).

    ENDLOOP.

    " Última linha com total
    lv_row = lv_row + 1.
    set_xls_footer(
      EXPORTING
        i_guid      = lv_style_footer_guid
        i_row       = lv_row
      CHANGING
        c_worksheet = lo_worksheet
    ).

    "filtros
    lo_autofilter     = lo_excel->add_new_autofilter( io_sheet = lo_worksheet ).
    ls_area-row_start = 1.
    ls_area-col_start = 1.
    ls_area-row_end   = lo_worksheet->get_highest_row( ).
    ls_area-col_end   = lo_worksheet->get_highest_column( ).
    lo_autofilter->set_filter_area( is_area = ls_area ).

    me->set_xls_autosize(
      CHANGING
        c_worksheet  = lo_worksheet
        c_column_dim = lo_column_dim
    ).

    lv_title = 'Itens Obsoletos'.
    lv_file  = |itens_obsoletos_{ p_werks }.xlsx|.
    lv_email = p_emai.

    lo_output->set_email(
     EXPORTING
       email     = lv_email
       file      = lv_file
       i_title   = lv_title
   ).

    lo_output->output( lo_excel ).


  ENDMETHOD.                    "send_email

  METHOD set_xls_header.

    c_worksheet->set_cell( ip_row = 1 ip_column = 'A' ip_value = 'Centro'           ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'B' ip_value = 'Material'         ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'C' ip_value = 'Descrição'        ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'D' ip_value = 'ClAv.'            ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'E' ip_value = 'Estoque total'    ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'F' ip_value = 'UM'               ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'G' ip_value = 'Valor Unitário'   ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'H' ip_value = 'Valor Estoque'    ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'I' ip_value = 'Moeda'            ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'J' ip_value = 'Última Entrada'   ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'K' ip_value = 'Última Saída'     ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'L' ip_value = 'Último Consumo'   ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'M' ip_value = 'Último Movimento' ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'N' ip_value = '%'                ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = 1 ip_column = 'O' ip_value = '%Acum'            ip_style = i_guid ).

  ENDMETHOD.

  METHOD set_xls_footer.

    c_worksheet->set_cell( ip_row = i_row ip_column = 'A' ip_value = space  ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'B' ip_value = space  ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'C' ip_value = space  ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'D' ip_value = space  ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'E' ip_value = space  ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'F' ip_value = space  ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'G' ip_value = space  ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'H' ip_value = cv_tot ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'I' ip_value = space  ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'J' ip_value = space  ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'K' ip_value = space  ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'L' ip_value = space  ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'M' ip_value = space  ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'N' ip_value = space  ip_style = i_guid ).
    c_worksheet->set_cell( ip_row = i_row ip_column = 'O' ip_value = space  ip_style = i_guid ).

  ENDMETHOD.

  METHOD set_xls_autosize.

    DATA lv_alpha TYPE char3.

    DO 15 TIMES. " A até O
      sy-index     = sy-index - 1.
      lv_alpha     = sy-abcde+sy-index(1).
      c_column_dim = c_worksheet->get_column_dimension( ip_column = lv_alpha ).
      c_column_dim->set_auto_size( 'X' ).
    ENDDO.

  ENDMETHOD.

ENDCLASS.                    "lcl_relat IMPLEMENTATION

*--------------------------------------------------------------------
* Objetos
*--------------------------------------------------------------------
DATA: go_relat            TYPE REF TO     lcl_relat.
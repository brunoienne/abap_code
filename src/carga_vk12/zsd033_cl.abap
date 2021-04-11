*&-----------------------------------------------------------------&*
*                             ClipTech                              *
*&-----------------------------------------------------------------&*
* Programa ..: ZSD033                                               *
* Transação..:                                                      *
* Módulo.....: SD                                                   *
* Especif....: ZSD033                                               *
* Funcional..: Filipe E. Morais                                     *
*&---------------------------------------------------------------- *&
* Descrição .: 54699/54700 - Atualização preço VK12                 *
*&-----------------------------------------------------------------*&
* Objetivo ..: Atualizar valores de condições VK12 via BAPI         *
*&-----------------------------------------------------------------*&
*                           HISTÓRICO                               *
*&-----------------------------------------------------------------*&
* DATA       | REQUEST    | RESPONSÁVEL | DESCRIÇÃO                 *
* 09/02/2021 | ER1K936081 | BIMORAIS    | Atualização preço VK12    *
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

      get_file
       IMPORTING
         i_filename       TYPE            rlgrap-filename
       CHANGING
         tc_file          TYPE            STANDARD TABLE,

      get_filename
        RETURNING
          value(filename) TYPE            rlgrap-filename,

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

      on_user_command
        FOR EVENT added_function OF cl_salv_events,

     call_bapi_price_con
      CHANGING
        c_relat TYPE any,

     fill_msg_bapiret
      RETURNING
        value(r_bapiret) TYPE bapiret2_t,

     create_dynamic_table.


*--------------------------------------------------------------------
* Objetos
*--------------------------------------------------------------------
    DATA:
      co_struct           TYPE REF TO     cl_abap_structdescr,
      co_salv             TYPE REF TO     cl_salv_table,
      co_sel              TYPE REF TO     cl_salv_selections.


*--------------------------------------------------------------------
* Tabelas Internas
*--------------------------------------------------------------------
    DATA:
      ct_fa929              TYPE TABLE OF   file_929   ,
      ct_fa004               TYPE TABLE OF   file_004   ,
      ct_a929               TYPE TABLE OF   a929       ,
      ct_a004               TYPE TABLE OF   a004       ,
      ct_konp               TYPE TABLE OF   konp       ,
      ct_fieldcat           TYPE lvc_t_fcat            .

*--------------------------------------------------------------------
* Work areas
*--------------------------------------------------------------------
    DATA:
      cs_t685               TYPE            t685       ,
      cs_f929               TYPE            file_929   ,
      cs_f004               TYPE            file_004   ,
      cs_a929               TYPE            a929       ,
      cs_a004               TYPE            a004       ,
      cs_konp               TYPE            konp       ,
      cs_variant            TYPE            disvariant .


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

    FIELD-SYMBOLS: <fs_tab>  TYPE ANY TABLE,
                   <fs_tabf> TYPE ANY TABLE.

    DATA: lv_tabname TYPE tabname,
          lv_tab     TYPE tabname,
          lv_tabfile TYPE tabname,
          lv_where   TYPE string.

    SELECT SINGLE * FROM t685 INTO cs_t685
      WHERE kschl EQ p_kschl.

    CHECK sy-subrc IS INITIAL.

    CASE 'X'.
      WHEN p_rb01.

        get_file(
         EXPORTING
           i_filename = p_dir
         CHANGING
           tc_file    = ct_fa929
        ).

        lv_tabname = 'A929'.
        lv_where   = ' VKORG EQ <FS_TABF>-VKORG AND ' &&
                     ' KDGRP EQ <FS_TABF>-KDGRP AND ' &&
                     ' MATNR EQ <FS_TABF>-MATNR AND ' &&
                     ' KFRST EQ SPACE'.

      WHEN p_rb02.

        get_file(
         EXPORTING
           i_filename = p_dir
         CHANGING
           tc_file    = ct_fa004
        ).

        lv_tabname = 'A004'.
        lv_where   = ' VKORG EQ <FS_TABF>-VKORG AND ' &&
                     ' VTWEG EQ <FS_TABF>-VTWEG AND ' &&
                     ' MATNR EQ <FS_TABF>-MATNR'.


    ENDCASE.

    CONCATENATE 'CT_' lv_tabname  INTO lv_tab.
    CONCATENATE 'CT_F' lv_tabname INTO lv_tabfile.

    ASSIGN (lv_tab)     TO <fs_tab>.
    ASSIGN (lv_tabfile) TO <fs_tabf>.

    CHECK <fs_tab> IS ASSIGNED AND <fs_tabf> IS ASSIGNED.

    SELECT * FROM (lv_tabname) INTO TABLE <fs_tab>
      FOR ALL ENTRIES IN <fs_tabf>
      WHERE kappl EQ cs_t685-kappl
        AND kschl EQ cs_t685-kschl
        AND datbi GE p_datam
        AND datab LE p_datam
        AND (lv_where).

    CHECK <fs_tab> IS NOT INITIAL.

    CLEAR lv_where.
    lv_where = ' KNUMH EQ <FS_TAB>-KNUMH'.

    SELECT * FROM konp INTO TABLE ct_konp
      FOR ALL ENTRIES IN <fs_tab>
      WHERE (lv_where).

  ENDMETHOD.                    "select_data


  " Tratamento dos dados selecionados
  METHOD processing_data.

    DATA: ls_status  TYPE status_type.

    me->create_dynamic_table( ).

    SORT ct_konp BY knumh.

    CASE 'X'.
      WHEN p_rb01.

        SORT ct_a929 BY vkorg kdgrp matnr.

        LOOP AT ct_fa929 INTO cs_f929.

          CLEAR cs_a929.
          READ TABLE ct_a929 INTO cs_a929
            WITH KEY vkorg = cs_f929-vkorg
                     kdgrp = cs_f929-kdgrp
                     matnr = cs_f929-matnr BINARY SEARCH.

          IF sy-subrc IS NOT INITIAL.
            ls_status-log      = c_log.
            ls_status-messages = fill_msg_bapiret( ).
          ENDIF.

          MOVE-CORRESPONDING cs_f929   TO <dyn_wa>.
          MOVE-CORRESPONDING ls_status TO <dyn_wa>. CLEAR ls_status.
          APPEND <dyn_wa> TO <dyn_table>. CLEAR <dyn_wa>.

        ENDLOOP.

      WHEN p_rb02.

        SORT ct_a004 BY vkorg vtweg matnr.

        LOOP AT ct_fa004 INTO cs_f004.

          CLEAR cs_a004.
          READ TABLE ct_a004 INTO cs_a004
            WITH KEY vkorg = cs_f004-vkorg
                     vtweg = cs_f004-vtweg
                     matnr = cs_f004-matnr BINARY SEARCH.

          IF sy-subrc IS NOT INITIAL.
            ls_status-log      = c_log.
            ls_status-messages = fill_msg_bapiret( ).
          ENDIF.

          MOVE-CORRESPONDING cs_f004   TO <dyn_wa>.
          MOVE-CORRESPONDING ls_status TO <dyn_wa>. CLEAR ls_status.
          APPEND <dyn_wa> TO <dyn_table>. CLEAR <dyn_wa>.

        ENDLOOP.

    ENDCASE.

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
            t_table      = <dyn_table> ).
      CATCH cx_salv_msg .
    ENDTRY.

  ENDMETHOD.                    "create_alv


  " Saída ALV
  METHOD output_data.

    DATA: lo_column_tab   TYPE REF TO     cl_salv_column_table,
          lo_lout         TYPE REF TO     cl_salv_layout,
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
    co_sel->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    " Eventos
    lo_events = co_salv->get_event( ).
    SET HANDLER on_link_click FOR lo_events.    " HOTSPOT click
*    SET HANDLER on_double_click FOR lo_events.  " Double click row
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
*    lo_lout->set_default( abap_true ).

    TRY.

        " Colunas ALV
        lo_column_list  = co_salv->get_columns( ).
        lo_column_list->set_optimize( abap_true ).
        lo_column_list->set_key_fixation( abap_true ).

        "Set icon
        lo_column   = lo_column_list->get_column( 'LOG' ).
        lo_column->set_short_text( '' ).
        lo_column->set_medium_text( '' ).
        lo_column->set_long_text( 'Log' ).

        lo_column_tab ?= lo_column.
        lo_column_tab->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lo_column_tab->set_icon( if_salv_c_bool_sap=>true ).

      CATCH cx_salv_not_found.
    ENDTRY.

    set_key( lo_column_list ).

    " Colunas com somatório
    lo_sum      = co_salv->get_aggregations( ).


    " Exibe ALV
    co_salv->display( ).


  ENDMETHOD.                    "output_data

  METHOD get_filename.

    DATA:
      lt_file             TYPE            filetable.

    DATA:
      ls_file             TYPE            file_table.

    DATA:
      lv_rc               TYPE            i.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        file_filter             = '*.CSV'
      CHANGING
        file_table              = lt_file
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.

    IF sy-subrc IS INITIAL.
      READ TABLE lt_file INTO ls_file INDEX 1.
      filename = ls_file-filename.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.                    "get_filename

  METHOD get_file.

    DATA:
      lt_file             TYPE TABLE OF   string,
      lt_fields           TYPE TABLE OF   string.

    DATA:
      ls_file             TYPE            string,
      ls_field            TYPE            string,
      ls_comp             TYPE            abap_compdescr.


    DATA:
      lo_struct           TYPE REF TO     cl_abap_structdescr,
      lo_new_line         TYPE REF TO     data.

    DATA:
      lv_diretorio        TYPE            string.

    FIELD-SYMBOLS:
      <fs_file>           TYPE            any,
      <ff_field>          TYPE            any.

    CHECK i_filename IS NOT INITIAL.

    lv_diretorio = i_filename.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = lv_diretorio
      CHANGING
        data_tab                = lt_file
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.

    CREATE DATA lo_new_line LIKE LINE OF tc_file.
    ASSIGN lo_new_line->* TO <fs_file>.

    CHECK <fs_file> IS ASSIGNED.
    lo_struct ?= cl_abap_typedescr=>describe_by_data( <fs_file> ).

    DELETE lt_file INDEX 1." Remove header line

    " Reade line by line
    LOOP AT lt_file INTO ls_file.

      " Split line into Table fields
      SPLIT ls_file AT ';' INTO TABLE lt_fields.

      " Read field by field
      LOOP AT lt_fields INTO ls_field.

        CHECK ls_field IS NOT INITIAL.

        " Get corresponding field on structure
        READ TABLE lo_struct->components INTO ls_comp INDEX sy-tabix.

        UNASSIGN <ff_field>.
        ASSIGN COMPONENT ls_comp-name OF STRUCTURE <fs_file> TO <ff_field>.

        CASE ls_comp-name.
          WHEN 'MATNR'.

            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = ls_field
              IMPORTING
                output       = <ff_field>
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.

          WHEN OTHERS.
            CASE ls_comp-type_kind.
              WHEN 'C' OR 'g'.
                <ff_field>  = ls_field.

              WHEN 'P'.
                TRANSLATE ls_field USING '. '.
                TRANSLATE ls_field USING ',.'.
                CONDENSE ls_field NO-GAPS.

                <ff_field>  = ls_field.

            ENDCASE.
        ENDCASE.

      ENDLOOP.

      APPEND <fs_file> TO tc_file.
      CLEAR <fs_file>.

    ENDLOOP.

  ENDMETHOD.                    "get_file

  " Set coluna chave
  METHOD set_key.

    DATA: lo_columns      TYPE REF TO     cl_salv_column_table.

    DATA: lo_struct       TYPE REF TO     cl_abap_structdescr.

    DATA: ls_key004         TYPE            a004_key,
          ls_key929         TYPE            a929_key,
          ls_comp           TYPE            abap_compdescr.

    CASE 'X'.
      WHEN p_rb01.
        lo_struct ?= cl_abap_datadescr=>describe_by_data( ls_key929 ).
      WHEN p_rb02.
        lo_struct ?= cl_abap_datadescr=>describe_by_data( ls_key004 ).
    ENDCASE.

    LOOP AT lo_struct->components INTO ls_comp.

      TRY .
          lo_columns  ?= lo_column_list->get_column( ls_comp-name ).
        CATCH cx_salv_not_found.  " ALV: General Error Class (Checked During Syntax Check)
      ENDTRY.

      TRY .
        lo_columns->set_key( abap_true ).
      ENDTRY.

    ENDLOOP.


  ENDMETHOD.                    "set_key


  METHOD check_relat.
    CHECK <dyn_table> IS INITIAL.
    subrc = 4.
    MESSAGE s000(cl) WITH text-e00 DISPLAY LIKE 'E'.
  ENDMETHOD.                    "CHECK_RELAT

  METHOD on_link_click.

    FIELD-SYMBOLS: <fs_msg> TYPE bapiret2_t.

    UNASSIGN <dyn_wa>.
    READ TABLE <dyn_table> ASSIGNING <dyn_wa> INDEX row.

    ASSIGN COMPONENT 'MESSAGES' OF STRUCTURE <dyn_wa> TO  <fs_msg>.

    CHECK <fs_msg> IS ASSIGNED AND <fs_msg> IS NOT INITIAL.

    CALL FUNCTION 'C14ALD_BAPIRET2_SHOW'
      TABLES
        i_bapiret2_tab = <fs_msg>.

  ENDMETHOD.                    "on_link_click

  METHOD on_user_command.

    DATA: ls_stable           TYPE         lvc_s_stbl  ,
          lv_ans              TYPE         c           .

    DATA:
      lt_rows                 TYPE         salv_t_row  ,
      ls_row                  LIKE LINE OF lt_rows     .

    lt_rows[] = co_sel->get_selected_rows( ).

    CASE sy-ucomm.
      WHEN '_SEND'.

        IF lt_rows[] IS INITIAL.
          MESSAGE s000(cl) WITH text-e01 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question = text-003
          IMPORTING
            answer        = lv_ans. " 1-sim 2-não A-cancelar

        CHECK lv_ans EQ '1'.

        LOOP AT lt_rows INTO ls_row.

          UNASSIGN <dyn_wa>.
          READ TABLE <dyn_table> ASSIGNING <dyn_wa> INDEX ls_row.

          CHECK <dyn_wa> IS ASSIGNED.

          me->call_bapi_price_con(
           CHANGING
             c_relat = <dyn_wa>
          ).

        ENDLOOP.

    ENDCASE.

    ls_stable-row = abap_true.
    ls_stable-col = abap_true.
    co_salv->refresh( s_stable = ls_stable ).


  ENDMETHOD.                    "on_user_command

  METHOD call_bapi_price_con.

    FIELD-SYMBOLS: <fs_log>      TYPE                    any                ,
                   <fs_messages> TYPE                    bapiret2_t         .

    DATA: ls_values              TYPE                    values_type        ,
          ls_key004              TYPE                    a004_key           ,
          ls_key929              TYPE                    a929_key           ,
          ls_bapicondct          TYPE                    bapicondct         ,
          ls_bapicondhd          TYPE                    bapicondhd         ,
          ls_bapicondit          TYPE                    bapicondit         .

    DATA: lt_bapicondct          TYPE STANDARD TABLE OF  bapicondct         ,
          lt_bapicondhd          TYPE STANDARD TABLE OF  bapicondhd         ,
          lt_bapicondit          TYPE STANDARD TABLE OF  bapicondit         ,
          lt_bapicondqs          TYPE STANDARD TABLE OF  bapicondqs         ,
          lt_bapicondvs          TYPE STANDARD TABLE OF  bapicondvs         ,
          lt_bapiret2            TYPE STANDARD TABLE OF  bapiret2           ,
          lt_bapiknumhs          TYPE STANDARD TABLE OF  bapiknumhs         ,
          lt_mem_initial         TYPE STANDARD TABLE OF  cnd_mem_initial    .

    " Valores do arquivo
    MOVE-CORRESPONDING c_relat TO ls_values.
    ASSIGN COMPONENT 'LOG'      OF STRUCTURE c_relat TO <fs_log>.
    ASSIGN COMPONENT 'MESSAGES' OF STRUCTURE c_relat TO <fs_messages>.

    CHECK <fs_log> IS INITIAL.

    CASE 'X'.
      WHEN p_rb01.
        MOVE-CORRESPONDING c_relat TO ls_key929.

        CLEAR cs_a929.
        READ TABLE ct_a929 INTO cs_a929
          WITH KEY  vkorg = ls_key929-vkorg
                    kdgrp = ls_key929-kdgrp
                    matnr = ls_key929-matnr BINARY SEARCH.

        CLEAR cs_konp.
        READ TABLE ct_konp INTO cs_konp
          WITH KEY knumh = cs_a929-knumh BINARY SEARCH.

        ls_bapicondct-table_no   = c_929.
        ls_bapicondct-varkey     = ls_key929.
        ls_bapicondct-valid_from = cs_a929-datab.
        ls_bapicondct-valid_to   = cs_a929-datbi.

        ls_bapicondhd-table_no   = c_929.
        ls_bapicondhd-varkey     = ls_key929.
        ls_bapicondhd-valid_from = cs_a929-datab.
        ls_bapicondhd-valid_to   = cs_a929-datbi.

      WHEN p_rb02.
        MOVE-CORRESPONDING c_relat TO ls_key004.

        CLEAR cs_a004.
        READ TABLE ct_a004 INTO cs_a004
          WITH KEY  vkorg = ls_key004-vkorg
                    vtweg = ls_key004-vtweg
                    matnr = ls_key004-matnr BINARY SEARCH.

        CLEAR cs_konp.
        READ TABLE ct_konp INTO cs_konp
          WITH KEY knumh = cs_a004-knumh BINARY SEARCH.

        ls_bapicondct-table_no   = c_004.
        ls_bapicondct-varkey     = ls_key004.
        ls_bapicondct-valid_from = cs_a004-datab.
        ls_bapicondct-valid_to   = cs_a004-datbi.

        ls_bapicondhd-table_no   = c_004.
        ls_bapicondhd-varkey     = ls_key004.
        ls_bapicondhd-valid_from = cs_a004-datab.
        ls_bapicondhd-valid_to   = cs_a004-datbi.

    ENDCASE.

    ls_bapicondct-cond_usage = cs_t685-kvewe.
    ls_bapicondct-applicatio = cs_konp-kappl.
    ls_bapicondct-cond_type  = cs_konp-kschl.
    ls_bapicondct-operation  = c_004.
    ls_bapicondct-cond_no    = cs_konp-knumh.

    ls_bapicondhd-cond_usage = cs_t685-kvewe.
    ls_bapicondhd-applicatio = cs_konp-kappl.
    ls_bapicondhd-cond_type  = cs_konp-kschl.
    ls_bapicondhd-operation  = c_004.
    ls_bapicondhd-cond_no    = cs_konp-knumh.

    ls_bapicondit-applicatio = cs_konp-kappl.
    ls_bapicondit-cond_type  = cs_konp-kschl.
    ls_bapicondit-operation  = c_004.
    ls_bapicondit-cond_count = 1.
    ls_bapicondit-cond_no    = cs_konp-knumh.
    ls_bapicondit-scaletype  = cs_konp-stfkz.
    ls_bapicondit-calctypcon = cs_konp-krech.

    IF ls_values-kbetr IS NOT INITIAL.
      ls_bapicondit-cond_value = ls_values-kbetr.
    ELSE.
      ls_bapicondit-cond_value = cs_konp-kbetr.
    ENDIF.

    IF ls_values-konwa IS NOT INITIAL.
      ls_bapicondit-condcurr = ls_values-konwa.
    ELSE.
      ls_bapicondit-condcurr = cs_konp-konwa.
    ENDIF.

    IF ls_values-mxwrt IS NOT INITIAL.
      ls_bapicondit-lowerlimit = ls_values-mxwrt.
    ELSE.
      ls_bapicondit-lowerlimit = cs_konp-mxwrt.
    ENDIF.

    IF ls_values-gkwrt IS NOT INITIAL.
      ls_bapicondit-upperlimit = ls_values-gkwrt.
    ELSE.
      ls_bapicondit-upperlimit = cs_konp-gkwrt.
    ENDIF.

    IF ls_values-kpein IS NOT INITIAL.
      ls_bapicondit-cond_p_unt = ls_values-kpein.
    ELSE.
      ls_bapicondit-cond_p_unt = cs_konp-kpein.
    ENDIF.

    IF ls_values-kmein IS NOT INITIAL.
      ls_bapicondit-cond_unit = ls_values-kmein.
    ELSE.
      ls_bapicondit-cond_unit = cs_konp-kmein.
    ENDIF.

    APPEND ls_bapicondct TO lt_bapicondct.
    APPEND ls_bapicondhd TO lt_bapicondhd.
    APPEND ls_bapicondit TO lt_bapicondit.

    CALL FUNCTION 'BAPI_PRICES_CONDITIONS'
      TABLES
        ti_bapicondct  = lt_bapicondct
        ti_bapicondhd  = lt_bapicondhd
        ti_bapicondit  = lt_bapicondit
        ti_bapicondqs  = lt_bapicondqs
        ti_bapicondvs  = lt_bapicondvs
        to_bapiret2    = lt_bapiret2
        to_bapiknumhs  = lt_bapiknumhs
        to_mem_initial = lt_mem_initial
      EXCEPTIONS
        update_error   = 1
        OTHERS         = 2.

    <fs_log>           = c_log.
    <fs_messages>      = lt_bapiret2.

    READ TABLE lt_bapiret2
      WITH KEY type = 'E' TRANSPORTING NO FIELDS.

    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.


  ENDMETHOD.                    "call_bapi_price_con

  METHOD fill_msg_bapiret.

    DATA: ls_ret   TYPE bapiret2.

    ls_ret-type       = 'E'.
    ls_ret-id         = 'CL'.
    ls_ret-message_v1 = text-e02.
    APPEND ls_ret TO r_bapiret.

  ENDMETHOD.                    "fill_msg_bapiret

  METHOD create_dynamic_table.

    DATA: new_table         TYPE REF TO     data,
          new_line          TYPE REF TO     data.

    DATA: ls_key004         TYPE            a004_key,
          ls_key929         TYPE            a929_key,
          ls_values         TYPE            values_type,
          ls_status         TYPE            status_type,
          ls_comp           TYPE            abap_compdescr,
          ls_fcat           TYPE            lvc_s_fcat.

    CASE 'X'.
      WHEN p_rb01.
        co_struct ?= cl_abap_datadescr=>describe_by_data( ls_key929 ).
      WHEN p_rb02.
        co_struct ?= cl_abap_datadescr=>describe_by_data( ls_key004 ).
    ENDCASE.

    LOOP AT co_struct->components INTO ls_comp.
      ls_fcat-fieldname = ls_comp-name.
      ls_fcat-inttype   = ls_comp-type_kind.
      ls_fcat-intlen    = ls_comp-length.

      CASE ls_comp-name.
        WHEN 'MATNR'.
          ls_fcat-ref_field = ls_fcat-fieldname.
          ls_fcat-ref_table = 'MARA'.
        WHEN 'VKORG' OR 'KDGRP'.
          ls_fcat-ref_field = ls_fcat-fieldname.
          ls_fcat-ref_table = 'A929'.
        WHEN 'VTWEG'.
          ls_fcat-ref_field = ls_fcat-fieldname.
          ls_fcat-ref_table = 'A004'.
      ENDCASE.
      APPEND ls_fcat TO ct_fieldcat. CLEAR ls_fcat.
    ENDLOOP.

    CLEAR co_struct.
    co_struct ?= cl_abap_datadescr=>describe_by_data( ls_values ).

    LOOP AT co_struct->components INTO ls_comp.
      ls_fcat-fieldname = ls_comp-name.
      ls_fcat-inttype   = ls_comp-type_kind.
      ls_fcat-intlen    = ls_comp-length.

      CASE ls_comp-name.
        WHEN 'KBETR' OR 'KONWA' OR 'KPEIN' OR 'KMEIN' OR 'MXWRT' OR 'GKWRT'.
          ls_fcat-ref_field = ls_fcat-fieldname.
          ls_fcat-ref_table = 'KONP'.
      ENDCASE.
      APPEND ls_fcat TO ct_fieldcat. CLEAR ls_fcat.
    ENDLOOP.

    CLEAR co_struct.
    co_struct ?= cl_abap_datadescr=>describe_by_data( ls_status ).

    LOOP AT co_struct->components INTO ls_comp.
      ls_fcat-fieldname = ls_comp-name.
      ls_fcat-inttype   = ls_comp-type_kind.

      CASE ls_comp-name.
        WHEN 'LOG'.
          ls_fcat-outputlen = 4.
          ls_fcat-intlen    = 4.

        WHEN 'MESSAGES'.
          ls_fcat-ref_field = ls_fcat-fieldname.
          ls_fcat-ref_table = 'CVIS_MESSAGE'.

      ENDCASE.
      APPEND ls_fcat TO ct_fieldcat. CLEAR ls_fcat.
    ENDLOOP.

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = ct_fieldcat
      IMPORTING
        ep_table        = new_table.

    ASSIGN new_table->* TO <dyn_table>.

    CREATE DATA new_line LIKE LINE OF <dyn_table>.
    ASSIGN new_line->* TO <dyn_wa>.

  ENDMETHOD.                    "create_dynamic_table

ENDCLASS.                    "lcl_relat IMPLEMENTATION

*--------------------------------------------------------------------
* Objetos
*--------------------------------------------------------------------
DATA: go_relat            TYPE REF TO     lcl_relat.
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
* Classe relatório Definição
*----------------------------------------------------------------------
CLASS lcl_relat DEFINITION.
    PUBLIC SECTION.
      METHODS:
        select_data,
  
        processing_data,
  
        output_data,
  
        get_directory
          RETURNING
          value(r_directory) TYPE string,
  
        get_file_path
          RETURNING
            value(r_path)    TYPE string.
  
    PRIVATE SECTION.
      METHODS:
        check_relat
          RETURNING
            value(subrc)     TYPE            sy-subrc,
  
        create_alv
          RETURNING
            value(alv)       TYPE REF TO     cl_salv_table,
  
        download_data
          CHANGING
            c_table          TYPE STANDARD TABLE,
  
        upload_data,
  
        fill_header_line
          CHANGING
            c_table          TYPE STANDARD TABLE,
  
        fill_table
          IMPORTING
            i_path           TYPE rlgrap-filename
          CHANGING
            c_table          TYPE STANDARD TABLE. " qqr tabela com tipo estruturado declarado
  
  
  *--------------------------------------------------------------------
  * Objetos
  *--------------------------------------------------------------------
      DATA:
        co_salv              TYPE REF TO     cl_salv_table,
        co_sel               TYPE REF TO     cl_salv_selections.
  
  
  *--------------------------------------------------------------------
  * Tabelas Internas
  *--------------------------------------------------------------------
      DATA:
        ct_relat             TYPE TABLE OF   relat_type,
        ct_marca             TYPE TABLE OF   marca_ty,
        ct_veiculos          TYPE TABLE OF   veiculo_ty.
  
  *--------------------------------------------------------------------
  * Work areas
  *--------------------------------------------------------------------
      DATA:
        cs_relat             TYPE            relat_type,
        cs_veiculos          TYPE            veiculo_ty,
        cs_marca             TYPE            marca_ty,
        cs_variant           TYPE            disvariant.
  
  ENDCLASS.                    "lcl_relat DEFINITION
  
  
  *----------------------------------------------------------------------
  * Classe relatório Implementação
  *----------------------------------------------------------------------
  CLASS lcl_relat IMPLEMENTATION.
  
    METHOD select_data.
  
      CASE 'X'.
        WHEN p_down.
  
          IF p_rdm IS NOT INITIAL.
            me->fill_header_line(
              CHANGING
                c_table  = ct_marca
            ).
  
            SELECT * FROM ztb_marcas_20 APPENDING CORRESPONDING FIELDS OF TABLE ct_marca.
          ELSE.
            me->fill_header_line(
              CHANGING
                c_table  = ct_veiculos
            ).
  
            SELECT * FROM ztb_veiculos_20 APPENDING CORRESPONDING FIELDS OF TABLE ct_veiculos.
          ENDIF.
  
        WHEN p_uplo.
          me->fill_table(
           EXPORTING
             i_path  = p_marcas
           CHANGING
             c_table = ct_marca
         ).
  
          me->fill_table(
            EXPORTING
              i_path  = p_veicl
            CHANGING
              c_table = ct_veiculos
          ).
  
      ENDCASE.
  
  
  
    ENDMETHOD.                    "select_data
  
    METHOD upload_data.
  
      DATA: lv_icon TYPE string.
  
      MODIFY ztb_marcas_20 FROM TABLE ct_marca.
  
      IF sy-subrc IS INITIAL.
  
        COMMIT WORK.
        CALL FUNCTION 'ICON_CREATE'
          EXPORTING
            name                  = 'ICON_GREEN_LIGHT'
            info                  = 'Sucesso'
          IMPORTING
            result                = lv_icon
          EXCEPTIONS
            icon_not_found        = 1
            outputfield_too_short = 2
            OTHERS                = 3.
        cs_relat-status   = lv_icon.
        cs_relat-mensagem = 'Tabela ZTB_MARCAS_20 atualizada com sucesso'.
  
      ELSE.
  
        ROLLBACK WORK.
        CLEAR lv_icon.
        CALL FUNCTION 'ICON_CREATE'
          EXPORTING
            name                  = 'ICON_RED_LIGHT'
            info                  = 'Erro'
          IMPORTING
            result                = lv_icon
          EXCEPTIONS
            icon_not_found        = 1
            outputfield_too_short = 2
            OTHERS                = 3.
        cs_relat-status   = lv_icon.
        cs_relat-mensagem = 'Tabela ZTB_MARCAS_20 não foi atualizada'.
      ENDIF.
  
      APPEND cs_relat TO ct_relat. CLEAR cs_relat.
  
      MODIFY ztb_veiculos_20 FROM TABLE ct_veiculos.
      IF sy-subrc IS INITIAL.
  
        COMMIT WORK.
        CLEAR lv_icon.
        CALL FUNCTION 'ICON_CREATE'
          EXPORTING
            name                  = 'ICON_GREEN_LIGHT'
            info                  = 'Sucesso'
          IMPORTING
            result                = lv_icon
          EXCEPTIONS
            icon_not_found        = 1
            outputfield_too_short = 2
            OTHERS                = 3.
        cs_relat-status   = lv_icon.
        cs_relat-mensagem = 'Tabela ZTB_VEICULOS_20 atualizada com sucesso'.
      ELSE.
  
        ROLLBACK WORK.
        CLEAR lv_icon.
        CALL FUNCTION 'ICON_CREATE'
          EXPORTING
            name                  = 'ICON_RED_LIGHT'
            info                  = 'Erro'
          IMPORTING
            result                = lv_icon
          EXCEPTIONS
            icon_not_found        = 1
            outputfield_too_short = 2
            OTHERS                = 3.
  
        cs_relat-status   = lv_icon.
        cs_relat-mensagem = 'Tabela ZTB_VEICULOS_20 não foi atualizada'.
      ENDIF.
  
      APPEND cs_relat TO ct_relat. CLEAR cs_relat.
  
    ENDMETHOD.                    "upload_data
  
    METHOD download_data.
  
      DATA: lv_icon     TYPE string,
            lv_filename TYPE string.
  
      IF p_rdm IS NOT INITIAL.
        CONCATENATE p_dir 'ztb_marcas_20.txt' INTO lv_filename SEPARATED BY '\'.
      ELSEIF p_rdv IS NOT INITIAL.
        CONCATENATE p_dir 'ztb_veiculos_20.txt' INTO lv_filename SEPARATED BY '\'.
      ENDIF.
  
      "modificar tamanho e componentes da tabela e passar field symbol
  
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename                = lv_filename
          write_field_separator   = '|'
        CHANGING
          data_tab                = c_table
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          not_supported_by_gui    = 22
          error_no_gui            = 23
          OTHERS                  = 24.
  
      IF sy-subrc IS INITIAL.
  
        CALL FUNCTION 'ICON_CREATE'
          EXPORTING
            name                  = 'ICON_GREEN_LIGHT'
            info                  = 'Sucesso'
          IMPORTING
            result                = lv_icon
          EXCEPTIONS
            icon_not_found        = 1
            outputfield_too_short = 2
            OTHERS                = 3.
  
        cs_relat-status   = lv_icon.
        CONCATENATE 'Arquivo salvo com sucesso em' p_dir INTO cs_relat-mensagem SEPARATED BY space.
        APPEND cs_relat TO ct_relat. CLEAR cs_relat.
  
      ELSE.
  
        CALL FUNCTION 'ICON_CREATE'
          EXPORTING
            name                  = 'ICON_RED_LIGHT'
            info                  = 'Erro'
          IMPORTING
            result                = lv_icon
          EXCEPTIONS
            icon_not_found        = 1
            outputfield_too_short = 2
            OTHERS                = 3.
  
        cs_relat-status   = lv_icon.
        cs_relat-mensagem = 'Arquivo não pode ser salvo'.
        APPEND cs_relat TO ct_relat. CLEAR cs_relat.
  
      ENDIF.
  
  
    ENDMETHOD.                    "download_data
  
    METHOD fill_table.
  
      DATA: lt_file   TYPE TABLE OF string,
            lt_fields TYPE TABLE OF string,
            lv_file   TYPE string.
  
      DATA: ls_file   TYPE string,
            ls_field  TYPE string,
            ls_comp   TYPE abap_compdescr.
  
      DATA: lo_line   TYPE REF TO data,
            lo_struct TYPE REF TO cl_abap_structdescr.
  
      FIELD-SYMBOLS:
          <fs_file>   TYPE any,
          <fs_mandt>  TYPE any,
          <fs_field>  TYPE any.
  
      CHECK i_path IS NOT INITIAL.
  
      lv_file = i_path.
  
      CALL METHOD cl_gui_frontend_services=>gui_upload
        EXPORTING
          filename                = lv_file
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
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
  
      CREATE DATA lo_line LIKE LINE OF c_table.
      ASSIGN lo_line->* TO <fs_file>.
  
      lo_struct ?= cl_abap_typedescr=>describe_by_data( <fs_file> ).
  
      " Header
      DELETE lt_file INDEX 1.
  
      LOOP AT lt_file INTO ls_file. " linhas do arquivo
  
        SPLIT ls_file AT ';' INTO TABLE lt_fields.
  
        LOOP AT lt_fields INTO ls_field. " campos de uma linha
  
          READ TABLE lo_struct->components INTO ls_comp INDEX sy-tabix.
          ASSIGN COMPONENT ls_comp-name OF STRUCTURE <fs_file> TO <fs_field>.
  
          <fs_field> = ls_field.
  
        ENDLOOP.
  
        UNASSIGN <fs_mandt>.
        ASSIGN COMPONENT 'MANDT' OF STRUCTURE <fs_file> TO <fs_mandt>.
        IF <fs_mandt> IS ASSIGNED.
          <fs_mandt> = sy-mandt.
        ENDIF.
        APPEND <fs_file> TO c_table. CLEAR <fs_file>.
  
      ENDLOOP.
  
    ENDMETHOD.                    "fill_table
  
    METHOD fill_header_line.
  
      DATA: lo_struct TYPE REF TO cl_abap_structdescr,
            lo_line   TYPE REF TO data.
  
      DATA: ls_comp   TYPE        abap_compdescr.
  
      FIELD-SYMBOLS:
         <fs_line>    TYPE        any,
         <fs_field>   TYPE        any.
  
      CREATE DATA lo_line LIKE LINE OF c_table.
      ASSIGN lo_line->* TO <fs_line>.
  
      lo_struct ?= cl_abap_typedescr=>describe_by_data( <fs_line> ).
  
      APPEND INITIAL LINE TO c_table ASSIGNING <fs_line>.
  
      LOOP AT lo_struct->components INTO ls_comp.
  
        UNASSIGN <fs_field>.
        ASSIGN COMPONENT ls_comp-name OF STRUCTURE <fs_line> TO <fs_field>.
  
        CASE ls_comp-name.
          WHEN 'MANDT'.
            <fs_field> = 'Mandante'.
          WHEN OTHERS.
            CONDENSE ls_comp-name.
            CALL FUNCTION 'ISP_CONVERT_FIRSTCHARS_TOUPPER' "Caps first letter of word
              EXPORTING
                input_string  = ls_comp-name
              IMPORTING
                output_string = ls_comp-name.
            <fs_field> = ls_comp-name.
        ENDCASE.
      ENDLOOP.
  
    ENDMETHOD.                    "fill_header_line
  
  
    " Tratamento dos dados selecionados
    METHOD processing_data.
  
      IF p_down IS NOT INITIAL.
  
        CHECK p_dir IS NOT INITIAL.
  
        IF p_rdm IS NOT INITIAL.
          me->download_data(
            CHANGING
              c_table = ct_marca
          ).
        ELSEIF p_rdv IS NOT INITIAL.
          me->download_data(
            CHANGING
              c_table = ct_veiculos
          ).
        ENDIF.
  
      ELSE.
        me->upload_data( ).
      ENDIF.
  
  
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
  
          " Set nome colunas
          lo_column   = lo_column_list->get_column( 'STATUS' ).
          lo_column->set_long_text( 'Status' ).
  
          lo_column   = lo_column_list->get_column( 'MENSAGEM' ).
          lo_column->set_long_text( 'Mensagem' ).
  
        CATCH cx_salv_not_found.
      ENDTRY.
  
      " Colunas com somatório
      lo_sum      = co_salv->get_aggregations( ).
  
      " Exibe ALV
      co_salv->display( ).
  
  
    ENDMETHOD.                    "output_data
  
    METHOD check_relat.
      CHECK ct_relat[] IS INITIAL.
      subrc = 4.
      MESSAGE s000(cl) WITH text-e00 DISPLAY LIKE 'E'.
    ENDMETHOD.                    "check_relat
  
    METHOD get_directory.
  
      DATA: lv_dir TYPE string.
  
      CALL METHOD cl_gui_frontend_services=>directory_browse
        CHANGING
          selected_folder      = lv_dir
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4.
  
      IF sy-subrc IS INITIAL.
        r_directory = lv_dir.
      ENDIF.
  
    ENDMETHOD.                    "get_directory
  
  
    METHOD get_file_path.
  
      DATA: lt_filetable TYPE filetable,
            ls_filetable TYPE file_table.
      DATA: lv_rc        TYPE i.
  
      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        CHANGING
          file_table              = lt_filetable
          rc                      = lv_rc
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          not_supported_by_gui    = 4
          OTHERS                  = 5.
  
      IF sy-subrc IS INITIAL.
        CLEAR ls_filetable.
        READ TABLE lt_filetable INTO ls_filetable INDEX 1.
        r_path = ls_filetable-filename.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
  
  
    ENDMETHOD.                    "get_file_path
  
  ENDCLASS.                    "lcl_relat IMPLEMENTATION
  
  *--------------------------------------------------------------------
  * Objetos
  *--------------------------------------------------------------------
  DATA: go_relat            TYPE REF TO     lcl_relat.
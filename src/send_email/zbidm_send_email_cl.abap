*&-----------------------------------------------------------------&*
*                             ClipTech                              *
*&-----------------------------------------------------------------&*
*&---------------------------------------------------------------- *&
* Descrição .: Classe para envio de email com anexo                 *
*&-----------------------------------------------------------------*&
* Objetivo ..:                                                      *
*&-----------------------------------------------------------------*&
*                           HISTÓRICO                               *
*&-----------------------------------------------------------------*&
* DATA       | REQUEST    | RESPONSÁVEL | DESCRIÇÃO                 *
* 13/11/2020 |            | BIMORAIS    |                           *
*&-----------------------------------------------------------------&*

*----------------------------------------------------------------------
* Classe relatório Definição
*----------------------------------------------------------------------
CLASS lcl_email DEFINITION.

    PUBLIC SECTION.
      METHODS:
        select_data
          EXPORTING
            e_out TYPE boolean,
  
        processing_data,
  
        sender_email
          RETURNING
            value(r_sender) TYPE ad_smtpadr,
  
        recipient_email
          RETURNING
            value(r_recipient) TYPE ad_smtpadr,
  
        subject_email
          RETURNING
            value(r_subject) TYPE so_obj_des,
  
        body_email
          RETURNING
            value(r_body) TYPE soli_tab,
  
        get_otf
          RETURNING
            value(r_otf) TYPE tsfotf,
  
        get_excel
          RETURNING
            value(r_excel) TYPE REF TO zcl_excel,
  
        get_attachment
          RETURNING
            value(r_attachment) TYPE sood-objdes,
  
       send_email
        IMPORTING
          i_subject      TYPE so_obj_des
          i_recipient    TYPE ad_smtpadr
          i_sender       TYPE ad_smtpadr       OPTIONAL
          i_body         TYPE soli_tab         OPTIONAL
          i_attachment   TYPE sood-objdes      OPTIONAL
          i_otf          TYPE tsfotf           OPTIONAL
          i_excel        TYPE REF TO zcl_excel OPTIONAL
        EXPORTING
          e_sent         TYPE boolean.
  
  
    PRIVATE SECTION.
  *--------------------------------------------------------------------
  * Tabelas Internas
  *--------------------------------------------------------------------
      DATA:
        ct_veiculo            TYPE TABLE OF   veiculo_ty,
        ct_marca              TYPE TABLE OF   marca_ty,
        ct_relat              TYPE TABLE OF   relat_ty.
  
  *--------------------------------------------------------------------
  * Work areas
  *--------------------------------------------------------------------
      DATA:
        cs_veiculo          TYPE            veiculo_ty,
        cs_marca            TYPE            marca_ty,
        cs_relat            TYPE            relat_ty.
  
  *--------------------------------------------------------------------
  * References
  *--------------------------------------------------------------------
      DATA:
        cx_bcs_exception    TYPE REF TO     cx_bcs.
  
  
  ENDCLASS.                    "lcl_email DEFINITION
  
  
  *----------------------------------------------------------------------
  * Classe relatório Implementação
  *----------------------------------------------------------------------
  CLASS lcl_email IMPLEMENTATION.
  
    " Seleção de dados
    METHOD select_data.
  
      SELECT placa marca modelo ano categoria bloqueado FROM ztb_veiculos_20
        INTO TABLE ct_veiculo.
  
      SELECT marca nome FROM ztb_marcas_20 INTO TABLE ct_marca
       FOR ALL ENTRIES IN ct_veiculo
       WHERE marca = ct_veiculo-marca.
  
      me->processing_data( ).
      " parâmetro que dispara email
      e_out = abap_true.
    ENDMETHOD.                    "select_data
  
    " Processamento dos dados
    METHOD processing_data.
  
      SORT: ct_marca BY marca.
  
      LOOP AT ct_veiculo INTO cs_veiculo.
  
        CLEAR cs_marca.
        READ TABLE ct_marca INTO cs_marca
          WITH KEY marca = cs_veiculo-marca BINARY SEARCH.
  
        cs_relat-marca     = cs_veiculo-marca.
        cs_relat-placa     = cs_veiculo-placa.
        cs_relat-nome      = cs_marca-nome.
        cs_relat-bloqueado = cs_veiculo-bloqueado.
        cs_relat-modelo    = cs_veiculo-modelo.
        APPEND cs_relat TO ct_relat. CLEAR cs_relat.
  
      ENDLOOP.
  
    ENDMETHOD.                    "processing_data
  
    "Remetente
    METHOD sender_email.
      r_sender = 'bimorais@cliptech.com.br'.
    ENDMETHOD.                    "sender_email
  
    "Destinatário
    METHOD recipient_email.
      r_recipient = 'bimorais@cliptech.com.br'.
    ENDMETHOD.                    "recipient_email
  
    "Assunto
    METHOD subject_email.
      r_subject = 'Título do email teste'.
    ENDMETHOD.                    "subject_email
  
    "Corpo
    METHOD body_email.
  
      FIELD-SYMBOLS:
       <body>  TYPE soli.
  
      DATA:
         lt_fcat    TYPE lvc_t_fcat,
         lt_fields  TYPE TABLE OF w3fields,
         lt_header  TYPE TABLE OF w3head,
         lt_html    TYPE TABLE OF w3html.
  
      DATA:
         ls_fcat    TYPE lvc_s_fcat,
         ls_header  TYPE w3head,
         ls_head    TYPE w3head,
         ls_html    TYPE w3html.
  
      ls_fcat-coltext = 'Placa'.
      APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  
      ls_fcat-coltext = 'Marca'.
      APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  
      ls_fcat-coltext = 'Nome'.
      APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  
      ls_fcat-coltext = 'Modelo'.
      APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  
      ls_fcat-coltext = 'Bloqueado?'.
      APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  
      LOOP AT lt_fcat INTO ls_fcat.
  
        ls_head-text = ls_fcat-coltext.
  
        CALL FUNCTION 'WWW_ITAB_TO_HTML_HEADERS'
          EXPORTING
            field_nr = sy-tabix
            text     = ls_head-text
            fgcolor  = 'black'
            bgcolor  = 'yellow'
          TABLES
            header   = lt_header.
  
        CALL FUNCTION 'WWW_ITAB_TO_HTML_LAYOUT'
          EXPORTING
            field_nr = sy-tabix
            fgcolor  = 'black'
            size     = '3'
          TABLES
            fields   = lt_fields.
  
      ENDLOOP.
  
      ls_header-text = 'Tabela de veículos'.
      ls_header-font = 'Arial'.
      ls_header-size = '2'.
  
      CALL FUNCTION 'WWW_ITAB_TO_HTML'
        EXPORTING
          table_header = ls_header
        TABLES
          html         = lt_html
          fields       = lt_fields
          row_header   = lt_header
          itable       = ct_relat.
  
  
      LOOP AT lt_html INTO ls_html.
        APPEND INITIAL LINE TO r_body ASSIGNING <body>.
        <body>-line = ls_html-line.
      ENDLOOP.
  
  
    ENDMETHOD.                    "body_email
  
    METHOD send_email.
  
      DATA: lo_send_request TYPE REF TO cl_bcs,
            lo_document     TYPE REF TO cl_document_bcs,
            lo_sender       TYPE REF TO cl_cam_address_bcs,
            lo_recipient    TYPE REF TO cl_cam_address_bcs.
  
      " Classe excel
      DATA: cl_writer       TYPE REF TO zif_excel_writer.
  
      DATA: it_tcoo         TYPE STANDARD TABLE OF itcoo,
            it_pdf          TYPE STANDARD TABLE OF tline,
            lv_len_in       TYPE sood-objlen            ,
            lv_bin_file     TYPE xstring                ,
            lv_bytecount    TYPE sood-objlen            ,
            lv_sent         TYPE os_boolean             .
  
      DATA  lt_attachment_hex TYPE solix_tab            .
  
  
      TRY .
          " Criação do obj e-mail
          lo_send_request = cl_bcs=>create_persistent( ).
  
          "Remetente
          IF i_sender IS NOT INITIAL.
            lo_sender = cl_cam_address_bcs=>create_internet_address( i_sender ).
            lo_send_request->set_sender( i_sender = lo_sender ).
          ENDIF.
  
          " Destinatário
          lo_recipient = cl_cam_address_bcs=>create_internet_address( i_recipient ).
          lo_send_request->add_recipient(
            EXPORTING
              i_recipient  = lo_recipient
              i_express    = abap_true
          ).
  
          " Corpo email
          lo_document = cl_document_bcs=>create_document(
              i_type         = 'HTM'
              i_subject      = i_subject
              i_length       = '90'
              i_text         = i_body
          ).
  
          " Anexo
          IF p_pdf IS NOT INITIAL.
  
            IF i_otf[] IS NOT INITIAL.
  
              it_tcoo[] = i_otf[].
  
              CALL FUNCTION 'CONVERT_OTF'
                EXPORTING
                  format                = 'PDF'
                  max_linewidth         = 132
                IMPORTING
                  bin_filesize          = lv_len_in
                  bin_file              = lv_bin_file
                TABLES
                  otf                   = it_tcoo
                  lines                 = it_pdf
                EXCEPTIONS
                  err_max_linewidth     = 1
                  err_format            = 2
                  err_conv_not_possible = 3
                  err_bad_otf           = 4
                  OTHERS                = 5.
  
              lt_attachment_hex = cl_bcs_convert=>xstring_to_solix( lv_bin_file ).
  
              lo_document->add_attachment(
               EXPORTING
                 i_attachment_type     = 'PDF'
                 i_attachment_subject  = i_attachment
                 i_att_content_hex     = lt_attachment_hex
             ).
  
            ENDIF.
  
          ELSE.
  
            CREATE OBJECT cl_writer TYPE zcl_excel_writer_2007.
  
            lv_bin_file = cl_writer->write_file( i_excel ).
            lt_attachment_hex = cl_bcs_convert=>xstring_to_solix( iv_xstring = lv_bin_file ).
            lv_bytecount = xstrlen( lv_bin_file ).
  
            lo_document->add_attachment(
             EXPORTING
               i_attachment_type     = 'XLS'
               i_attachment_subject  = i_attachment
               i_att_content_hex     = lt_attachment_hex
               i_attachment_size     = lv_bytecount
           ).
  
          ENDIF.
  
          lo_send_request->set_document( lo_document ).
  
          lv_sent = lo_send_request->send( i_with_error_screen = abap_true ).
  
          IF lv_sent IS NOT INITIAL.
            COMMIT WORK.
            WRITE: 'Email enviado com sucesso'.
          ELSE.
            WRITE: 'Não foi possível enviar o email'.
          ENDIF.
  
        CATCH cx_bcs INTO cx_bcs_exception.
  
      ENDTRY.
  
    ENDMETHOD.                    "send_email
  
    METHOD get_otf.
  
      DATA: lv_fname TYPE  rs38l_fnam.
  
      DATA: ls_opt     TYPE ssfcompop,
            ls_ctrl    TYPE ssfctrlop,
            ls_jobinfo TYPE ssfcrescl.
  
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = 'ZSF_20_VEICULO'
        IMPORTING
          fm_name            = lv_fname
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.
  
      ls_ctrl-getotf    = 'X'.
      ls_ctrl-preview   = 'X'.
      ls_ctrl-no_dialog = 'X'.
      ls_opt-tddest     = 'LOCL'.
      ls_opt-tdnoprint  = 'X'.
  
  
      CALL FUNCTION lv_fname
        EXPORTING
          control_parameters = ls_ctrl
          output_options     = ls_opt
          user_settings      = ' '
        IMPORTING
          job_output_info    = ls_jobinfo
        TABLES
          it_veiculo         = ct_veiculo
          it_marca           = ct_marca
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.
  
      r_otf = ls_jobinfo-otfdata.
  
    ENDMETHOD.                    "get_otf
  
    METHOD get_excel.
  
      DATA:
         lt_field_catalog         TYPE            zexcel_t_fieldcatalog.
  
      DATA:
         lo_excel                 TYPE REF TO     zcl_excel,
         lo_worksheet             TYPE REF TO     zcl_excel_worksheet,
         lo_style                 TYPE REF TO     zcl_excel_style_conditional,
         lo_style_numeric         TYPE REF TO     zcl_excel_style,
         lo_style_total           TYPE REF TO     zcl_excel_style,
         lo_column_dimension      TYPE REF TO     zcl_excel_worksheet_columndime.
  
      DATA:
         ls_iconset               TYPE            zexcel_conditional_iconset.
  
      DATA:
         ls_table_settings        TYPE            zexcel_s_table_settings.
  
      FIELD-SYMBOLS:
         <fs_fcat>                TYPE            zexcel_s_fieldcatalog.
  
      CREATE OBJECT lo_excel.
      lo_worksheet = lo_excel->get_active_worksheet( ).
  
      ls_iconset-iconset                  = zcl_excel_style_conditional=>c_iconset_5arrows.
      ls_iconset-cfvo1_type               = zcl_excel_style_conditional=>c_cfvo_type_percent.
      ls_iconset-cfvo1_value              = '0'.
      ls_iconset-cfvo2_type               = zcl_excel_style_conditional=>c_cfvo_type_percent.
      ls_iconset-cfvo2_value              = '20'.
      ls_iconset-cfvo3_type               = zcl_excel_style_conditional=>c_cfvo_type_percent.
      ls_iconset-cfvo3_value              = '40'.
      ls_iconset-cfvo4_type               = zcl_excel_style_conditional=>c_cfvo_type_percent.
      ls_iconset-cfvo4_value              = '60'.
      ls_iconset-cfvo5_type               = zcl_excel_style_conditional=>c_cfvo_type_percent.
      ls_iconset-cfvo5_value              = '80'.
      ls_iconset-showvalue                = zcl_excel_style_conditional=>c_showvalue_true.
  
      "Conditional style
      lo_style                     = lo_worksheet->add_new_conditional_style( ).
      lo_style->rule               = zcl_excel_style_conditional=>c_rule_iconset.
      lo_style->mode_iconset       = ls_iconset.
      lo_style->priority           = 1.
  
      lo_style_total               = lo_excel->add_new_style( ).
      lo_style_total->number_format->format_code = zcl_excel_style_number_format=>c_format_number_comma_sep2.
      lo_style_total->font->bold   = abap_true.
  
      lo_style_numeric             = lo_excel->add_new_style( ).
      lo_style_numeric->number_format->format_code = zcl_excel_style_number_format=>c_format_number_comma_sep2.
  
      lt_field_catalog = zcl_excel_common=>get_fieldcatalog( ct_relat ).
  
      LOOP AT lt_field_catalog ASSIGNING <fs_fcat>.
  
        <fs_fcat>-position = sy-tabix.
  
        CASE <fs_fcat>-abap_type.
          WHEN 'P'.
            <fs_fcat>-abap_type = cl_abap_typedescr=>typekind_packed.
            <fs_fcat>-style     = lo_style_numeric->get_guid( ).
        ENDCASE.
  
        IF <fs_fcat>-totals_function EQ abap_true.
          <fs_fcat>-totals_function = zcl_excel_table=>totals_function_sum.
          <fs_fcat>-style_total     = lo_style_total->get_guid( ).
        ENDIF.
  
      ENDLOOP.
  
      ls_table_settings-table_style  = zcl_excel_table=>builtinstyle_medium9.
      ls_table_settings-nofilters    = abap_false.
  
      lo_worksheet->bind_table( ip_table = ct_relat
                                is_table_settings = ls_table_settings
                                it_field_catalog  = lt_field_catalog ).
  
      r_excel = lo_excel.
  
    ENDMETHOD.                    "get_excel
  
    METHOD get_attachment.
  
      r_attachment = 'Tabela de veiculos'.
  
    ENDMETHOD.                    "get_attachment
  
  ENDCLASS.                    "lcl_email IMPLEMENTATION
  
  *--------------------------------------------------------------------
  * Objetos
  *--------------------------------------------------------------------
  DATA: go_mail            TYPE REF TO     lcl_email.
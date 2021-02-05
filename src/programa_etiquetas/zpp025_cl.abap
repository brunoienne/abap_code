*&-----------------------------------------------------------------&*
*                             ClipTech                              *
*&-----------------------------------------------------------------&*
* Programa ..: ZPP025                                               *
* Transação..: ZPP002                                               *
* Módulo.....: PP                                                   *
* Especif....:                                                      *
* Funcional..:                                                      *
*&---------------------------------------------------------------- *&
* Descrição .: Nova versão do programa de impressão de etq.[ZPP002] *
*&-----------------------------------------------------------------*&
* Objetivo ..:                                                      *
*&-----------------------------------------------------------------*&
*                           HISTÓRICO                               *
*&-----------------------------------------------------------------*&
* DATA       | REQUEST    | RESPONSÁVEL | DESCRIÇÃO                 *
* 08.12.2020 | ER1K93543  | BIMORAIS    | Impressão de etiquetas    *
*&-----------------------------------------------------------------&*

*----------------------------------------------------------------------
* Classe relatório Definição
*----------------------------------------------------------------------
CLASS lcl_print DEFINITION.
    PUBLIC SECTION.
      METHODS:
        constructor,
  
        select_single_op,
  
        select_multiple_op,
  
        processing_multiple_op,
  
        print_data,
  
        handle_pbo,
  
        set_listbox_values.
  
    PRIVATE SECTION.
      METHODS:
       processing_single_op
          IMPORTING
            i_aufnr         TYPE afpo-aufnr
            i_qtdimpetq     TYPE zqtd_tot
            i_matnr         TYPE matnr
            i_psmng         TYPE co_psmng
            i_atwrt         TYPE atwrt
            i_qtd_etfn      TYPE zqtd_tot,
  
        update_ztb_ficosa,
  
        update_ord_producao,
  
        update_ord_prod_magna,
  
        update_ztb_magna,
  
        update_ztb_pintura,
  
        set_characteristics,
  
        select_reprint_op,
  
        select_reprint_fic,
  
        select_reprint_mag_huf,
  
        print_seoyon,
  
        print_tenpao,
  
        print_ad,
  
        print_epson,
  
        print_ficosa,
  
        print_mag_huf,
  
        print_koito,
  
        print_lg,
  
        print_ord_prod,
  
        print_stanley,
  
        set_range_op,
  
        create_alv
          CHANGING
            c_table         TYPE STANDARD TABLE,
  
        call_form
          IMPORTING
            i_formname     TYPE      tdsfname.
  
  *--------------------------------------------------------------------
  * Atributos Estáticos
  *--------------------------------------------------------------------
      CLASS-DATA:
        cr_aufnr            TYPE RANGE OF aufnr,
        cv_atinn            TYPE ausp-atinn.
  
  *--------------------------------------------------------------------
  * Tabelas Internas
  *--------------------------------------------------------------------
      DATA:
        ct_etiq             TYPE TABLE OF   zst_etiq_lg,
        ct_afpo             TYPE TABLE OF   afpo_type,
        ct_ausp             TYPE TABLE OF   ausp_type.
  
  *--------------------------------------------------------------------
  * Work areas
  *--------------------------------------------------------------------
      DATA:
        cs_etiq             TYPE            zst_etiq_lg,
        cs_afpo             TYPE            afpo_type,
        cs_ausp             TYPE            ausp_type,
        cs_parametros       TYPE            zst_etiquetas.
  
  
  ENDCLASS.                    "lcl_relat DEFINITION
  
  
  *----------------------------------------------------------------------
  * Classe relatório Implementação
  *----------------------------------------------------------------------
  CLASS lcl_print IMPLEMENTATION.
  
    " Construtor
    METHOD constructor.
  
      DATA: lv_input(200)  TYPE c,
           lv_output(200) TYPE c.
  
      lv_input = 'Z_QTDE_CAIXA'.
  
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = lv_input
        IMPORTING
          output = lv_output.
  
      SELECT SINGLE atinn FROM cabn INTO cv_atinn
        WHERE atinn EQ lv_output.
  
    ENDMETHOD.                    "constructor
  
    " Seleção de dados
    METHOD select_single_op.
  
      DATA: lv_matnr       TYPE matnr,
            lv_psmng       TYPE co_psmng,
            lv_atwrt       TYPE atwrt,
            lv_qtimpetq    TYPE p DECIMALS 4,
            lv_qtimpetq_fn TYPE p DECIMALS 4,
            lv_qtd         TYPE i,
            lv_qtdefn      TYPE i.
  
      SELECT SINGLE psmng matnr INTO (lv_psmng, lv_matnr)
        FROM afpo
        WHERE aufnr EQ p_aufnr.
  
      SELECT SINGLE atwrt FROM ausp INTO lv_atwrt
        WHERE objek EQ lv_matnr
          AND atinn EQ cv_atinn.
  
      IF lv_psmng IS INITIAL.
        MESSAGE text-012 TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
      IF lv_atwrt IS INITIAL.
        MESSAGE text-013 TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
      TRANSLATE lv_atwrt USING: '. ',
                                ',.'.
      CONDENSE lv_atwrt NO-GAPS.
  
      lv_qtimpetq    = lv_psmng / lv_atwrt.
      lv_qtimpetq    = trunc( lv_qtimpetq ).
      lv_qtimpetq_fn = lv_psmng - ( lv_qtimpetq * lv_atwrt ).
  
      IF NOT lv_qtimpetq_fn IS INITIAL.
        lv_qtimpetq = lv_qtimpetq + 1.
      ENDIF.
  
      CASE p_etiq.
        WHEN 'SEOYON' OR 'MAGNA' OR 'TENPAO' OR 'AD'.
          lv_qtimpetq = lv_psmng.
      ENDCASE.
  
  
      " Se for reimpressão
      IF p_reimp IS NOT INITIAL AND
        p_nro IS NOT INITIAL.
        lv_qtimpetq = p_nro.
      ENDIF.
  
      IF p_reimp IS NOT INITIAL AND
         p_qtlg IS NOT INITIAL.
        lv_atwrt = p_qtlg.
      ENDIF.
  
      CASE p_etiq.
        WHEN 'LG' OR 'KOITO' OR 'STANLEY' OR 'AD'.
          IF p_reimp IS NOT INITIAL.
            CLEAR lv_qtimpetq_fn.
          ENDIF.
      ENDCASE.
  
      " Set variaveis da classe
      v_psmng        = lv_psmng.
      lv_qtd         = lv_qtimpetq.
      lv_qtdefn      = lv_qtimpetq_fn.
  
      me->processing_single_op(
        EXPORTING
          i_aufnr        = p_aufnr
          i_qtdimpetq    = lv_qtd
          i_matnr        = lv_matnr
          i_psmng        = lv_psmng
          i_atwrt        = lv_atwrt
          i_qtd_etfn     = lv_qtdefn
      ).
  
    ENDMETHOD.                    "select_single_op
  
    METHOD select_multiple_op.
  
      FIELD-SYMBOLS:
          <fs_ausp> TYPE ausp_type.
  
      DATA: lt_afpo TYPE TABLE OF afpo_type,
            lt_ausp TYPE TABLE OF ausp_type.
  
      DATA: lv_psmng TYPE abap_bool,
            lv_atwrt TYPE abap_bool.
  
      me->set_range_op( ).
  
      IF cr_aufnr[] IS INITIAL.
        MESSAGE text-011 TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
      SELECT aufnr psmng matnr INTO TABLE ct_afpo
        FROM afpo
        WHERE aufnr IN cr_aufnr.
  
      CHECK ct_afpo[] IS NOT INITIAL.
  
      lt_afpo[] = ct_afpo[].
      SORT lt_afpo BY aufnr.
      DELETE ADJACENT DUPLICATES FROM lt_afpo COMPARING matnr.
  
      LOOP AT lt_afpo INTO cs_afpo.
        cs_ausp-objek = cs_afpo-matnr.
        APPEND cs_ausp TO lt_ausp. CLEAR cs_ausp.
      ENDLOOP.
  
      LOOP AT ct_afpo INTO cs_afpo.
        IF cs_afpo-psmng IS INITIAL.
          lv_psmng = abap_true.
        ENDIF.
      ENDLOOP.
  
      SELECT objek atwrt FROM ausp INTO TABLE ct_ausp
        FOR ALL ENTRIES IN lt_ausp
        WHERE objek EQ lt_ausp-objek
          AND atinn EQ cv_atinn.
  
      LOOP AT ct_ausp ASSIGNING <fs_ausp>.
        TRANSLATE <fs_ausp>-atwrt USING: '. ',
                                         ',.'.
        CONDENSE <fs_ausp>-atwrt NO-GAPS.
        IF <fs_ausp>-atwrt IS INITIAL.
          lv_atwrt = abap_true.
        ENDIF.
  
      ENDLOOP.
  
      IF lv_psmng EQ abap_true.
        MESSAGE text-012 TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
      IF lv_atwrt EQ abap_true.
        MESSAGE text-013 TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
    ENDMETHOD.                    "select_multiple_op
  
    METHOD processing_multiple_op.
  
      DATA: lv_qtimpetq    TYPE p DECIMALS 4,
            lv_qtimpetq_fn TYPE p DECIMALS 4.
  
      SORT ct_ausp BY objek.
  
      LOOP AT ct_afpo INTO cs_afpo.
  
        CLEAR cs_ausp.
        READ TABLE ct_ausp INTO cs_ausp
          WITH KEY objek = cs_afpo-matnr BINARY SEARCH.
  
        CHECK sy-subrc IS INITIAL.
  
        cs_etiq-aufnr = cs_afpo-aufnr.
        cs_etiq-matnr = cs_afpo-matnr.
        cs_etiq-psmng = cs_afpo-psmng.
        cs_etiq-atwrt = cs_ausp-atwrt.
  
        lv_qtimpetq    = cs_afpo-psmng / cs_ausp-atwrt.
        lv_qtimpetq    = trunc( lv_qtimpetq ).
        lv_qtimpetq_fn = cs_afpo-psmng - ( lv_qtimpetq * cs_ausp-atwrt ).
  
        cs_etiq-qtd_tot_etiq = lv_qtimpetq.
        cs_etiq-qtd_etfn     = lv_qtimpetq_fn.
  
        IF cs_etiq-qtd_tot_etiq IS INITIAL.
          cs_etiq-qtd_tot_etiq = 1.
        ENDIF.
  
        IF p_reimp IS NOT INITIAL AND p_nro IS NOT INITIAL.
          cs_etiq-qtd_tot_etiq = p_nro.
        ENDIF.
  
        IF p_reimp IS NOT INITIAL AND p_qtlg IS NOT INITIAL.
          cs_etiq-qtd_etfn   = p_qtlg.
        ENDIF.
        APPEND cs_etiq TO ct_etiq. CLEAR cs_etiq.
  
      ENDLOOP.
  
    ENDMETHOD.                    "processing_multiple_op
  
    METHOD processing_single_op.
  
      DATA: lv_count TYPE i.
  
      DO i_qtdimpetq TIMES.
        lv_count             = lv_count + 1.
        cs_etiq-aufnr        = i_aufnr.
        cs_etiq-nro_etiq     = lv_count.
        cs_etiq-qtd_tot_etiq = i_qtdimpetq.
        cs_etiq-matnr        = i_matnr.
        cs_etiq-psmng        = i_psmng.
        cs_etiq-atwrt        = i_atwrt.
        IF lv_count = i_qtdimpetq.
          cs_etiq-qtd_etfn = i_qtd_etfn.
        ENDIF.
        APPEND cs_etiq TO ct_etiq. CLEAR cs_etiq.
      ENDDO.
  
    ENDMETHOD.                    "processing_single_op
  
    METHOD print_data.
  
      CASE p_etiq.
        WHEN 'AD'.
          me->print_ad( ).
  
        WHEN 'EPSON'.
          me->print_epson( ).
  
        WHEN 'FICOSA'.
          me->print_ficosa( ).
  
        WHEN 'HUF' OR 'MAGNA'.
          me->print_mag_huf( ).
  
        WHEN 'KOITO'.
          me->print_koito( ).
  
        WHEN 'LG'.
          me->print_lg( ).
  
        WHEN 'OP' OR 'OPKOI' OR 'OPMAG' OR 'OPPIN'.
          me->print_ord_prod( ).
  
        WHEN 'SEOYON'.
          me->print_seoyon( ).
          RETURN.
  
        WHEN 'STANLEY'.
          me->print_stanley( ).
  
        WHEN 'TENPAO'.
          me->print_tenpao( ).
          RETURN.
  
      ENDCASE.
  
    ENDMETHOD.                    "print_data
  
    METHOD create_alv.
  
      DATA: lt_fcat    TYPE slis_t_fieldcat_alv.
      DATA: ls_layout  TYPE slis_layout_alv,
            ls_grid    TYPE lvc_s_glay.
      DATA: lv_command TYPE slis_formname.
  
      ls_layout-zebra             = 'X'.
      ls_layout-colwidth_optimize = 'X'.
      ls_layout-box_fieldname     = 'SEL'.
      ls_grid-edt_cll_cb          = 'X'.
  
      CASE p_etiq.
        WHEN 'OP' OR 'OPKOI' OR 'OPPIN' OR 'OPMAG' OR 'EPSON'.
          PERFORM zf_build_fcat USING:
            'AUFNR'   'AUFNR'   'ZCONTROL_ETQ' ' ' CHANGING lt_fcat,
            'VOL_DE'  'VOL_DE'  'ZCONTROL_ETQ' ' ' CHANGING lt_fcat,
            'VOL_ATE' 'VOL_ATE' 'ZCONTROL_ETQ' ' ' CHANGING lt_fcat,
            'MATNR'   'MATNR'   'ZCONTROL_ETQ' ' ' CHANGING lt_fcat,
            'QTDE'    'QTDE'    'ZCONTROL_ETQ' 'X' CHANGING lt_fcat.
  
          IF p_etiq EQ 'OPPIN' OR p_etiq EQ 'OPMAG'.
            PERFORM zf_build_fcat USING:
            'DTIMP'   'DTIMP'   'ZCONTROL_ETQ_PIN' ' ' CHANGING lt_fcat.
          ENDIF.
  
          lv_command = 'USER_COMMAND_OP'.
  
        WHEN 'FICOSA'.
          PERFORM zf_build_fcat USING:
           'AUFNR'   'AUFNR'   'ZTB_ETIQ_FIC' ' ' CHANGING lt_fcat,
           'MATNR'   'MATNR'   'ZTB_ETIQ_FIC' ' ' CHANGING lt_fcat,
           'DTIMP'   'DTIMP'   'ZTB_ETIQ_FIC' ' ' CHANGING lt_fcat,
           'QTD_EFN' 'QTD_EFN' 'ZTB_ETIQ_FIC' 'X' CHANGING lt_fcat.
          lv_command = 'USER_COMMAND_FIC'.
  
        WHEN 'MAGNA' OR 'HUF'.
          PERFORM zf_build_fcat USING:
          'MATNR'   'MATNR'   'ZTB_ETIQ_MAGNA' ' ' CHANGING lt_fcat,
          'AUFNR'   'AUFNR'   'ZTB_ETIQ_MAGNA' ' ' CHANGING lt_fcat,
          'RODADA'  'RODADA'  'ZTB_ETIQ_MAGNA' ' ' CHANGING lt_fcat,
          'SEQ'     'SEQ'     'ZTB_ETIQ_MAGNA' ' ' CHANGING lt_fcat,
          'PARTN'   'PARTN'   'ZTB_ETIQ_MAGNA' ' ' CHANGING lt_fcat,
          'COR'     'COR'     'ZTB_ETIQ_MAGNA' ' ' CHANGING lt_fcat,
          'LADO'    'LADO'    'ZTB_ETIQ_MAGNA' ' ' CHANGING lt_fcat,
          'MODELO'  'MODELO'  'ZTB_ETIQ_MAGNA' ' ' CHANGING lt_fcat,
          'QTD_OP'  'QTD_OP'  'ZTB_ETIQ_MAGNA' ' ' CHANGING lt_fcat,
          'DTIMP'   'DTIMP'   'ZTB_ETIQ_MAGNA' ' ' CHANGING lt_fcat.
          lv_command = 'USER_COMMAND_MAG'.
      ENDCASE.
  
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program       = sy-repid
          i_callback_pf_status_set = 'ZPP002'
          i_callback_user_command  = lv_command
          is_layout                = ls_layout
          it_fieldcat              = lt_fcat[]
          i_grid_settings          = ls_grid
        TABLES
          t_outtab                 = c_table.
  
    ENDMETHOD.                    "create_alv
  
    METHOD handle_pbo.
  
      LOOP AT SCREEN.
  
        CASE screen-group1.
          WHEN 'FLI'.
            IF p_etiq EQ 'OPMAG' OR
               p_etiq EQ 'OPPIN' OR
               p_etiq EQ 'OP'.
              screen-active = 1.
            ELSE.
              screen-active = 0.
            ENDIF.
  
          WHEN 'CCT'.
            IF p_etiq EQ 'OP'.
              screen-active = 1.
            ELSE.
              screen-active = 0.
              CLEAR p_cct.
            ENDIF.
  
          WHEN 'STA'.
            IF p_etiq EQ 'STANLEY'.
              screen-active = 1.
            ELSE.
              screen-active = 0.
            ENDIF.
  
          WHEN 'NOP'.
            IF p_etiq EQ 'STANLEY'.
              screen-input = 0.
            ELSE.
              CLEAR: p_op1, p_op2, p_op3, p_op4.
              screen-active = 1.
            ENDIF.
  
          WHEN 'ZEB'.
            IF p_etiq EQ 'SEOYON'.
              screen-active = 1.
            ELSE.
              screen-active = 0.
            ENDIF.
  
          WHEN 'MAG'.
            screen-active = 0.
            IF p_etiq EQ 'MAGNA' OR
               p_etiq EQ 'HUF'.
              IF p_reimp IS NOT INITIAL.
                screen-active = 1.
              ENDIF.
            ENDIF.
  
            IF p_etiq EQ 'MAGNA'.
              text = text-b05.
            ENDIF.
  
            IF p_etiq EQ 'HUF'.
              text = text-b06.
            ENDIF.
  
          WHEN 'MGN'.
            screen-active = 0.
            IF p_etiq EQ 'MAGNA' OR
               p_etiq EQ 'HUF'.
              IF p_reimp IS INITIAL.
                screen-active = 1.
              ENDIF.
            ENDIF.
  
            IF p_etiq EQ 'MAGNA'.
              text2 = text-b09.
            ENDIF.
  
            IF p_etiq EQ 'HUF'.
              text2 = text-b10.
            ENDIF.
  
          WHEN 'SEY'.
            screen-active = 0.
            IF p_etiq EQ 'SEOYON' AND p_reimp IS NOT INITIAL.
              screen-active = 1.
            ENDIF.
  
          WHEN 'TEN'.
            screen-active = 0.
            IF p_etiq EQ 'TENPAO' AND p_reimp IS NOT INITIAL.
              screen-active = 1.
            ENDIF.
  
        ENDCASE.
  
        MODIFY SCREEN.
      ENDLOOP.
  
    ENDMETHOD.                    "handle_pbo
  
  
    METHOD set_listbox_values.
  
      DATA: lt_list TYPE vrm_values,
            ls_list TYPE vrm_value.
  
      DATA: lt_etiq TYPE TABLE OF ztb_etiq_impr,
            ls_etiq TYPE ztb_etiq_impr.
  
      SELECT * FROM ztb_etiq_impr INTO TABLE lt_etiq
        WHERE ativo EQ abap_true.
  
      LOOP AT lt_etiq INTO ls_etiq.
        ls_list-key  = ls_etiq-codigo.
        ls_list-text = ls_etiq-descricao.
        APPEND ls_list TO lt_list. CLEAR ls_list.
      ENDLOOP.
  
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = 'P_ETIQ'
          values = lt_list.
  
    ENDMETHOD.                    "set_listbox_values
  
    METHOD set_range_op.
  
      FIELD-SYMBOLS:
        <fs_value>   TYPE any,
        <fs_line>    LIKE LINE OF cr_aufnr.
  
      DATA: lv_param TYPE string,
            lv_index TYPE string.
  
      DO 4 TIMES.
        lv_index = sy-index.
        CONCATENATE 'P_OP' lv_index INTO lv_param.
  
        ASSIGN (lv_param) TO <fs_value>.
        CHECK <fs_value> IS ASSIGNED AND <fs_value> IS NOT INITIAL.
  
        APPEND INITIAL LINE TO cr_aufnr ASSIGNING <fs_line>.
        <fs_line>-sign   = 'I'.
        <fs_line>-option = 'EQ'.
        <fs_line>-low    = <fs_value>.
      ENDDO.
  
    ENDMETHOD.                    "set_range_op
  
  
    METHOD call_form.
  
      DATA: ssfcompop TYPE ssfcompop,
            ssfctrlop TYPE ssfctrlop.
  
      DATA: lv_funcao TYPE rs38l_fnam.
  
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = i_formname
        IMPORTING
          fm_name            = lv_funcao
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
  
      ssfcompop-tddest    = 'LOCL'.
      ssfcompop-tdimmed   = 'X'.
      ssfcompop-tdnewid   = 'X'.
      ssfcompop-tddelete  = 'X'.
      ssfctrlop-device    = 'PRINTER'.
      ssfctrlop-no_dialog = ''.
  
      CALL FUNCTION lv_funcao
        EXPORTING
          user_settings      = space
          control_parameters = ssfctrlop
          output_options     = ssfcompop
          i_cct              = p_cct
          i_parametros       = cs_parametros
        TABLES
          ti_etiq_lg         = ct_etiq
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
  
    ENDMETHOD.                    "call_form
  
    METHOD update_ztb_ficosa.
  
      FIELD-SYMBOLS:
         <fs_etiq> TYPE zst_etiq_lg.
  
      DATA: ls_fic TYPE ztb_etiq_fic.
  
      LOOP AT ct_etiq ASSIGNING <fs_etiq>.
        <fs_etiq>-kanban = sy-datum.
        ls_fic-mandt     = sy-mandt.
        ls_fic-aufnr     = <fs_etiq>-aufnr.
        ls_fic-matnr     = <fs_etiq>-matnr.
        ls_fic-qtd       = <fs_etiq>-atwrt.
        ls_fic-qtd_efn   = <fs_etiq>-qtd_etfn.
        ls_fic-dtimp     = sy-datum.
        MODIFY ztb_etiq_fic FROM ls_fic.
      ENDLOOP.
  
    ENDMETHOD.                    "update_ztb_ficosa
  
    METHOD update_ord_prod_magna.
  
      FIELD-SYMBOLS:
       <fs_etiq> TYPE zst_etiq_lg.
  
      DATA: ls_mag TYPE zcontrol_etq_mag.
  
      IF p_flib IS NOT INITIAL.
        cs_parametros-message = text-002.
      ENDIF.
  
      LOOP AT ct_etiq ASSIGNING <fs_etiq>.
        <fs_etiq>-kanban = sy-datum.
        ls_mag-aufnr     = p_aufnr.
        ls_mag-matnr     = <fs_etiq>-matnr.
        ls_mag-qtde      = <fs_etiq>-atwrt.
        IF <fs_etiq>-qtd_etfn IS NOT INITIAL.
          ls_mag-qtde    = <fs_etiq>-qtd_etfn.
        ENDIF.
        ls_mag-vol_de    = <fs_etiq>-nro_etiq.
        ls_mag-vol_ate   = <fs_etiq>-qtd_tot_etiq.
        ls_mag-dtimp     = sy-datum.
  
        MODIFY zcontrol_etq_mag FROM ls_mag.
      ENDLOOP.
  
    ENDMETHOD.                    "update_ord_prod_magna
  
    METHOD update_ztb_magna.
  
      DATA: lv_caracteristica TYPE ausp-atinn.
  
      DATA: lt_magna     TYPE TABLE OF ztb_etiq_magna,
            lt_magna_aux TYPE TABLE OF ztb_etiq_magna.
  
      DATA: ls_magna     TYPE ztb_etiq_magna.
  
      SELECT * FROM ztb_etiq_magna INTO TABLE lt_magna_aux
        WHERE aufnr EQ p_aufnr
        AND rodada  EQ p_rodada.
  
      IF lt_magna_aux[] IS NOT INITIAL.
        MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
      CLEAR cs_etiq.
      READ TABLE ct_etiq INTO cs_etiq INDEX 1.
  
      REFRESH ct_etiq.
  
      DO p_qtop TIMES.
  
        ls_magna-mandt       = sy-mandt.
        ls_magna-matnr       = cs_etiq-matnr.
        ls_magna-aufnr       = cs_etiq-aufnr.
        ls_magna-dtimp       = sy-datum.
        ls_magna-qtd_op      = p_qtop.
        ls_magna-rodada      = p_rodada.
        ls_magna-seq         = sy-index.
  
        cs_etiq-qtd_tot_etiq = p_qtop.
        cs_etiq-atwrt        = p_rodada.
        cs_etiq-kanban       = sy-datum.
        cs_etiq-nro_etiq     = sy-index.
  
        " Cor
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'ZMAT_COR'
          IMPORTING
            output = lv_caracteristica.
  
        SELECT SINGLE atwrt FROM ausp
          INTO ls_magna-cor
          WHERE objek EQ cs_etiq-matnr
            AND atinn EQ lv_caracteristica
            AND mafid EQ 'O'
            AND klart EQ '001'.
  
        "Lado
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'ZMAT_LADO'
          IMPORTING
            output = lv_caracteristica.
  
        SELECT SINGLE atwrt FROM ausp
          INTO ls_magna-lado
          WHERE objek EQ cs_etiq-matnr
            AND atinn EQ lv_caracteristica
            AND mafid EQ 'O'
            AND klart EQ '001'.
  
        "Partnumber
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'ZMAT_COD_CLIENTE'
          IMPORTING
            output = lv_caracteristica.
  
        SELECT SINGLE atwrt FROM ausp
          INTO ls_magna-partn
          WHERE objek EQ cs_etiq-matnr
            AND atinn EQ lv_caracteristica
            AND mafid EQ 'O'
            AND klart EQ '001'.
  
        "Modelo
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'ZMAT_MODELO'
          IMPORTING
            output = lv_caracteristica.
  
        SELECT SINGLE atwrt FROM ausp
          INTO ls_magna-modelo
          WHERE objek EQ cs_etiq-matnr
            AND atinn EQ lv_caracteristica
            AND mafid EQ 'O'
            AND klart EQ '001'.
  
        "Projeto
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'ZMAT_PROJETO'
          IMPORTING
            output = lv_caracteristica.
  
        SELECT SINGLE atwrt FROM ausp
          INTO ls_magna-projeto
          WHERE objek EQ cs_etiq-matnr
            AND atinn EQ lv_caracteristica
            AND mafid EQ 'O'
            AND klart EQ '001'.
  
        APPEND cs_etiq  TO ct_etiq.
        APPEND ls_magna TO lt_magna. CLEAR ls_magna.
  
      ENDDO.
  
      INSERT ztb_etiq_magna FROM TABLE lt_magna.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.
        MESSAGE s000(cl) WITH text-018.
      ENDIF.
  
  
  
    ENDMETHOD.                    "update_ztb_magna
  
    METHOD update_ord_producao.
  
      DATA: lt_zctrlet_aux TYPE TABLE OF zcontrol_etq,
            ls_zctrlet_aux TYPE zcontrol_etq.
  
      SELECT * FROM zcontrol_etq INTO TABLE lt_zctrlet_aux
           WHERE aufnr = p_aufnr.
  
      IF sy-subrc = 0.
        LOOP AT lt_zctrlet_aux INTO ls_zctrlet_aux.
          DELETE zcontrol_etq FROM ls_zctrlet_aux.
        ENDLOOP.
      ENDIF.
  
      CLEAR: lt_zctrlet_aux[], ls_zctrlet_aux.
  
      LOOP AT ct_etiq INTO cs_etiq.
        ls_zctrlet_aux-aufnr   = p_aufnr.
        ls_zctrlet_aux-matnr   = cs_etiq-matnr.
        ls_zctrlet_aux-qtde    = cs_etiq-atwrt.
        IF cs_etiq-qtd_etfn IS NOT INITIAL.
          ls_zctrlet_aux-qtde  = cs_etiq-qtd_etfn.
        ENDIF.
        ls_zctrlet_aux-vol_de  = cs_etiq-nro_etiq.
        ls_zctrlet_aux-vol_ate = cs_etiq-qtd_tot_etiq.
        APPEND ls_zctrlet_aux TO lt_zctrlet_aux.
      ENDLOOP.
  
      INSERT zcontrol_etq FROM TABLE lt_zctrlet_aux.
  
    ENDMETHOD.                    "update_ord_producao
  
    METHOD update_ztb_pintura.
  
      FIELD-SYMBOLS:
          <fs_etiq> TYPE zst_etiq_lg.
  
      DATA: ls_pin TYPE zcontrol_etq_pin.
  
      IF p_flib IS NOT INITIAL.
        cs_parametros-message = text-002.
      ENDIF.
  
      LOOP AT ct_etiq ASSIGNING <fs_etiq>.
        <fs_etiq>-kanban   = sy-datum.
        ls_pin-aufnr       = p_aufnr.
        ls_pin-matnr       = <fs_etiq>-matnr.
        ls_pin-qtde        = <fs_etiq>-atwrt.
        IF <fs_etiq>-qtd_etfn IS NOT INITIAL.
          ls_pin-qtde      = <fs_etiq>-qtd_etfn.
        ENDIF.
        ls_pin-vol_de      = <fs_etiq>-nro_etiq.
        ls_pin-vol_ate     = <fs_etiq>-qtd_tot_etiq.
        ls_pin-dtimp       = sy-datum.
        MODIFY zcontrol_etq_pin FROM ls_pin.
      ENDLOOP.
  
    ENDMETHOD.                    "update_ztb_pintura
  
    METHOD set_characteristics.
  
      DATA: lt_etiq   TYPE TABLE OF zst_etiq_lg,
            lt_auspx  TYPE TABLE OF auspx_type,
            lt_ausp   TYPE TABLE OF ausp.
  
      DATA: ls_etiq   TYPE zst_etiq_lg,
            ls_auspx  TYPE auspx_type,
            ls_ausp   TYPE ausp.
  
      DATA: lv_kanban TYPE ausp-atinn.
  
      CHECK ct_etiq[] IS NOT INITIAL.
  
      lt_etiq[] = ct_etiq[].
      SORT lt_etiq BY matnr.
      DELETE ADJACENT DUPLICATES FROM lt_etiq COMPARING matnr.
  
      LOOP AT lt_etiq INTO ls_etiq.
        ls_auspx-matnr = ls_etiq-matnr.
        APPEND ls_auspx TO lt_auspx. CLEAR ls_auspx.
      ENDLOOP.
  
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = 'ZMAT_KANBAN'
        IMPORTING
          output = lv_kanban.
  
      SELECT * FROM ausp INTO TABLE lt_ausp
        FOR ALL ENTRIES IN lt_auspx
        WHERE objek EQ lt_auspx-matnr
          AND atinn EQ lv_kanban.
  
      CLEAR ls_etiq.
      LOOP AT lt_ausp INTO ls_ausp.
        ls_etiq-kanban  = ls_ausp-atwrt.
        MODIFY ct_etiq FROM ls_etiq TRANSPORTING kanban WHERE matnr EQ ls_ausp-objek.
      ENDLOOP.
  
      cs_parametros-caracteristica  = 'ZMAT_KANBAN'.
  
    ENDMETHOD.                    "set_characteristics
  
    METHOD select_reprint_op.
  
      REFRESH t_op.
  
      CASE p_etiq.
        WHEN 'OP' OR 'OPKOI' OR 'EPSON'.
          SELECT * FROM zcontrol_etq INTO CORRESPONDING FIELDS OF TABLE t_op
            WHERE aufnr EQ p_aufnr.
        WHEN 'OPPIN'.
          SELECT *  FROM zcontrol_etq_pin INTO CORRESPONDING FIELDS OF TABLE t_op
            WHERE aufnr EQ p_aufnr.
        WHEN 'OPMAG'.
          SELECT * FROM zcontrol_etq_mag INTO CORRESPONDING FIELDS OF TABLE t_op
            WHERE aufnr EQ p_aufnr.
      ENDCASE.
  
      IF t_op[] IS INITIAL.
        MESSAGE text-014 TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
  
    ENDMETHOD.                    "select_reprint_op
  
    METHOD select_reprint_fic.
  
      REFRESH t_ficosa.
  
      SELECT * FROM ztb_etiq_fic INTO CORRESPONDING FIELDS OF TABLE t_ficosa
         WHERE aufnr EQ p_aufnr.
  
      IF t_ficosa[] IS INITIAL.
        MESSAGE text-014 TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
  
    ENDMETHOD.                    "select_reprint_fic
  
    METHOD select_reprint_mag_huf.
  
      REFRESH t_magna.
  
      SELECT * FROM ztb_etiq_magna INTO TABLE t_magna
       WHERE aufnr  EQ p_aufnr
         AND dtimp  IN s_dat
         AND seq    IN s_seqm
         AND rodada IN s_rod.
  
      SORT t_magna BY dtimp rodada seq.
  
      IF t_magna[] IS INITIAL.
        MESSAGE s000(cl) WITH text-015 DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
    ENDMETHOD.                    "select_reprint_mag_huf
  
    METHOD print_seoyon.
  
      DATA: lo_seoyon TYPE REF TO zcl_seyon.
  
      CREATE OBJECT lo_seoyon.
  
      lo_seoyon->set_printer( p_print ).
      lo_seoyon->set_darkness( p_darkn ).
      lo_seoyon->set_print_speed( p_prts ).
      lo_seoyon->set_slew_speed( p_slws ).
  
      IF p_reimp IS INITIAL.
        lo_seoyon->print_seoyon_label( ct_etiq[] ).
      ELSE.
        lo_seoyon->reprint_seoyon_label(
          EXPORTING
            i_aufnr = p_aufnr
            i_datum = p_datum
            i_seq   = s_seq[]
        ).
      ENDIF.
  
    ENDMETHOD.                    "print_seoyon
  
    METHOD print_tenpao.
  
      DATA: lo_tenpao TYPE REF TO zcl_tenpao.
  
      CREATE OBJECT lo_tenpao.
  
      lo_tenpao->set_darkness( p_darkn ).
  
      IF p_reimp IS INITIAL.
        lo_tenpao->print_label( ct_etiq[] ).
      ELSE.
        lo_tenpao->reprint_label(
          EXPORTING
            i_aufnr = p_aufnr
            i_datum = p_datten
            i_seqnr = s_seqnr[]
        ).
      ENDIF.
  
    ENDMETHOD.                    "print_tenpao
  
    METHOD print_ad.
  
      IF p_reimp EQ 'X' AND
         p_nro IS INITIAL.
        MESSAGE text-019 TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ELSE.
        me->call_form( 'ZSFPP_ETIQ_AD' ).
      ENDIF.
  
    ENDMETHOD.                    "print_ad
  
    METHOD print_epson.
  
      cs_parametros-ic_epson = abap_true.
      IF p_reimp IS INITIAL.
        me->call_form( 'ZWM_ETIQ_IC' ).
      ELSE.
        me->select_reprint_op( ).
        me->create_alv(
          CHANGING
            c_table = t_op
        ).
      ENDIF.
  
    ENDMETHOD.                    "print_epson
  
    METHOD print_ficosa.
  
      IF p_reimp IS INITIAL.
        cs_parametros-name    = 'FICOSA DO BRASIL LTDA'.
        cs_parametros-message = text-002.
        me->update_ztb_ficosa( ).
        me->call_form( 'ZWM_ETIQ_LG' ).
      ELSE.
        me->select_reprint_fic( ).
        me->create_alv(
          CHANGING
            c_table       = t_ficosa
        ).
      ENDIF.
  
    ENDMETHOD.                    "print_ficosa
  
    METHOD print_mag_huf.
  
      IF p_reimp IS INITIAL.
        me->update_ztb_magna( ).
        CASE p_etiq.
          WHEN 'MAGNA'.
            me->call_form( 'ZWM_ETIQ_MAGNA' ).
          WHEN 'HUF'.
            me->call_form( 'ZWM_ETIQ_HUF' ).
        ENDCASE.
      ELSE.
        me->select_reprint_mag_huf( ).
        me->create_alv(
          CHANGING
            c_table       = t_magna
        ).
      ENDIF.
  
    ENDMETHOD.                    "print_mag_huf
  
    METHOD print_koito.
  
      IF p_reimp EQ 'X' AND
         p_nro IS INITIAL.
        MESSAGE text-019 TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ELSE.
        cs_parametros-name              = 'NAL DO BRASIL'.
        cs_parametros-print_etiq_num    = abap_true.
        me->set_characteristics( ).
        me->call_form( 'ZWM_ETIQ_LG_QR' ).
      ENDIF.
  
    ENDMETHOD.                    "print_koito
  
    METHOD print_lg.
  
      IF p_reimp EQ 'X' AND
         p_nro IS INITIAL.
        MESSAGE text-019 TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ELSE.
        cs_parametros-name = 'LG ELECTRONICS DO BRASIL'.
        me->call_form( 'ZWM_ETIQ_LG' ).
      ENDIF.
  
    ENDMETHOD.                    "print_lg
  
    METHOD print_ord_prod.
  
      IF p_flib EQ 'X'.
        cs_parametros-message = text-002.
      ENDIF.
  
      IF p_reimp IS INITIAL.
        CASE p_etiq.
          WHEN 'OPKOI'.
            me->update_ord_producao( ).
            me->call_form( 'ZWM_ETIQ_ORD_KOITO' ).
          WHEN 'OP'.
            me->update_ord_producao( ).
            me->call_form( 'ZWM_ETIQ_ORD' ).
          WHEN 'OPPIN'.
            me->update_ztb_pintura( ).
            me->call_form( 'ZWM_ETIQ_ORD_PINTURA' ).
          WHEN 'OPMAG'.
            me->update_ord_prod_magna( ).
            me->call_form( 'ZWM_ETIQ_ORD_MAGNA' ).
        ENDCASE.
      ELSE.
        me->select_reprint_op( ).
        me->create_alv(
          CHANGING
            c_table       = t_op
        ).
      ENDIF.
  
    ENDMETHOD.                    "print_ord_prod
  
    METHOD print_stanley.
  
      IF p_reimp EQ 'X' AND
         p_nro IS INITIAL.
        MESSAGE text-019 TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ELSE.
        cs_parametros-print_etiq_num    = abap_true.
        me->set_characteristics( ).
        me->call_form( 'ZWM_ETIQ_STANLEY_MATRIZ' ).
      ENDIF.
  
    ENDMETHOD.                    "print_stanley
  
  
  ENDCLASS.                    "lcl_relat IMPLEMENTATION
  
  *--------------------------------------------------------------------
  * Objetos
  *--------------------------------------------------------------------
  DATA: go_print            TYPE REF TO     lcl_print,
        go_tenpao           TYPE REF TO     zcl_tenpao.
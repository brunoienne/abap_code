*&---------------------------------------------------------------------*
*&  Include           ZWM_ETIQUETAS_F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PF_IMPRIMIR_ETIQUETA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_imprimir_etiqueta .


  DATA:
    lo_magna              TYPE REF TO     zcl_magna,
    lo_seyon              TYPE REF TO     zcl_seyon.


  DATA: it_zctrlet_aux TYPE TABLE OF zcontrol_etq,
        wa_zctrlet_aux TYPE zcontrol_etq.


  IF p_reimp IS INITIAL.
    IF gv_psmng = 0.
      MESSAGE 'Ordem sem quantidade.' TYPE 'I' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ELSE.
    IF p_etiq IS INITIAL AND p_orp <> 'X' AND
        p_seoyon IS INITIAL               AND
        p_tenpao IS INITIAL               AND
        p_magna  IS INITIAL               AND " >>>BIDM 16.01.2020
        p_pin    IS INITIAL.                  " >>>BIDM 30.01.2020
      MESSAGE 'Para re-impressão, informar a quantidade de etiquetas' TYPE 'I' DISPLAY LIKE 'E'.
      STOP.
    ELSE.
      IF p_orp = 'X' OR p_pin = 'X'.
        PERFORM get_data_alv.

        IF t_final[] IS INITIAL.
          MESSAGE 'Dados anteriores inexistentes para re-impressão desta ordem de produção.' TYPE 'I' DISPLAY LIKE 'E'.
          STOP.
        ENDIF.

        IF p_flib IS NOT INITIAL.
          gs_parametros-message = text-m01.
        ENDIF.

        PERFORM carrega_catalogo.

        t_layout-zebra  = 'X'.
        t_layout-box_fieldname     = 'SEL'.
*        t_layout-edit       = abap_true.

        ls_grid_settings-edt_cll_cb = abap_true.

        CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
            i_callback_program       = sy-repid
            i_callback_user_command  = 'USER_COMMAND'
            i_callback_pf_status_set = 'PF'
*           i_background_id          = 'ALV_BACKGROUND'
*           i_grid_title             = text-c00
            is_layout                = t_layout
            it_fieldcat              = t_fieldcat[]
            i_save                   = 'X'
            i_grid_settings          = ls_grid_settings
          TABLES
            t_outtab                 = t_final.
        STOP.
      ELSEIF p_lg     EQ abap_true
          OR p_fic    EQ abap_true
          OR p_koito  EQ abap_true
          OR p_stanl  EQ abap_true
          OR p_etiqad EQ abap_true.
        v_qtimpetq = p_etiq.
        CLEAR: v_qtimpetq_fn.
        IF p_qtlg IS NOT INITIAL.
          gv_atwrt = p_qtlg.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  preenche_tb p_aufnr v_qtimpetq gv_matnr gv_psmng gv_atwrt v_qtimpetq_fn.

  CLEAR gs_parametros.  " <<< FEdM - 04.01.2017

  CHECK ti_etiq[] IS NOT INITIAL.
  IF p_yaz = 'X'.
    gv_formname = 'ZWM_ETIQ_YAZ'.
  ELSEIF p_lg   = 'X'.
    gv_formname         = 'ZWM_ETIQ_LG'.
* >>> FEdM - 17.01.2017
    gs_parametros-name  = 'LG ELECTRONICS DO BRAS'.

  ELSEIF p_fic  = 'X'.
    gv_formname         = 'ZWM_ETIQ_LG'.
    gs_parametros-name  = 'FICOSA DO BRASIL LTDA'.
    gs_parametros-message = text-m01.
* <<< FEdM - 17.01.2017

* >>> FEdM - 09.04.2018
  ELSEIF p_koito  = 'X'.
    gv_formname                     = 'ZWM_ETIQ_LG_QR'.
    gs_parametros-name              = 'NAL DO BRASIL'.
    gs_parametros-print_etiq_num    = abap_true.
    PERFORM zf_set_characteristics.
* <<< FEdM - 09.04.2018

* >>> BIdM - 24.10.2019
  ELSEIF p_stanl = 'X'.
    gv_formname                   = 'ZWM_ETIQ_LG'.
    gs_parametros-name            = 'STANLEY DO BRASIL'.
    gs_parametros-print_etiq_num  = abap_true.
    PERFORM zf_set_characteristics.
* <<< BIdM - 24.10.2019

* >>> BIdM - 12.06.2019
  ELSEIF p_etiqad = 'X'.
    gv_formname   = 'ZSFPP_ETIQ_AD'.
* <<< BIdM - 12.06.2019

  ELSEIF p_orp = 'X'  OR
         p_pin = 'X'  OR
         p_epson = abap_true.

    IF p_flib IS NOT INITIAL.
      gs_parametros-message = text-m01.
    ENDIF.

    IF p_orp = 'X'.
      gv_formname = 'ZWM_ETIQ_ORD'.
    ELSEIF p_pin = 'X'.                   " >>> BIdM - 30.01.2020
      gv_formname = 'ZWM_ETIQ_ORD_PINTURA'.
    ENDIF.                                " <<< BIdM - 30.01.2020

    IF p_reimp IS INITIAL.
      SELECT * FROM zcontrol_etq INTO TABLE it_zctrlet_aux
        WHERE aufnr = p_aufnr.
      IF sy-subrc = 0.
        LOOP AT it_zctrlet_aux INTO wa_zctrlet_aux.
          DELETE zcontrol_etq FROM wa_zctrlet_aux.
        ENDLOOP.
        CLEAR: it_zctrlet_aux[].
        CLEAR: wa_zctrlet_aux.
      ENDIF.
      LOOP AT ti_etiq INTO wa_etiq.
        wa_zctrlet_aux-aufnr = p_aufnr.
        wa_zctrlet_aux-matnr = wa_etiq-matnr.
        wa_zctrlet_aux-qtde  = wa_etiq-atwrt.
        IF wa_etiq-qtd_etfn IS NOT INITIAL.
          wa_zctrlet_aux-qtde  = wa_etiq-qtd_etfn.
        ENDIF.
        wa_zctrlet_aux-vol_de  = wa_etiq-nro_etiq.
        wa_zctrlet_aux-vol_ate = wa_etiq-qtd_tot_etiq.
        APPEND wa_zctrlet_aux TO it_zctrlet_aux.
      ENDLOOP.
      INSERT zcontrol_etq FROM TABLE it_zctrlet_aux.
    ENDIF.

* >>> FEdM - 04.01.2017
    IF p_epson = abap_true.
      gs_parametros-ic_epson  = abap_true.
      gv_formname             = 'ZWM_ETIQ_IC'.
    ENDIF.
* <<< FEdM - 04.01.2017

* >>> FEdM - 16.03.2017

    " Etiqueta SEYON
  ELSEIF p_seoyon IS NOT INITIAL.

    CREATE OBJECT lo_seyon.
    IF p_reimp IS INITIAL.

      lo_seyon->set_printer( p_print ).
      lo_seyon->set_darkness( p_darkn ).

*>>> BIdM 24/01/2020
      lo_seyon->set_print_speed( p_prts ).
      lo_seyon->set_slew_speed( p_slws ).
*<<< BIdM 24/01/2020

      " Impressão
      lo_seyon->print_seoyon_label( ti_etiq[] ).

    ELSEIF p_reimp IS NOT INITIAL.

      lo_seyon->set_printer( p_print ).
      lo_seyon->set_darkness( p_darkn ).
*>>> BIdM 24/01/2020
      lo_seyon->set_print_speed( p_prts ).
      lo_seyon->set_slew_speed( p_slws ).
*<<< BIdM 24/01/2020

      " Reimpressão
      lo_seyon->reprint_seoyon_label( i_aufnr = p_aufnr
                                      i_datum = p_datum
                                      i_seq   = s_seq[] ).
    ENDIF.

    " Encerra execução pois não possui formulário SMARTFORMS
    RETURN.
* <<< FEdM - 16.03.2017

* >>> FEdM - 05.05.2019
  ELSEIF p_magna IS NOT INITIAL.

*>>> BIdM 15.01.2020
    gv_formname = 'ZWM_ETIQ_MAGNA'.

    IF p_reimp IS INITIAL.

      PERFORM zf_print_magna.

    ELSEIF p_reimp IS NOT INITIAL.

      REFRESH it_ztb_magna.
      SELECT * FROM ztb_etiq_magna INTO TABLE it_ztb_magna
        WHERE aufnr  EQ p_aufnr
          AND dtimp  IN s_dat
          AND seq    IN s_seqm
          AND rodada IN s_rod.

      IF it_ztb_magna[] IS INITIAL.

        MESSAGE s000(cl)
        WITH 'Não foram encontradas etiquetas'
             'para reimpressão' DISPLAY LIKE 'E'.
        RETURN.
      ELSE.

        PERFORM alimenta_catalogo USING:
          '1' '01' 'MATNR'      'IT_ZTB_MAGNA' 'L' '' '' '' 'MATNR'  'ZTB_ETIQ_MAGNA' '' '' '' '' '',
          '1' '02' 'AUFNR'     'IT_ZTB_MAGNA' 'L' '' '' '' 'AUFNR' 'ZTB_ETIQ_MAGNA' '' '' '' '' '',
          '1' '03' 'RODADA'     'IT_ZTB_MAGNA' 'L' '' '' '' 'RODADA' 'ZTB_ETIQ_MAGNA' '' '' '' '' '',
          '1' '04' 'SEQ'    'IT_ZTB_MAGNA' 'L' '' '' '' 'SEQ' 'ZTB_ETIQ_MAGNA' '' '' '' '' '',
          '1' '05' 'PARTN'      'IT_ZTB_MAGNA' 'L' '' '' '' 'PARTN' 'ZTB_ETIQ_MAGNA' '' '' 'Partnumber' '' '',
          '1' '06' 'COR'       'IT_ZTB_MAGNA' 'L' '' '' '' 'COR' 'ZTB_ETIQ_MAGNA' '' '' 'Cor' '25' '',
          '1' '07' 'LADO'       'IT_ZTB_MAGNA' 'L' '' '' '' 'LADO' 'ZTB_ETIQ_MAGNA' '' '' 'Lado' '25' '',
          '1' '08' 'MODELO'       'IT_ZTB_MAGNA' 'L' '' '' '' 'MODELO' 'ZTB_ETIQ_MAGNA' '' '' 'Modelo' '25' '',
          '1' '09' 'QTD_OP'       'IT_ZTB_MAGNA' 'L' '' '' '' 'QTD_OP' 'ZTB_ETIQ_MAGNA' '' '' 'Quantidade' '25' '',
          '1' '10' 'DTIMP'       'IT_ZTB_MAGNA' 'L' '' '' '' 'DTIMP' 'ZTB_ETIQ_MAGNA' '' '' 'Data de impressão' '25' ''.

        t_layout-zebra             = 'X'.
        t_layout-box_fieldname     = 'SEL'.
        t_layout-colwidth_optimize = 'X'.

        CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
            i_callback_program       = sy-repid
            i_callback_user_command  = 'USER_COMMAND_MAGNA'
            i_callback_pf_status_set = 'PF'
            is_layout                = t_layout
            it_fieldcat              = t_fieldcat[]
            i_save                   = 'X'
            i_grid_settings          = ls_grid_settings
          TABLES
            t_outtab                 = it_ztb_magna.

        STOP.

      ENDIF.

    ENDIF.
*<<< BIdM 15.01.2020


*    CREATE OBJECT lo_magna.
*
*
*    lo_magna->print_magna_label( ti_etiq[] ).

* <<< FEdM - 05.05.2019

* >>> FEdM - 13.06.2017
  ELSEIF p_tenpao IS NOT INITIAL.

    IF p_reimp IS INITIAL.
      go_tenpao->set_darkness( p_darkn ).
      go_tenpao->print_label( ti_etiq[] ).

    ELSEIF p_reimp IS NOT INITIAL.
      go_tenpao->set_darkness( p_darkn ).
      go_tenpao->reprint_label( i_aufnr = p_aufnr       " Nº ordem
                                i_datum = p_datten      " Data atual do
                                i_seqnr = s_seqnr[] ).  " Sequencial

    ENDIF.

    " Encerra execução pois não possui formulário SMARTFORMS
    RETURN.

  ENDIF.
* <<< FEdM - 13.06.2017


  PERFORM zp_chama_form USING gv_formname.

ENDFORM.                    " PF_IMPRIMIR_ETIQUETA
*
**&---------------------------------------------------------------------*
**&      Form  ZP_CHAMA_FORM
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_GV_FORMNAME  text
**----------------------------------------------------------------------*
FORM zp_chama_form  USING    p_gv_formname TYPE tdsfname.
  DATA: ssfcompop TYPE ssfcompop,
        ssfctrlop TYPE ssfctrlop.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = p_gv_formname
    IMPORTING
      fm_name            = gv_funcao
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  ssfcompop-tddest = 'LOCL'.
  ssfcompop-tdimmed = 'X'.
  ssfcompop-tdnewid = 'X'.
  ssfcompop-tddelete = 'X'.
  ssfctrlop-device = 'PRINTER'.
  ssfctrlop-no_dialog = ''.

  CALL FUNCTION gv_funcao
    EXPORTING
      user_settings      = space
      control_parameters = ssfctrlop
      output_options     = ssfcompop
      i_cct              = p_cct
      i_parametros       = gs_parametros    " <<< FEdM - 04.01.2017
    TABLES
      ti_etiq_lg         = ti_etiq
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4.

ENDFORM.                    " ZP_CHAMA_FORM

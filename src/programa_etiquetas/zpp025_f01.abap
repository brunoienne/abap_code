*----------------------------------------------------------------------*
***INCLUDE ZPP025_F01 .
*----------------------------------------------------------------------*

FORM zf_call_form USING  p_formname   TYPE tdsfname
                         p_parametros TYPE zst_etiquetas.

  DATA: ssfcompop TYPE ssfcompop,
        ssfctrlop TYPE ssfctrlop.

  DATA: lv_funcao TYPE tdsfname.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = p_formname
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
      i_parametros       = p_parametros
    TABLES
      ti_etiq_lg         = t_etiq
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4.


ENDFORM.                    "zf_call_form


*&---------------------------------------------------------------------*
*&      Form  ZF_BUILD_FCAT
*&---------------------------------------------------------------------*
FORM zf_build_fcat  USING    p_fieldname
                             p_ref_fieldname
                             p_ref_tabname
                             p_edit
                    CHANGING lt_fcat TYPE slis_t_fieldcat_alv.

  DATA: ls_fcat TYPE slis_fieldcat_alv.

  ls_fcat-fieldname     = p_fieldname.
  ls_fcat-ref_fieldname = p_ref_fieldname.
  ls_fcat-ref_tabname   = p_ref_tabname.
  ls_fcat-edit          = p_edit.
  APPEND ls_fcat TO lt_fcat.

ENDFORM.                    " ZF_BUILD_FCAT

*&---------------------------------------------------------------------*
*&      Form  user_command_op
*&---------------------------------------------------------------------*
FORM user_command_op USING lv_okcode LIKE sy-ucomm l_selfield TYPE slis_selfield.

  DATA: ls_param    TYPE zst_etiquetas,
        lv_formname TYPE tdsfname.

  DATA: lt_op       TYPE TABLE OF op_type.

  lv_okcode = sy-ucomm.
  CHECK lv_okcode EQ 'EXECUTE'.

  lt_op[] = t_op[].
  DELETE lt_op WHERE sel IS INITIAL.

  IF lt_op[] IS NOT INITIAL.

    REFRESH t_etiq.
    LOOP AT t_op INTO s_op WHERE sel = 'X'.
      s_etiq-aufnr        = s_op-aufnr.
      s_etiq-nro_etiq     = s_op-vol_de.
      s_etiq-qtd_tot_etiq = s_op-vol_ate.
      s_etiq-matnr        = s_op-matnr.
      s_etiq-psmng        = v_psmng.
      s_etiq-atwrt        = s_op-qtde.
      CONDENSE s_etiq-atwrt.
      IF p_etiq EQ 'OPPIN' OR p_etiq EQ 'OPMAG'.
        s_etiq-kanban     = s_op-dtimp. " Pintura ou OP Magna preenche data em Kanban
      ENDIF.
      APPEND s_etiq TO t_etiq. CLEAR s_etiq.
    ENDLOOP.

    IF p_flib EQ 'X'.
      ls_param-message = text-002.
    ENDIF.

    CASE p_etiq.
      WHEN 'OP'.
        lv_formname = 'ZWM_ETIQ_ORD'.
      WHEN 'OPPIN'.
        lv_formname = 'ZWM_ETIQ_ORD_PINTURA'.
      WHEN 'OPMAG'.
        lv_formname = 'ZWM_ETIQ_ORD_MAGNA'.
      WHEN 'OPKOI'.
        lv_formname = 'ZWM_ETIQ_ORD_KOITO'.
      WHEN 'EPSON'.
        ls_param-ic_epson = abap_true.
        lv_formname = 'ZWM_ETIQ_IC'.
    ENDCASE.
    PERFORM zf_call_form USING lv_formname ls_param.

  ELSE.
    MESSAGE text-016 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    "user_command_op

*&---------------------------------------------------------------------*
*&      Form  user_command_mag
*&---------------------------------------------------------------------*
FORM user_command_mag USING lv_okcode LIKE sy-ucomm l_selfield TYPE slis_selfield.

  DATA: ls_param    TYPE zst_etiquetas,
        lv_formname TYPE tdsfname.

  DATA: lt_mag      TYPE TABLE OF magna_ty.

  lv_okcode = sy-ucomm.
  CHECK lv_okcode EQ 'EXECUTE'.

  lt_mag[] = t_magna[].
  DELETE lt_mag WHERE sel IS INITIAL.

  IF lt_mag[] IS NOT INITIAL.

    REFRESH t_etiq.
    LOOP AT t_magna INTO s_magna WHERE sel = 'X'.
      s_etiq-aufnr        = s_magna-aufnr.
      s_etiq-nro_etiq     = s_magna-seq.
      s_etiq-matnr        = s_magna-matnr.
      s_etiq-qtd_tot_etiq = s_magna-qtd_op.
      s_etiq-atwrt        = s_magna-rodada.
      s_etiq-kanban       = s_magna-dtimp.
      APPEND s_etiq TO t_etiq. CLEAR s_etiq.
    ENDLOOP.

    CASE p_etiq.
      WHEN 'MAGNA'.
        lv_formname = 'ZWM_ETIQ_MAGNA'.
      WHEN 'HUF'.
        lv_formname = 'ZWM_ETIQ_HUF'.
    ENDCASE.
    PERFORM zf_call_form USING lv_formname ls_param.

  ELSE.
    MESSAGE text-016 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    "user_command_mag

*&---------------------------------------------------------------------*
*&      Form  user_command_fic
*&---------------------------------------------------------------------*
FORM user_command_fic USING lv_okcode LIKE sy-ucomm l_selfield TYPE slis_selfield.

  DATA: ls_param    TYPE zst_etiquetas,
        lv_formname TYPE tdsfname.

  DATA: lt_fic      TYPE TABLE OF ficosa_ty.

  lv_okcode = sy-ucomm.
  CHECK lv_okcode EQ 'EXECUTE'.

  lt_fic[] = t_ficosa[].
  DELETE lt_fic WHERE sel IS INITIAL.

  IF lt_fic[] IS NOT INITIAL.

    REFRESH t_etiq.
    LOOP AT t_ficosa INTO s_ficosa WHERE sel = 'X'.
      s_etiq-aufnr    = s_ficosa-aufnr.
      s_etiq-matnr    = s_ficosa-matnr.
      s_etiq-psmng    = v_psmng.
      s_etiq-kanban   = s_ficosa-dtimp.
      s_etiq-atwrt    = s_ficosa-qtd.
      s_etiq-qtd_etfn = s_ficosa-qtd_efn.
      APPEND s_etiq TO t_etiq. CLEAR s_etiq.
    ENDLOOP.

    lv_formname      = 'ZWM_ETIQ_LG'.
    ls_param-name    = 'FICOSA DO BRASIL LTDA'.
    ls_param-message = text-002.
    PERFORM zf_call_form USING lv_formname ls_param.

  ELSE.
    MESSAGE text-016 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    "user_command_fic

*&---------------------------------------------------------------------*
*&      Form  zpp002
*&---------------------------------------------------------------------*
FORM zpp002 USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZPP002' EXCLUDING rt_extab.
ENDFORM.                    "pf
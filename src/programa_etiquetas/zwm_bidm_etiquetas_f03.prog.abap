*&---------------------------------------------------------------------*
*&  Include           ZWM_ETIQUETAS_F03
*&---------------------------------------------------------------------*

FORM get_data_alv.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE t_final
  FROM zcontrol_etq WHERE
    aufnr = p_aufnr.

ENDFORM.                    "get_data_alv

*&---------------------------------------------------------------------*
*&      Form  alimenta_catalogo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE               text
*      -->(PV_ROW_POS)        text
*      -->VALUE               text
*      -->(PV_COL_POS)        text
*      -->VALUE               text
*      -->(PV_FIELDNAME)      text
*      -->VALUE               text
*      -->(PV_TABNAME)        text
*      -->VALUE               text
*      -->(PV_JUST)           text
*      -->VALUE               text
*      -->(PV_NO_ZERO)        text
*      -->VALUE               text
*      -->(PV_DO_SUM)         text
*      -->VALUE               text
*      -->(PV_NO_OUT)         text
*      -->VALUE               text
*      -->(PV_REF_FIELDNAME)  text
*      -->VALUE               text
*      -->(PV_REF_TABNAME)    text
*      -->VALUE               text
*      -->(PV_QFIELDNAME)     text
*      -->VALUE               text
*      -->(PV_QTABNAME)       text
*      -->VALUE               text
*      -->(PV_SELTEXT_L)      text
*      -->VALUE               text
*      -->(PV_OUTPUTLEN)      text
*----------------------------------------------------------------------*
FORM alimenta_catalogo USING value(pv_row_pos)
                             value(pv_col_pos)
                             value(pv_fieldname)
                             value(pv_tabname)
                             value(pv_just)
                             value(pv_no_zero)
                             value(pv_do_sum)
                             value(pv_no_out)
                             value(pv_ref_fieldname)
                             value(pv_ref_tabname)
                             value(pv_qfieldname)
                             value(pv_qtabname)
                             value(pv_seltext_l)
                             value(pv_outputlen)
                             pv_edit.                       " <<< FEdM - 28.10.2015

  DATA: lv_fieldcat TYPE slis_fieldcat_alv.

  CLEAR lv_fieldcat.
  lv_fieldcat-row_pos       = pv_row_pos.
  lv_fieldcat-col_pos       = pv_col_pos.
  lv_fieldcat-fieldname     = pv_fieldname.
  lv_fieldcat-tabname       = pv_tabname.
  lv_fieldcat-just          = pv_just.
  lv_fieldcat-no_zero       = pv_no_zero.
  lv_fieldcat-do_sum        = pv_do_sum.
  lv_fieldcat-no_out        = pv_no_out.
  lv_fieldcat-ref_fieldname = pv_ref_fieldname.
  lv_fieldcat-ref_tabname   = pv_ref_tabname.
  lv_fieldcat-qfieldname    = pv_qfieldname.
  lv_fieldcat-qtabname      = pv_qtabname.
  lv_fieldcat-seltext_l     = pv_seltext_l.
  lv_fieldcat-outputlen     = pv_outputlen.
  lv_fieldcat-edit          = pv_edit.                      " <<< FEdM - 28.10.2015
  APPEND lv_fieldcat TO t_fieldcat.

ENDFORM.                    " alimenta_catalogo

*&---------------------------------------------------------------------*
*&      Form  carrega_catalogo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM carrega_catalogo.

  PERFORM alimenta_catalogo USING:
  '1' '01' 'AUFNR'      'WT_REL' 'L' '' '' '' 'AUFNR'  'ZCONTROL_ETQ' '' '' '' '' '',
  '1' '02' 'VOL_DE'     'WT_REL' 'L' '' '' '' 'VOL_DE' 'ZCONTROL_ETQ' '' '' '' '' '',
  '1' '03' 'VOL_ATE'    'WT_REL' 'L' '' '' '' 'VOL_ATE' 'ZCONTROL_ETQ' '' '' '' '' '',
  '1' '04' 'MATNR'      'WT_REL' 'L' '' '' '' 'MATNR' 'ZCONTROL_ETQ' '' '' '' '' '',
  '1' '05' 'QTDE'       'WT_REL' 'L' '' '' '' 'QTDE' 'ZCONTROL_ETQ' '' '' 'Qtde.' '' 'X'.

ENDFORM.                    " CARREGA_CATALOGO

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_OKCODE  text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
FORM user_command USING lv_okcode LIKE sy-ucomm l_selfield TYPE slis_selfield.
* assign the function code to variable v_okcode
  lv_okcode = sy-ucomm.
* handle the code execution based on the function code encountered
  CASE lv_okcode.
* when the function code is EXECUTE then process the selected records
    WHEN 'EXECUTE'.

      REFRESH ti_etiq.
      LOOP AT t_final INTO wa_final WHERE sel = 'X'.
        wa_etiq-aufnr = wa_final-aufnr.
        wa_etiq-nro_etiq = wa_final-vol_de.
        wa_etiq-qtd_tot_etiq = wa_final-vol_ate.
        wa_etiq-matnr = wa_final-matnr.
        wa_etiq-psmng = gv_psmng.
        wa_etiq-atwrt = wa_final-qtde.
        APPEND wa_etiq TO ti_etiq.

      ENDLOOP.

      IF ti_etiq[] IS INITIAL.
        MESSAGE 'Selecionar pelo menos uma linha.' TYPE 'S' DISPLAY LIKE 'E'.
* >>> FEdM - 28.10.2015
*        LEAVE TO SCREEN 0.
*        STOP.
*      ENDIF.
      ELSE.
* <<< FEdM - 28.10.2015
        IF p_orp = 'X'.
          gv_formname = 'ZWM_ETIQ_ORD'.
        ELSEIF p_pin = 'X'.
          gv_formname = 'ZWM_ETIQ_ORD_PINTURA'.
        ENDIF.

        PERFORM zp_chama_form USING gv_formname.
* >>> FEdM - 28.10.2015
*      LEAVE TO SCREEN 0.
*      STOP.
      ENDIF.
* <<< FEdM - 28.10.2015

  ENDCASE.
ENDFORM.                    "USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  zf_impressao_magna
*&---------------------------------------------------------------------*
* >>> BIdM 16.01.2020
FORM zf_print_magna.

  FIELD-SYMBOLS:
                     <fs_magna_aux>      TYPE magna_type.

  DATA: lv_caracteristica TYPE ausp-atinn    ,
        lv_printed        TYPE abap_bool     .

  DATA: ti_etiq_aux TYPE TABLE OF zst_etiq_lg,
        wa_etiq_aux TYPE zst_etiq_lg.

  REFRESH it_ztb_magna_aux.

  SELECT *  INTO CORRESPONDING FIELDS OF TABLE it_ztb_magna_aux
    FROM ztb_etiq_magna
    WHERE aufnr EQ p_aufnr
    AND rodada  EQ p_rodada.


  IF it_ztb_magna_aux[] IS NOT INITIAL. " rodada já impressa alguma vez

    MESSAGE 'Rodada já impressa.' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.

*    lv_printed = abap_true.
*    SORT it_ztb_magna BY seq.
*
*    UNASSIGN <fs_magna_aux>.
*    READ TABLE it_ztb_magna_aux ASSIGNING <fs_magna_aux> INDEX lines( it_ztb_magna_aux ). " lê ultima sequência
*
*    ADD 1 TO <fs_magna_aux>-seq.

*  ELSE.
*    lv_printed = abap_false.
  ENDIF.

  CLEAR wa_etiq.
  READ TABLE ti_etiq INTO wa_etiq INDEX 1.

  DO p_qtop TIMES.
    APPEND INITIAL LINE TO ti_etiq_aux.
  ENDDO.

  REFRESH ti_etiq.


  LOOP AT ti_etiq_aux INTO wa_etiq_aux.

    wa_etiq_magna-mandt  = sy-mandt.
    wa_etiq_magna-matnr  = wa_etiq-matnr.
    wa_etiq_magna-aufnr  = wa_etiq-aufnr.
    wa_etiq_magna-dtimp  = sy-datum.
    wa_etiq_magna-qtd_op = p_qtop.
    wa_etiq_magna-rodada = p_rodada.

    wa_etiq-qtd_tot_etiq = p_qtop.
    wa_etiq-atwrt        = p_rodada.
    wa_etiq-kanban       = sy-datum.

*    IF lv_printed = 'X'.
*      wa_etiq-nro_etiq   = <fs_magna_aux>-seq.
*      wa_etiq_magna-seq  = <fs_magna_aux>-seq.
*      <fs_magna_aux>-seq = <fs_magna_aux>-seq + 1.
*    ELSE.
    wa_etiq-nro_etiq  = sy-tabix.
    wa_etiq_magna-seq = sy-tabix.
*    ENDIF.

    " Cor
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'ZMAT_COR'
      IMPORTING
        output = lv_caracteristica.

    SELECT SINGLE atwrt FROM ausp
      INTO wa_etiq_magna-cor
      WHERE objek EQ wa_etiq-matnr
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
      INTO wa_etiq_magna-lado
      WHERE objek EQ wa_etiq-matnr
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
      INTO wa_etiq_magna-partn
      WHERE objek EQ wa_etiq-matnr
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
      INTO wa_etiq_magna-modelo
      WHERE objek EQ wa_etiq-matnr
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
      INTO wa_etiq_magna-projeto
      WHERE objek EQ wa_etiq-matnr
        AND atinn EQ lv_caracteristica
        AND mafid EQ 'O'
        AND klart EQ '001'.


    APPEND wa_etiq TO ti_etiq.
    APPEND wa_etiq_magna TO it_ztb_magna.
    CLEAR wa_etiq_magna.

  ENDLOOP.

  INSERT ztb_etiq_magna FROM TABLE it_ztb_magna.
  IF sy-subrc IS INITIAL.
    COMMIT WORK.
    MESSAGE s000(cl) WITH 'Registro(s) Magna gravados com sucesso'.
  ENDIF.


ENDFORM.                    "zf_impressao_magna
* <<< BIdM 16.01.2020

*&---------------------------------------------------------------------*
*&      Form  user_command_magna
*&---------------------------------------------------------------------*
FORM user_command_magna USING lv_okcode LIKE sy-ucomm l_selfield TYPE slis_selfield. " >>> BIdM 16.01.2020

  lv_okcode = sy-ucomm.

  CASE lv_okcode.

    WHEN 'EXECUTE'.

      REFRESH ti_etiq.
      LOOP AT it_ztb_magna INTO wa_etiq_magna WHERE sel = 'X'.

        wa_etiq-aufnr        = wa_etiq_magna-aufnr.
        wa_etiq-nro_etiq     = wa_etiq_magna-seq.
        wa_etiq-matnr        = wa_etiq_magna-matnr.
        wa_etiq-qtd_tot_etiq = wa_etiq_magna-qtd_op.
        wa_etiq-atwrt        = wa_etiq_magna-rodada.
        wa_etiq-kanban       = wa_etiq_magna-dtimp.
        APPEND wa_etiq TO ti_etiq.

      ENDLOOP.

      IF ti_etiq[] IS INITIAL.

        MESSAGE 'Selecionar pelo menos uma linha.' TYPE 'S' DISPLAY LIKE 'E'.

      ELSE.
        gv_formname = 'ZWM_ETIQ_MAGNA'.
        PERFORM zp_chama_form USING gv_formname.

      ENDIF.

  ENDCASE.

ENDFORM.                    "user_command_magna          " <<< BIdM 16.01.2020


*&---------------------------------------------------------------------*
*&      Form  pf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM pf USING rt_extab TYPE slis_t_extab.
* >>> FEdM - 28.10.2015
*  SET PF-STATUS 'ZTG_STAT'.
  SET PF-STATUS 'ZPP002' EXCLUDING rt_extab.
* <<< FEdM - 28.10.2015
ENDFORM.                    "pf

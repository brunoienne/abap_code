*&---------------------------------------------------------------------*
*&  Include           ZWM_ETIQUETAS_F01
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*  PERFORM: pf_buscar_afko.
  PERFORM: pf_prepara_dados.

END-OF-SELECTION.

  PERFORM pf_imprimir_etiqueta.

* >>> FEdM - 09.06.2015
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'P02'.
      IF p_orp IS INITIAL.
        screen-active = 0.
        CLEAR p_cct.
      ENDIF.
    ENDIF.

* >>> FEdM - 16.03.2017
    IF screen-group1 EQ 'SEY'.  " Seyon fields
      screen-active = 0.
      IF p_seoyon IS NOT INITIAL AND
         p_reimp IS NOT INITIAL.
        screen-active =  1.
      ENDIF.
    ENDIF.
* <<< FEdM - 16.03.2017

* >>> BIdM - 16.01.2020
    IF screen-group1 EQ 'MAG'.  " Magna reprint
      screen-active = 0.
      IF p_magna IS NOT INITIAL AND
         p_reimp IS NOT INITIAL.
        screen-active =  1.
      ENDIF.
    ENDIF.
* <<< BIdM - 16.01.2020

* >>> BIdM - 17.01.2020
    IF screen-group1 EQ 'MGN'.  " Magna fields
      screen-active = 0.
      IF p_magna IS NOT INITIAL AND p_reimp IS INITIAL.
        screen-active =  1.
      ENDIF.
    ENDIF.
* <<< BIdM - 17.01.2020


* >>> FEdM - 13.06.2017
    IF screen-group1 EQ 'TEN'.  " Tenpao fields
      screen-active = 0.
      IF p_tenpao IS NOT INITIAL AND
         p_reimp  IS NOT INITIAL.
        screen-active =  1.
      ENDIF.
    ENDIF.
* <<< FEdM - 13.06.2017

* >>> FEdM - 27.06.2017
    IF screen-group1 EQ 'ZEB'.
      IF p_seoyon IS NOT INITIAL  .
        screen-active   = 1.
        screen-required = 1.
      ELSE.
        screen-active   = 0.
        screen-required = 0.
      ENDIF.
    ENDIF.
* <<< FEdM - 27.06.2017

* >>> FEdM - 12.08.2019
    IF screen-group1 EQ 'FLI'.
      IF p_orp IS INITIAL AND p_pin IS INITIAL.
        screen-active   = 0.
        screen-required = 0.
      ENDIF.
    ENDIF.
* <<< FEdM - 12.08.2019

    MODIFY SCREEN.
  ENDLOOP.
* <<< FEdM - 09.06.215

* >>> FEdM - 27.06.2017
INITIALIZATION.
  CREATE OBJECT go_tenpao.

AT SELECTION-SCREEN ON RADIOBUTTON GROUP opt.
  CLEAR p_darkn.

AT SELECTION-SCREEN ON p_darkn.

  CASE abap_true.
    WHEN p_tenpao.
      IF p_darkn IS INITIAL.
        p_darkn = go_tenpao->get_darkness( 'ZPP_TENPAO_DARKNESS' ).
      ENDIF.

    WHEN p_seoyon.
      IF p_darkn IS INITIAL.
        p_darkn = go_tenpao->get_darkness( 'ZPP_TENPAO_DARKNESS' ).
      ENDIF.
*>>> BIdM - 24/01/2020
      IF p_prts IS INITIAL.
        p_prts = go_tenpao->get_darkness( 'ZPP_TENPAO_PRINTSPEED' ).
      ENDIF.
      IF p_slws IS INITIAL.
        p_slws = go_tenpao->get_darkness( 'ZPP_TENPAO_SLEWSPEED' ).
      ENDIF.
*<<< BIdM - 24/01/2020
  ENDCASE.
* <<< FEdM - 27.06.2017

*&---------------------------------------------------------------------*
*&      Form  PF_BUSCAR_AFKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM pf_buscar_afko .
*
*  DATA: lv_count TYPE i.
*
*  SELECT COUNT( * )
*    FROM afko
*    INTO lv_count
*   WHERE aufnr = p_aufnr.
*
*  IF lv_count eq 0.
*    MESSAGE 'Ordem não encontrada.' TYPE 'I' DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.
*ENDFORM.                    " PF_BUSCAR_AFKO

FORM pf_prepara_dados.

  DATA: f_input(200)   TYPE c,
        f_output(200)  TYPE c.

  f_input = 'Z_QTDE_CAIXA'.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = f_input
    IMPORTING
      output = f_output.


  SELECT SINGLE atinn FROM cabn INTO gv_atinn
    WHERE atinn = f_output.

  busca_dados p_aufnr.

  SELECT SINGLE atwrt FROM ausp INTO gv_atwrt
    WHERE objek = gv_matnr
      AND atinn = gv_atinn.

  IF gv_psmng IS INITIAL.
    MESSAGE 'Verificar quantidade da Ordem de Produção.' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF gv_atwrt IS INITIAL.
    MESSAGE 'Verificar dados de classificação do material.' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  v_qtimpetq = gv_psmng / gv_atwrt.

  v_qtimpetq = trunc( v_qtimpetq ).

  v_qtimpetq_fn = gv_psmng - ( v_qtimpetq * gv_atwrt ).

  IF NOT v_qtimpetq_fn IS INITIAL.

    v_qtimpetq = v_qtimpetq + 1.

  ENDIF.



* >>> FEdM - 15.03.2017

  CASE abap_true.

      " Cliente Seyon deverá ser impressa 1 etiqueta por quantidade da OP
    WHEN p_seoyon.
      " Quantidade da OP
      v_qtimpetq = gv_psmng.

    WHEN p_magna.
      " Quantidade da OP
      v_qtimpetq = gv_psmng.

      " Cliente Tenpao deverá ser impressa 1 etiqueta por quantidade da
    WHEN p_tenpao.
      " Quantidade da OP
      v_qtimpetq = gv_psmng.

    WHEN p_etiqad.
      " Quantidade da OP
      v_qtimpetq = gv_psmng.

  ENDCASE.
* <<< FEdM - 15.03.2017

ENDFORM.                    "pf_prepara_dados

*&---------------------------------------------------------------------*
*&      Form  ZF_SET_CHARACTERISTICS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_set_characteristics .


  DATA:
    lt_etiq               TYPE TABLE OF   zst_etiq_lg,
    lt_auspx              TYPE TABLE OF   auspx_type,
    lt_ausp               TYPE TABLE OF   ausp.

  DATA:
    ls_etiq               TYPE            zst_etiq_lg,
    ls_auspx              TYPE            auspx_type,
    ls_ausp               TYPE            ausp.

  DATA:
    lv_kanban             TYPE            ausp-atinn.


  CHECK ti_etiq[] IS NOT INITIAL.
  lt_etiq[] = ti_etiq[].

  SORT lt_etiq BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_etiq COMPARING matnr.

  LOOP AT lt_etiq INTO ls_etiq.
    ls_auspx-matnr  = ls_etiq-matnr.
    APPEND ls_auspx TO lt_auspx. CLEAR ls_auspx.
  ENDLOOP.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'ZMAT_KANBAN'
    IMPORTING
      output = lv_kanban.

  SELECT * FROM ausp
    INTO TABLE lt_ausp
    FOR ALL ENTRIES IN lt_auspx
    WHERE objek EQ lt_auspx-matnr
      AND atinn EQ lv_kanban.


  CLEAR ls_etiq.
  LOOP AT lt_ausp INTO ls_ausp.
    ls_etiq-kanban  = ls_ausp-atwrt.
    MODIFY ti_etiq FROM ls_etiq TRANSPORTING kanban
      WHERE matnr EQ ls_ausp-objek.
  ENDLOOP.

  gs_parametros-caracteristica  = 'ZMAT_KANBAN'.


ENDFORM.                    " ZF_SET_CHARACTERISTICS

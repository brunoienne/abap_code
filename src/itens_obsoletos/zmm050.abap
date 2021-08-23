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

REPORT  zmm050.

*--------------------------------------------------------------------
* Tabelas Banco de Dados
*--------------------------------------------------------------------
TABLES: s032.

*--------------------------------------------------------------------
* Constantes
*--------------------------------------------------------------------
CONSTANTS:
  c_red(6)    TYPE            c             VALUE 'FCE4D6',
  c_green(6)  TYPE            c             VALUE 'E2EFDA',
  c_blue(6)   TYPE            c             VALUE 'DDEBF7',
  c_yellow(6) TYPE            c             VALUE 'FFFF00'.

*--------------------------------------------------------------------
* Tipos
*--------------------------------------------------------------------
TYPES:
  BEGIN OF relat_type,
    werks    TYPE s032-werks,
    matnr    TYPE s032-matnr,
    maktx    TYPE makt-maktx,
    bklas    TYPE s032-bklas,
    mbwbest  TYPE s032-mbwbest,
    basme    TYPE s032-basme,
    vlunit   TYPE p DECIMALS 4,
    wbwbest  TYPE s032-wbwbest,
    hwaer    TYPE s032-hwaer,
    letztzug TYPE s032-letztzug,
    letztabg TYPE s032-letztabg,
    letztver TYPE s032-letztver,
    letztbew TYPE s032-letztbew,
    pctotal  TYPE s032-wbwbest,
    pcacumul TYPE s032-wbwbest,
    color    TYPE lvc_t_scol,
  END OF relat_type,


  BEGIN OF s032_type,
    werks    TYPE s032-werks,
    matnr    TYPE s032-matnr,
    bklas    TYPE s032-bklas,
    basme    TYPE s032-basme,
    hwaer    TYPE s032-hwaer,
    letztzug TYPE s032-letztzug,
    letztabg TYPE s032-letztabg,
    letztver TYPE s032-letztver,
    letztbew TYPE s032-letztbew,
  END OF s032_type,

  BEGIN OF aux_type,
    werks   TYPE s032-werks,
    matnr   TYPE s032-matnr,
    mbwbest TYPE s032-mbwbest,
    wbwbest TYPE s032-wbwbest,
  END OF aux_type.


*----------------------------------------------------------------------
* Tela de Seleção
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.

PARAMETERS: p_werks TYPE werks_d OBLIGATORY.
SELECT-OPTIONS s_bklas FOR s032-bklas.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(17) TEXT-003.

PARAMETERS: p_yea   RADIOBUTTON GROUP rd2.
SELECTION-SCREEN COMMENT 35(10) TEXT-001 FOR FIELD p_yea.

PARAMETERS: p_thr   RADIOBUTTON GROUP rd2.
SELECTION-SCREEN COMMENT 55(10) TEXT-002 FOR FIELD p_thr.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
PARAMETERS: p_op1  RADIOBUTTON GROUP rad DEFAULT 'X' MODIF ID alv USER-COMMAND r1,
            p_op2  RADIOBUTTON GROUP rad,
            p_emai TYPE char50 MODIF ID ema OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN END OF BLOCK b01.

*--------------------------------------------------------------------
* Classe relatório
*--------------------------------------------------------------------
INCLUDE zdemo_excel_outputopt_clip_v02.
INCLUDE zmm050_cl.



*----------------------------------------------------------------------
* Eventos de tela
*----------------------------------------------------------------------
INITIALIZATION.
  CREATE OBJECT go_relat.


AT SELECTION-SCREEN ON p_varian.
  go_relat->set_variant( variant = p_varian ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varian.
  p_varian = go_relat->f4_variant( ).

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_op2 IS INITIAL AND
      screen-group1 = 'EMA'.
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.


*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  go_relat->select_data( ).
  go_relat->processing_data( ).
  IF p_op2 IS INITIAL.
    go_relat->output_data( ).
  ELSE.
    go_relat->send_email( ).
  ENDIF.
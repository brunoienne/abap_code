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

REPORT zpp025.
*--------------------------------------------------------------------
* Tabelas Banco de Dados
*--------------------------------------------------------------------
TABLES: ztb_etiq_magna, ztbpp_tenpao, ztb_etiq_seyon.

*--------------------------------------------------------------------
* Tipos
*--------------------------------------------------------------------
TYPES:

BEGIN OF afpo_type,
  aufnr TYPE afpo-aufnr,
  psmng TYPE afpo-psmng,
  matnr TYPE afpo-matnr,
END OF afpo_type,

BEGIN OF ausp_type,
  objek TYPE ausp-objek,
  atwrt TYPE ausp-atwrt,
END OF ausp_type,

BEGIN OF auspx_type,
  matnr TYPE ausp-objek,
END OF auspx_type,

BEGIN OF op_type,
  aufnr   TYPE zcontrol_etq-aufnr,
  vol_de  TYPE zcontrol_etq-vol_de,
  vol_ate TYPE zcontrol_etq-vol_ate,
  matnr   TYPE zcontrol_etq-matnr,
  qtde    TYPE zcontrol_etq-qtde,
  dtimp   TYPE zcontrol_etq_pin-dtimp,
  sel,
END OF op_type.

TYPES: BEGIN OF magna_ty.
        INCLUDE TYPE ztb_etiq_magna.
TYPES: sel.
TYPES: END OF magna_ty.

TYPES: BEGIN OF ficosa_ty.
        INCLUDE TYPE ztb_etiq_fic.
TYPES: sel.
TYPES: END OF ficosa_ty.

*----------------------------------------------------------------------
* Tela de Seleção
*----------------------------------------------------------------------
" Selecionar Nº Ordem
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (12) text-001 FOR FIELD p_aufnr.
PARAMETERS: p_aufnr TYPE afpo-aufnr MODIF ID nop.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK b01.

" Selecionar Etiqueta
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-b02.
PARAMETERS: p_etiq TYPE char8 AS LISTBOX VISIBLE LENGTH 50 USER-COMMAND lis OBLIGATORY.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_flib AS CHECKBOX MODIF ID fli.
SELECTION-SCREEN COMMENT (30) text-002 FOR FIELD p_flib MODIF ID fli.
PARAMETERS p_cct  AS CHECKBOX MODIF ID cct.
SELECTION-SCREEN COMMENT (30) text-003 FOR FIELD p_cct MODIF ID cct.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(8) text-004 FOR FIELD p_op1 MODIF ID sta.
PARAMETERS: p_op1    TYPE aufnr MODIF ID sta.
SELECTION-SCREEN COMMENT 27(8) text-005 FOR FIELD p_op2 MODIF ID sta.
PARAMETERS: p_op2    TYPE aufnr MODIF ID sta.
SELECTION-SCREEN COMMENT 51(8) text-006 FOR FIELD p_op3 MODIF ID sta.
PARAMETERS: p_op3    TYPE aufnr MODIF ID sta.
SELECTION-SCREEN COMMENT 75(8) text-007 FOR FIELD p_op4 MODIF ID sta.
PARAMETERS: p_op4    TYPE aufnr MODIF ID sta.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b02.

" Opções de reeimpressão
SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-b03.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_reimp AS CHECKBOX USER-COMMAND rep.
SELECTION-SCREEN COMMENT 4(11) text-008 FOR FIELD p_reimp.
SELECTION-SCREEN COMMENT 30(18) text-009 FOR FIELD p_nro.
PARAMETERS: p_nro TYPE n LENGTH 3.
SELECTION-SCREEN COMMENT 60(22) text-010 FOR FIELD p_qtlg.
PARAMETERS: p_qtlg TYPE n LENGTH 3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b03.

" Definições Zebra
SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE text-b04.
PARAMETERS: p_print TYPE rsposel-device OBLIGATORY MODIF ID zeb,
            p_darkn TYPE zde_zebra_darkness MODIF ID zeb,
            p_prts  TYPE n LENGTH 2 MODIF ID zeb,
            p_slws  TYPE n LENGTH 2 MODIF ID zeb.
SELECTION-SCREEN END OF BLOCK b04.

" Reeimpressão etiqueta Magna/HUF
SELECTION-SCREEN BEGIN OF BLOCK b05 WITH FRAME TITLE text.
SELECT-OPTIONS: s_rod FOR ztb_etiq_magna-rodada NO-EXTENSION MODIF ID mag,
                s_dat FOR sy-datum DEFAULT sy-datum NO INTERVALS NO-EXTENSION MODIF ID mag,
                s_seqm FOR ztb_etiq_magna-seq NO-EXTENSION MODIF ID mag.
SELECTION-SCREEN END OF BLOCK b05.

" Opções p/ impressão Magna/HUF
SELECTION-SCREEN BEGIN OF BLOCK b06 WITH FRAME TITLE text2.
PARAMETERS: p_rodada TYPE n LENGTH 3 OBLIGATORY MODIF ID mgn,
            p_qtop   TYPE n LENGTH 4 OBLIGATORY MODIF ID mgn.
SELECTION-SCREEN END OF BLOCK b06.

" Reeimpressão etiqueta Tenpao
SELECTION-SCREEN BEGIN OF BLOCK b07 WITH FRAME TITLE text-b07.
PARAMETERS: p_datten  TYPE sydatum DEFAULT sy-datum OBLIGATORY MODIF ID ten.
SELECT-OPTIONS: s_seqnr FOR ztbpp_tenpao-seqnr MODIF ID ten.
SELECTION-SCREEN END OF BLOCK b07.

" Reeimpressão etiqueta Seoyon
SELECTION-SCREEN BEGIN OF BLOCK b08 WITH FRAME TITLE text-b08.
PARAMETERS: p_datum TYPE sydatum DEFAULT sy-datum OBLIGATORY MODIF ID sey.
SELECT-OPTIONS s_seq FOR ztb_etiq_seyon-seq MODIF ID sey.
SELECTION-SCREEN END OF BLOCK b08.


*--------------------------------------------------------------------
* Includes
*--------------------------------------------------------------------
INCLUDE zpp025_top.
INCLUDE zpp025_cl.
INCLUDE zpp025_f01.

*--------------------------------------------------------------------
* Eventos de tela
*--------------------------------------------------------------------
INITIALIZATION.
  CREATE OBJECT go_print.
  CREATE OBJECT go_tenpao.


AT SELECTION-SCREEN ON p_darkn.
  IF p_prts IS INITIAL.
    p_prts = go_tenpao->get_darkness( 'ZPP_TENPAO_PRINTSPEED' ).
  ENDIF.

  IF p_slws IS INITIAL.
    p_slws = go_tenpao->get_darkness( 'ZPP_TENPAO_SLEWSPEED' ).
  ENDIF.

  IF p_darkn IS INITIAL.
    CASE p_etiq.
      WHEN 'TENPAO' OR 'SEOYON'.
        p_darkn = go_tenpao->get_darkness( 'ZPP_TENPAO_DARKNESS' ).
    ENDCASE.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  go_print->set_listbox_values( ).
  go_print->handle_pbo( ).

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  CASE p_etiq.
    WHEN 'STANLEY'.
      go_print->select_multiple_op( ).
      go_print->processing_multiple_op( ).
    WHEN OTHERS.
      go_print->select_single_op( ).
  ENDCASE.

  go_print->print_data( ).
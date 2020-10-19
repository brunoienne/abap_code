*&-----------------------------------------------------------------&*
*                             ClipTech                              *
*&-----------------------------------------------------------------&*
* Programa ..: ZWM003                                               *
* Transação..: ZWM003                                               *
* Módulo.....: WM                                                   *
* Especif....: XX001 -                                              *
* Funcional..:                                                      *
*&---------------------------------------------------------------- *&
* Descrição .: Integração UD Numero Série                           *
*&-----------------------------------------------------------------*&
* Objetivo ..:                                                      *
*&-----------------------------------------------------------------*&
*                           HISTÓRICO                               *
*&-----------------------------------------------------------------*&
* DATA       | REQUEST    | RESPONSÁVEL | DESCRIÇÃO                 *
* 22/04/2020 | ER1K932679 | Bruno Morais|                           *
*&-----------------------------------------------------------------&*

REPORT  zp_bidm_integracao_serie.

*--------------------------------------------------------------------
* Tabelas Banco de Dados
*--------------------------------------------------------------------

*--------------------------------------------------------------------
* Constantes
*--------------------------------------------------------------------
CONSTANTS:
      c_002(3)            TYPE            c             VALUE '002',
      c_o                 TYPE            c             VALUE 'O'.


*--------------------------------------------------------------------
* Variáveis globais
*--------------------------------------------------------------------
DATA: v_ucomm             TYPE            sy-ucomm,
      v_sernr             TYPE            seri-sernr,
      v_charg             TYPE            mcha-licha,
      v_matnr             TYPE            marc-matnr,
      v_werks             TYPE            marc-werks,
      v_msg1(80)          TYPE            c,
      v_msg2(65)          TYPE            c,
      v_msg3(65)          TYPE            c.

*----------------------------------------------------------------------
* Tela de Seleção
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME.
PARAMETERS: p_matnr TYPE marc-matnr OBLIGATORY,
            p_werks TYPE marc-werks OBLIGATORY,
            p_charg TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b01.


*--------------------------------------------------------------------
* Includes
*--------------------------------------------------------------------
INCLUDE ZP_BIDM_INTEGRACAO_SERIE_CL.
*INCLUDE zwm003_cl.

*----------------------------------------------------------------------
* Eventos de tela
*----------------------------------------------------------------------
INITIALIZATION.
  CREATE OBJECT go_integra.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  go_integra->check_marc( ).


*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'Z_STANDARD'.
  SET TITLEBAR 'NUMSERIE'.

  LOOP AT SCREEN.

    IF screen-group1 = 'CH1'. " lote fornecedor
      IF p_charg IS INITIAL.
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDMODULE.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN 'ENTER'.
      go_integra->processing_data( ).
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_exit_9000 INPUT.
  CASE sy-ucomm.
    WHEN '&F03' OR 'FIN'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN '&F12'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_EXIT_9000  INPUT

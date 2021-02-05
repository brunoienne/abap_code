*&-----------------------------------------------------------------&*
*                             ClipTech                              *
*&-----------------------------------------------------------------&*
* Programa ..: ZWM003                                               *
* Transação..:                                                      *
* Módulo.....: WM                                                   *
* Especif....: XX001 - Modelo de ALV                                *
* Funcional..:                                                      *
*&---------------------------------------------------------------- *&
* Descrição .: Integração UD Numero Série                           *
*&-----------------------------------------------------------------*&
* Objetivo ..:                                                      *
*&-----------------------------------------------------------------*&
*                           HISTÓRICO                               *
*&-----------------------------------------------------------------*&
* DATA       | REQUEST    | RESPONSÁVEL | DESCRIÇÃO                 *
* 22/04/2020 | ER1K932679 |  BIMORAIS   |                           *
*&-----------------------------------------------------------------&*

REPORT  zp_bidm_integracao_serie.

*--------------------------------------------------------------------
* Tabelas Banco de Dados
*--------------------------------------------------------------------
TABLES: mara, marc.

*--------------------------------------------------------------------
* Constantes
*--------------------------------------------------------------------
CONSTANTS:
      c_ud(2)             TYPE            c             VALUE 'UD'  ,
      c_o                 TYPE            c             VALUE 'O'   ,
      c_002(3)            TYPE            c             VALUE '002' ,
      c_hist(4)           TYPE            c             VALUE '@96@',
      c_green(4)          TYPE            c             VALUE '@08@',
      c_yellow(4)         TYPE            c             VALUE '@09@',
      c_red(4)            TYPE            c             VALUE '@0A@'.


*--------------------------------------------------------------------
* Variáveis
*--------------------------------------------------------------------
DATA: lv_okcode           TYPE sy-ucomm.

*--------------------------------------------------------------------
* Tipos
*--------------------------------------------------------------------
TYPES: BEGIN OF relat_type,
      matnr              TYPE             mara-matnr,
      maktx              TYPE             makt-maktx,
      werks              TYPE             mard-werks,
      sernr              TYPE             seri-sernr,
      licha              TYPE             mcha-licha,
      status             TYPE             icon_d,
      mensg              TYPE             string,
      t_return           TYPE             bapiret2 OCCURS 0,
  END OF relat_type.

*----------------------------------------------------------------------
* Tela de Seleção
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
SELECT-OPTIONS: s_matnr FOR mara-matnr NO INTERVALS,
                s_werks FOR marc-werks NO-EXTENSION.
PARAMETERS: p_charg TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b01.


*--------------------------------------------------------------------
* Classe relatório
*--------------------------------------------------------------------
INCLUDE zp_bidm_integracao_serie_cl.
INCLUDE zp_bidm_integracao_serie_o01.
INCLUDE zp_bidm_integracao_serie_i01.

*----------------------------------------------------------------------
* Eventos de tela
*----------------------------------------------------------------------
INITIALIZATION.
  CREATE OBJECT go_integra.

AT SELECTION-SCREEN ON p_varian.
  go_integra->set_variant( variant = p_varian ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varian.
  p_varian = go_integra->f4_variant( ).

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  go_integra->select_data( ).
  go_integra->processing_data( ).
  go_integra->output_data( ).
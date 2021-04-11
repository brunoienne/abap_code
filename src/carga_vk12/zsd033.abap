*&-----------------------------------------------------------------&*
*                             ClipTech                              *
*&-----------------------------------------------------------------&*
* Programa ..: ZSD033                                               *
* Transação..:                                                      *
* Módulo.....: SD                                                   *
* Especif....: XX001 - Modelo de ALV                                *
* Funcional..: Filipe E. Morais                                     *
*&---------------------------------------------------------------- *&
* Descrição .: 54699/54700 - Atualização preço VK12                 *
*&-----------------------------------------------------------------*&
* Objetivo ..:                                                      *
*&-----------------------------------------------------------------*&
*                           HISTÓRICO                               *
*&-----------------------------------------------------------------*&
* DATA       | REQUEST    | RESPONSÁVEL | DESCRIÇÃO                 *
* 09/02/2021 | ER1K936081 | BIMORAIS    | Atualização preço VK12    *
*&-----------------------------------------------------------------&*

REPORT  zsd033.

*--------------------------------------------------------------------
* Tabelas Banco de Dados
*--------------------------------------------------------------------

*--------------------------------------------------------------------
* Constantes
*--------------------------------------------------------------------
CONSTANTS:
      c_004(3)            TYPE            c             VALUE '004',
      c_929(3)            TYPE            c             VALUE '929',
      c_log(4)            TYPE            c             VALUE '@96@'.

*--------------------------------------------------------------------
* Tipos
*--------------------------------------------------------------------
TYPES:
BEGIN OF a004_key,
      vkorg               TYPE            a004-vkorg        ,
      vtweg               TYPE            a004-vtweg        ,
      matnr               TYPE            a004-matnr        ,
END OF a004_key,

BEGIN OF a929_key,
      vkorg               TYPE            a929-vkorg        ,
      kdgrp               TYPE            a929-kdgrp        ,
      matnr               TYPE            a929-matnr        ,
END OF a929_key,

BEGIN OF values_type,
      kbetr               TYPE            kbetr             ,
      konwa               TYPE            konwa             ,
      kpein               TYPE            kpein             ,
      kmein               TYPE            kmein             ,
      mxwrt               TYPE            mxwrt             ,
      gkwrt               TYPE            gkwrt             ,
END OF values_type,

BEGIN OF status_type,
      log                 TYPE            char4             ,
      messages            TYPE            bapiret2_t        ,
END OF status_type.

TYPES:BEGIN OF file_004.
        INCLUDE TYPE a004_key.
        INCLUDE TYPE values_type.
TYPES:END OF file_004.

TYPES:BEGIN OF file_929.
        INCLUDE TYPE a929_key.
        INCLUDE TYPE values_type.
TYPES:END OF file_929.


*----------------------------------------------------------------------
* Tela de Seleção
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
SELECTION-SCREEN BEGIN OF LINE.
"Org.vendas/Grupo Clientes/Material
PARAMETER p_rb01  RADIOBUTTON GROUP rad DEFAULT 'X' USER-COMMAND rb1.
SELECTION-SCREEN COMMENT 4(35) text-001 FOR FIELD p_rb01.
SELECTION-SCREEN POSITION 40.
"Material
PARAMETER p_rb02  RADIOBUTTON GROUP rad.
SELECTION-SCREEN COMMENT 45(20) text-002 FOR FIELD p_rb02.
SELECTION-SCREEN END OF LINE.


PARAMETERS: p_kschl TYPE rv13a-kschl OBLIGATORY,
            p_dir   TYPE rlgrap-filename OBLIGATORY,
            p_datam TYPE rv130-datam OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b01.


*--------------------------------------------------------------------
* Classe relatório
*--------------------------------------------------------------------
INCLUDE zsd033_top.
INCLUDE zsd033_cl.


*--------------------------------------------------------------------
* Objetos
*--------------------------------------------------------------------


*----------------------------------------------------------------------
* Eventos de tela
*----------------------------------------------------------------------
INITIALIZATION.
  CREATE OBJECT go_relat.


AT SELECTION-SCREEN ON p_varian.
  go_relat->set_variant( variant = p_varian ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dir.
  p_dir = go_relat->get_filename( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varian.
  p_varian = go_relat->f4_variant( ).


*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  go_relat->select_data( ).
  go_relat->processing_data( ).
  go_relat->output_data( ).
*&-----------------------------------------------------------------&*
*                             ClipTech                              *
*&-----------------------------------------------------------------&*
* Programa ..: ZGBXX001                                             *
* Transação..:                                                      *
* Módulo.....: XX                                                   *
* Especif....: XX001 - Modelo de ALV                                *
* Funcional..:                                                      *
*&---------------------------------------------------------------- *&
* Descrição .: Modelo de ALV                                        *
*&-----------------------------------------------------------------*&
* Objetivo ..:                                                      *
*&-----------------------------------------------------------------*&
*                           HISTÓRICO                               *
*&-----------------------------------------------------------------*&
* DATA       | REQUEST    | RESPONSÁVEL | DESCRIÇÃO                 *
*            |            |             |                           *
*&-----------------------------------------------------------------&*

REPORT  load_data.

*--------------------------------------------------------------------
* Tabelas Banco de Dados
*--------------------------------------------------------------------

*--------------------------------------------------------------------
* Tipos
*--------------------------------------------------------------------
TYPES:
BEGIN OF marca_ty,
  mandt(8) TYPE c ,
  marca(5) TYPE c,
  nome(20) TYPE c,
END OF marca_ty,

BEGIN OF veiculo_ty,
  mandt(8)     TYPE c,
  placa(7)     TYPE c,
  marca(5)     TYPE c,
  modelo(20)   TYPE c,
  ano(4)       TYPE c,
  categoria(9) TYPE c,
  bloqueado(9) TYPE c,
END OF veiculo_ty,

BEGIN OF relat_type,
  status   TYPE string,
  mensagem TYPE string,
END OF relat_type.


*----------------------------------------------------------------------
* Tela de Seleção
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_down RADIOBUTTON GROUP rd1 USER-COMMAND dow DEFAULT 'X'.
SELECTION-SCREEN COMMENT 4(10) text-c02 FOR FIELD p_down.
SELECTION-SCREEN POSITION 15.
PARAMETERS p_uplo RADIOBUTTON GROUP rd1.
SELECTION-SCREEN COMMENT 18(10) text-c01 FOR FIELD p_uplo.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-b02.
PARAMETERS: p_marcas TYPE rlgrap-filename MODIF ID gr1,
            p_veicl  TYPE rlgrap-filename MODIF ID gr1.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-b03.
PARAMETERS: p_rdm TYPE c RADIOBUTTON GROUP rad DEFAULT 'X' MODIF ID gr2,
            p_rdv TYPE c RADIOBUTTON GROUP rad MODIF ID gr2,
            p_dir TYPE rlgrap-filename MODIF ID gr2.
SELECTION-SCREEN END OF BLOCK b03.

SELECTION-SCREEN END OF BLOCK b01.


*--------------------------------------------------------------------
* Classe relatório
*--------------------------------------------------------------------
INCLUDE load_data_cl.


*----------------------------------------------------------------------
* Eventos de tela
*----------------------------------------------------------------------
INITIALIZATION.
  CREATE OBJECT go_relat.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_marcas.
  p_marcas = go_relat->get_file_path( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_veicl.
  p_veicl = go_relat->get_file_path( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dir.
  p_dir = go_relat->get_directory( ).

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    CASE screen-group1.
      WHEN 'GR1'.
        IF p_uplo IS NOT INITIAL.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
      WHEN 'GR2'.
        IF p_uplo IS NOT INITIAL.
          screen-active = 0.
        ELSE.
          screen-active = 1.
        ENDIF.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.


*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  go_relat->select_data( ).
  go_relat->processing_data( ).
  go_relat->output_data( ).
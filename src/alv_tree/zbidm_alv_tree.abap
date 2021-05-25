*&-----------------------------------------------------------------&*
*                             ClipTech                              *
*&-----------------------------------------------------------------&*
* Programa ..: ZGBXX001                                             *
* Transação..:                                                      *
* Módulo.....: XX                                                   *
* Especif....: XX001 - Modelo de ALV                                *
* Funcional..:                                                      *
*&---------------------------------------------------------------- *&
* Descrição .: Modelo de ALV            *
*&-----------------------------------------------------------------*&
* Objetivo ..:                                                      *
*&-----------------------------------------------------------------*&
*                           HISTÓRICO                               *
*&-----------------------------------------------------------------*&
* DATA       | REQUEST    | RESPONSÁVEL | DESCRIÇÃO                 *
*            |            |             |                           *
*&-----------------------------------------------------------------&*

REPORT  zbidm_alv_tree.

*--------------------------------------------------------------------
* Tabelas Banco de Dados
*--------------------------------------------------------------------
TABLES: ztb_veiculos_20.

*--------------------------------------------------------------------
* Constantes
*--------------------------------------------------------------------
CONSTANTS:
      cc_icon_car         TYPE            salv_de_tree_image  VALUE '@AW@',
      cc_icon_positive    TYPE            salv_de_tree_image  VALUE '@04@',
      cc_icon_negative    TYPE            salv_de_tree_image  VALUE '@05@',
      cc_icon_date        TYPE            salv_de_tree_image  VALUE '@1U@',
      c_a(01)             TYPE            c                   VALUE 'A',
      c_mark              TYPE            c                   VALUE 'X'.

*--------------------------------------------------------------------
* Tipos
*--------------------------------------------------------------------
TYPES:
BEGIN OF relat_tree_type,

node      TYPE string,

nome      TYPE ztb_marcas_20-nome,
modelo    TYPE ztb_veiculos_20-modelo,
categoria TYPE ztb_veiculos_20-categoria,
bloqueado TYPE ztb_veiculos_20-bloqueado,
valor     TYPE ztb_veiculos_20-valor,

ano       TYPE ztb_veiculos_20-ano,
marca     TYPE ztb_veiculos_20-marca,

END OF relat_tree_type,

BEGIN OF marca_type,
  marca  TYPE  ztb_marcas_20-marca,
  nome   TYPE  ztb_marcas_20-nome,
  pais   TYPE  ztb_marcas_20-pais,
END OF marca_type,

BEGIN OF total_type,
  ano    TYPE ztb_veiculos_20-ano  ,
  total  TYPE ztb_veiculos_20-valor,
END OF total_type,

BEGIN OF ano_marca,
  ano   TYPE ztb_veiculos_20-ano,
  marca TYPE ztb_veiculos_20-marca,
  total TYPE ztb_veiculos_20-valor,
END OF ano_marca,

BEGIN OF veiculo_type,
  ano       TYPE ztb_veiculos_20-ano,
  marca     TYPE ztb_veiculos_20-marca,
  placa     TYPE ztb_veiculos_20-placa,
  modelo    TYPE ztb_veiculos_20-modelo,
  categoria TYPE ztb_veiculos_20-categoria,
  bloqueado TYPE ztb_veiculos_20-bloqueado,
  valor     TYPE ztb_veiculos_20-valor,
END OF veiculo_type.


*----------------------------------------------------------------------
* Tela de Seleção
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
SELECT-OPTIONS: s_ano       FOR ztb_veiculos_20-ano,
                s_categ     FOR ztb_veiculos_20-categoria.
SELECTION-SCREEN END OF BLOCK b01.


*--------------------------------------------------------------------
* Classe relatório
*--------------------------------------------------------------------
INCLUDE zbidm_alv_tree_cl.


*--------------------------------------------------------------------
* Objetos
*--------------------------------------------------------------------


*----------------------------------------------------------------------
* Eventos de tela
*----------------------------------------------------------------------
INITIALIZATION.
  CREATE OBJECT go_relat.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  go_relat->select_data( ).
  go_relat->processing_data( ).
  go_relat->output_data_tree( ).
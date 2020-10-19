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

REPORT  zbidm_alv_editavel_botao.

*--------------------------------------------------------------------
* Tabelas Banco de Dados
*--------------------------------------------------------------------
TABLES: ztb_veiculos_20.

*--------------------------------------------------------------------
* Constantes
*--------------------------------------------------------------------
CONSTANTS:
      c_a(01)             TYPE            c             VALUE 'A',
      c_mark              TYPE            c             VALUE 'X'.

*--------------------------------------------------------------------
* Tipos
*--------------------------------------------------------------------
TYPES: BEGIN OF veiculo_ty,
        placa     TYPE ztb_veiculos_20-placa,
        marca     TYPE ztb_veiculos_20-marca,
        modelo    TYPE ztb_veiculos_20-modelo,
        ano       TYPE ztb_veiculos_20-ano,
       END OF veiculo_ty,

       BEGIN OF marca_ty,
         marca    TYPE ztb_marcas_20-marca,
         nome     TYPE ztb_marcas_20-nome,
       END OF marca_ty,

       BEGIN OF relat_ty,
        placa     TYPE ztb_veiculos_20-placa,
        marca     TYPE ztb_veiculos_20-marca,
        nome      TYPE ztb_marcas_20-nome,
        modelo    TYPE ztb_veiculos_20-modelo,
        ano       TYPE ztb_veiculos_20-ano,
       END OF relat_ty.

TYPES:
  relat_type_table        TYPE            relat_ty     OCCURS  0.


*--------------------------------------------------------------------
* Tabelas Internas
*--------------------------------------------------------------------
DATA: gt_veiculo        TYPE TABLE OF   veiculo_ty,
      gt_marca          TYPE TABLE OF   marca_ty,
      gt_relat          TYPE TABLE OF   relat_ty.

*--------------------------------------------------------------------
* Workareas
*--------------------------------------------------------------------
DATA: gs_veiculo       TYPE veiculo_ty,
      gs_marca         TYPE marca_ty,
      gs_relat         TYPE relat_ty.

*--------------------------------------------------------------------
* Variaveis
*--------------------------------------------------------------------

DATA: v_variant           TYPE            disvariant.

*--------------------------------------------------------------------
* Ranges
*--------------------------------------------------------------------


*--------------------------------------------------------------------
* Includes especifico do ALV
*--------------------------------------------------------------------

*----------------------------------------------------------------------
* Tela de Seleção
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
SELECT-OPTIONS: s_placa FOR ztb_veiculos_20-placa.
SELECT-OPTIONS: s_marca FOR ztb_veiculos_20-marca.
SELECT-OPTIONS: s_ano   FOR ztb_veiculos_20-ano.


SELECTION-SCREEN END OF BLOCK b01.

PARAMETERS: p_varian      TYPE            slis_vari.

*----------------------------------------------------------------------
* Classe relatório
*----------------------------------------------------------------------
INCLUDE ZBIDM_AVL_EDITAVEL_BOTAO_CL.
*INCLUDE zteste_bidm_005_cl.

INITIALIZATION.
  CREATE OBJECT go_relat.

*----------------------------------------------------------------------
* Eventos de tela
*----------------------------------------------------------------------
AT SELECTION-SCREEN.
  IF p_varian IS INITIAL.
    CLEAR v_variant.
  ELSE.
    v_variant-report  = sy-repid.
    v_variant-variant = p_varian.
  ENDIF.
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varian.
  PERFORM zf_variant_f4 USING p_varian.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM zf_select_data.
  PERFORM zf_processing_data.
  PERFORM zf_print_data.


*&-----------------------------------------------------------------*&
*   Declaração de FORMs
*&-----------------------------------------------------------------*&

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
FORM zf_select_data .

  SELECT placa marca modelo ano INTO TABLE gt_veiculo
    FROM ztb_veiculos_20
    WHERE placa IN s_placa
    AND   marca IN s_marca
    AND   ano   IN s_ano.

  IF gt_veiculo[] IS NOT INITIAL.
    SELECT marca nome FROM ztb_marcas_20
      INTO TABLE gt_marca
      FOR ALL ENTRIES IN gt_veiculo
      WHERE marca = gt_veiculo-marca.
  ENDIF.

ENDFORM.                    " ZF_SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSING_DATA
*&---------------------------------------------------------------------*
FORM zf_processing_data .

  SORT gt_veiculo BY marca.
  SORT gt_marca   BY marca.

  LOOP AT gt_veiculo INTO gs_veiculo.

    CLEAR gs_marca.
    READ TABLE gt_marca INTO gs_marca
    WITH KEY marca = gs_veiculo-marca BINARY SEARCH.
    gs_relat-placa  = gs_veiculo-placa.
    gs_relat-marca  = gs_marca-marca.
    gs_relat-nome   = gs_marca-nome.
    gs_relat-modelo = gs_veiculo-modelo.
    gs_relat-ano    = gs_veiculo-ano.
    APPEND gs_relat TO gt_relat.
    CLEAR gs_relat.

  ENDLOOP.

ENDFORM.                    " ZF_PROCESSING_DATA

*&---------------------------------------------------------------------*
*&      Form  ZF_PRINT_DATA
*&---------------------------------------------------------------------*
FORM zf_print_data .

  DATA: lt_fcat           TYPE            slis_t_fieldcat_alv,
        lw_lout           TYPE            slis_layout_alv,
        lw_glay           TYPE            lvc_s_glay.

  IF gt_relat[] IS NOT INITIAL.

    lw_lout-zebra              = c_mark.
    lw_lout-expand_all         = c_mark.
    lw_lout-colwidth_optimize  = c_mark.

    PERFORM zf_preenche_fcat TABLES lt_fcat.

    lw_glay-edt_cll_cb = 'X'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'ZF_PFSTATUS'
        i_callback_user_command  = 'ZF_UCOMMAND'
        i_grid_settings          = lw_glay
        is_layout                = lw_lout
        it_fieldcat              = lt_fcat
        i_save                   = c_a
        is_variant               = v_variant
      TABLES
        t_outtab                 = gt_relat
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ELSE.

    MESSAGE s000(cl) WITH text-e00 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM.                    " ZF_PRINT_DATA

*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHE_FCAT
*&---------------------------------------------------------------------*
FORM zf_preenche_fcat
  TABLES   pt_fieldcat.

  DATA: ls_fcat         TYPE      slis_fieldcat_alv.


  ls_fcat-fieldname     = 'PLACA'.
  ls_fcat-ref_tabname   = 'ZTB_VEICULOS_20'.
  ls_fcat-key           = abap_true.
  APPEND ls_fcat TO pt_fieldcat. CLEAR ls_fcat.

  ls_fcat-fieldname     = 'MARCA'.
  ls_fcat-ref_tabname   = 'ZTB_VEICULOS_20'.
  APPEND ls_fcat TO pt_fieldcat. CLEAR ls_fcat.

  ls_fcat-fieldname     = 'NOME'.
  ls_fcat-ref_tabname   = 'ZTB_MARCAS_20'.
  APPEND ls_fcat TO pt_fieldcat. CLEAR ls_fcat.

  ls_fcat-fieldname     = 'MODELO'.
  ls_fcat-ref_tabname   = 'ZTB_VEICULOS_20'.
  APPEND ls_fcat TO pt_fieldcat. CLEAR ls_fcat.

  ls_fcat-fieldname     = 'ANO'.
  ls_fcat-ref_tabname   = 'ZTB_VEICULOS_20'.
  APPEND ls_fcat TO pt_fieldcat. CLEAR ls_fcat.

ENDFORM.                    " ZF_PREENCHE_FCAT

*&---------------------------------------------------------------------*
*&      Form  ZF_PFSTATUS
*&---------------------------------------------------------------------*
FORM zf_pfstatus
  USING pw_extab          TYPE            kkblo_t_extab.

  SET PF-STATUS 'ZGUI_ALV' .

ENDFORM.                    "ZF_PFSTATUS

*&---------------------------------------------------------------------*
*&      Form  ZF_UCOMMAND
*&---------------------------------------------------------------------*
FORM zf_ucommand
  USING lc_ucomm          TYPE            sy-ucomm
        ls_selfield       TYPE            slis_selfield.


  DATA:
     lo_grid1             TYPE REF TO     cl_gui_alv_grid, " ALV instance
     l_repid              TYPE            syrepid .

  l_repid = sy-repid.


  " permite tornar o alv editavel, atualizando campos de tabela interna
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_callback_program = l_repid
      e_repid            = l_repid
      e_grid             = lo_grid1.

  CASE lc_ucomm.

    WHEN '_CHANGE'.

      go_relat->change_alv(
        CHANGING
          it_relat = gt_relat
          io_grid1 = lo_grid1
      ).

    WHEN '&DATA_SAVE'.
      go_relat->save_data(
        CHANGING
          it_relat = gt_relat
          io_grid1 = lo_grid1
      ).

  ENDCASE.

ENDFORM.                    "ZF_UCOMMAND

*&---------------------------------------------------------------------*
*&      Form  ZF_VARIANT_F4
*&---------------------------------------------------------------------*
FORM zf_variant_f4  USING    p_p_var.

  CONSTANTS:
    c_variant_save        VALUE 'U'.

  DATA: wa_variant        TYPE            disvariant,
        vl_exit.

  MOVE: sy-repid TO wa_variant-report.

  CLEAR v_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = wa_variant
      i_save        = c_variant_save
    IMPORTING
      e_exit        = vl_exit
      es_variant    = v_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc IS INITIAL AND vl_exit IS INITIAL.
    p_varian   = v_variant-variant.
  ELSE.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "z_variant_f4

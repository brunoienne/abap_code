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

REPORT  zp_bidm_modelo_abap.

*--------------------------------------------------------------------
* Tabelas Banco de Dados
*--------------------------------------------------------------------

*--------------------------------------------------------------------
* Constantes
*--------------------------------------------------------------------
CONSTANTS:
      c_a(01)             TYPE            c             VALUE 'A',
      c_mark              TYPE            c             VALUE 'X'.

*--------------------------------------------------------------------
* Tipos
*--------------------------------------------------------------------
TYPES: BEGIN OF relat_type,
         campo1,
       END OF relat_type.

*--------------------------------------------------------------------
* Tabelas Internas
*--------------------------------------------------------------------
DATA: gt_relat             TYPE TABLE OF   relat_type.

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
SELECTION-SCREEN END OF BLOCK b01.
PARAMETERS: p_varian      TYPE            slis_vari.

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
*       text
*----------------------------------------------------------------------*
FORM zf_select_data .



ENDFORM.                    " ZF_SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_processing_data .



ENDFORM.                    " ZF_PROCESSING_DATA

*&---------------------------------------------------------------------*
*&      Form  ZF_PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_print_data .

  DATA: lt_fcat           TYPE            slis_t_fieldcat_alv,
        lw_lout           TYPE            slis_layout_alv.

  IF gt_relat[] IS NOT INITIAL.

    lw_lout-zebra              = c_mark.
    lw_lout-expand_all         = c_mark.
    lw_lout-colwidth_optimize  = c_mark.
    lw_lout-box_fieldname      = 'MARK'.

    PERFORM zf_preenche_fcat TABLES lt_fcat.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'ZF_PFSTATUS'
        i_callback_user_command  = 'ZF_UCOMMAND'
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
*       Preenchimento da tabela Field catalog
*----------------------------------------------------------------------*
*      -->PW_FCAT  text
*----------------------------------------------------------------------*
FORM zf_preenche_fcat
  TABLES   pt_fieldcat.

  DATA: ls_fcat         TYPE      slis_fieldcat_alv.

*  ls_fcat-fieldname = 'VBELN' .
*  ls_fcat-tabname   = 'T_RELAT'.
*  ls_fcat-seltext_l = 'Doc.vendas'.
*  ls_fcat-ref_fieldname = 'VBELN'.
*  ls_fcat-ref_tabname   = 'VBAK'.
*  APPEND ls_fcat TO pt_fieldcat. CLEAR ls_fcat.
*
*  ls_fcat-fieldname = 'POSNR' .
*  ls_fcat-tabname   = 'T_RELAT'.
*  ls_fcat-seltext_l = 'Item'.
*  APPEND ls_fcat TO pt_fieldcat. CLEAR ls_fcat.



ENDFORM.                    " ZF_PREENCHE_FCAT

*&---------------------------------------------------------------------*
*&      Form  ZF_PFSTATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_pfstatus
  USING pw_extab          TYPE            kkblo_t_extab.

*  SET PF-STATUS 'ZGUI_ALV' EXCLUDING pw_extab.

ENDFORM.                    "ZF_PFSTATUS

*&---------------------------------------------------------------------*
*&      Form  ZF_UCOMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_ucommand
  USING lc_ucomm          TYPE            sy-ucomm
        ls_selfield       TYPE            slis_selfield.


*  CASE lc_ucomm.
*    WHEN .
*  ENDCASE.

ENDFORM.                    "ZF_UCOMMAND

*&---------------------------------------------------------------------*
*&      Form  ZF_VARIANT_F4
*&---------------------------------------------------------------------*
*       Selecao de variante
*----------------------------------------------------------------------*
*      -->P_VAR  VAriante
*----------------------------------------------------------------------*
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

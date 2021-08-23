*&---------------------------------------------------------------------*
*&  Include           ZTESTE_BIDM_132_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
FORM zf_select_data .

SELECT a~nfenum,
a~docnum,
b~itmnum,
b~matnr,
b~maktx,
b~nbm,
b~menge FROM j_1bnfdoc AS a
INNER JOIN  j_1bnflin AS b ON a~docnum = b~docnum INTO TABLE @DATA(lt_nflin)
UP TO 3 ROWS.

LOOP AT lt_nflin INTO DATA(ls_nflin).
MOVE-CORRESPONDING ls_nflin TO wa_nflin.
APPEND wa_nflin TO it_nflin. CLEAR wa_nflin.
ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSING_DATA
*&---------------------------------------------------------------------*
FORM zf_processing_data .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_POPUP
*&---------------------------------------------------------------------*
FORM zf_alv_popup .

DATA: lt_fcat TYPE            slis_t_fieldcat_alv,
lt_excl TYPE            slis_t_extab.

DATA: ls_fcat          TYPE            slis_fieldcat_alv,
ls_excl          TYPE            slis_extab,
ls_layout        TYPE            slis_layout_alv,
ls_selfield      TYPE            slis_selfield,
ls_grid_settings TYPE            lvc_s_glay,
ls_exit          TYPE            slis_exit_by_user.

DATA: lv_lines         TYPE            i.

READ TABLE it_nflin INTO wa_nflin WITH KEY mark = 'X'.

IF sy-subrc IS NOT INITIAL.
MESSAGE s000(cl) WITH 'Marcar uma linha' DISPLAY LIKE 'E'.
RETURN.
ENDIF.

ls_layout-zebra              = abap_true.
ls_layout-expand_all         = abap_true.
ls_layout-colwidth_optimize  = abap_true.
ls_layout-info_fieldname     = 'COLOR'.
ls_layout-box_fieldname      = 'MARKI'.

ls_grid_settings-edt_cll_cb = abap_true.

ls_fcat-fieldname     = 'TAXTYP'.
ls_fcat-tabname       = 'WA_NFLIN-T_TAXES'.
ls_fcat-ref_fieldname = ls_fcat-fieldname.
ls_fcat-ref_tabname   = 'J_1BNFSTX'.
ls_fcat-seltext_s     = 'Tipo Imp.'.
ls_fcat-edit          = abap_true.
ls_fcat-input         = abap_true.
APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

ls_fcat-fieldname     = 'BASE'.
ls_fcat-tabname       = 'WA_NFLIN-T_TAXES'.
ls_fcat-ref_fieldname = ls_fcat-fieldname.
ls_fcat-seltext_s     = 'Base'.
ls_fcat-edit          = abap_true.
ls_fcat-input         = abap_true.
APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

ls_fcat-fieldname     = 'RATE'.
ls_fcat-tabname       = 'WA_NFLIN-T_TAXES'.
ls_fcat-ref_fieldname = ls_fcat-fieldname.
ls_fcat-seltext_s     = '%'.
ls_fcat-edit          = abap_true.
ls_fcat-input         = abap_true.
APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

ls_fcat-fieldname     = 'TAXVAL'.
ls_fcat-tabname       = 'WA_NFLIN-T_TAXES'.
ls_fcat-ref_fieldname = ls_fcat-fieldname.
ls_fcat-seltext_s     = '$'.
ls_fcat-edit          = abap_true.
ls_fcat-input         = abap_true.
APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

ls_fcat-fieldname     = 'EXCBAS'.
ls_fcat-tabname       = 'WA_NFLIN-T_TAXES'.
ls_fcat-ref_fieldname = ls_fcat-fieldname.
ls_fcat-seltext_s     = 'Isentas'.
ls_fcat-edit          = abap_true.
ls_fcat-input         = abap_true.
APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

ls_fcat-fieldname     = 'OTHBAS'.
ls_fcat-tabname       = 'WA_NFLIN-T_TAXES'.
ls_fcat-ref_fieldname = ls_fcat-fieldname.
ls_fcat-seltext_s     = 'Outras'.
ls_fcat-edit          = abap_true.
ls_fcat-input         = abap_true.
APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

IF lv_lines LT 10.
lv_lines = lv_lines + 10.
ENDIF.

PERFORM zf_ltexcl TABLES lt_excl.

DATA(lv_title) = CONV text70( |Impostos Doc. { wa_nflin-docnum } Item { wa_nflin-itmnum }| ).

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
EXPORTING
i_grid_title             = lv_title
i_callback_program       = sy-repid
i_callback_user_command  = 'ZF_USER_COMMAND'
i_callback_pf_status_set = 'PF_TAXES'
is_layout                = ls_layout
it_fieldcat              = lt_fcat
it_excluding             = lt_excl
i_screen_start_column    = 10
i_screen_start_line      = 5
i_screen_end_column      = 100
i_screen_end_line        = lv_lines
TABLES
t_outtab                 = wa_nflin-t_taxes
EXCEPTIONS
program_error            = 1
OTHERS                   = 2.
IF sy-subrc <> 0.
ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_LTEXCL
*&---------------------------------------------------------------------*
FORM zf_ltexcl  TABLES   pt_excl.

APPEND '&ETA' TO pt_excl.
APPEND '%SC'  TO pt_excl.
APPEND '%SC+' TO pt_excl.
APPEND '&OUP' TO pt_excl.
APPEND '&ODN' TO pt_excl.
APPEND '&ILT' TO pt_excl.
APPEND '&OL0' TO pt_excl.
APPEND '&CRB' TO pt_excl.
APPEND '&CRL' TO pt_excl.
APPEND '&CRR' TO pt_excl.
APPEND '&CRE' TO pt_excl.
APPEND '&UMC' TO pt_excl.
APPEND '&RNT' TO pt_excl.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_USER_COMMAND
*&---------------------------------------------------------------------*
FORM zf_user_command USING lv_okcode LIKE sy-ucomm l_selfield TYPE slis_selfield.

READ TABLE it_nflin ASSIGNING FIELD-SYMBOL(<fs_lin>)
WITH KEY mark = 'X'.

CASE lv_okcode.
WHEN '_OK' OR '_CANCEL'.
<fs_lin>-t_taxes = wa_nflin-t_taxes.
LEAVE TO SCREEN 0.

WHEN '_ADDROW'.
APPEND INITIAL LINE TO wa_nflin-t_taxes.
l_selfield-refresh = abap_true.


WHEN '_DELROW'.
DELETE wa_nflin-t_taxes WHERE marki EQ abap_true.
l_selfield-refresh = abap_true.
ENDCASE.

<fs_lin>-t_taxes = wa_nflin-t_taxes.

ENDFORM.

FORM pf_taxes USING pw_extab TYPE kkblo_t_extab.
SET PF-STATUS 'PF_TAXES' EXCLUDING pw_extab.
ENDFORM.
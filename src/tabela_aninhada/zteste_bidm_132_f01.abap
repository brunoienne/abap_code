*&---------------------------------------------------------------------*
*&  Include           ZTESTE_BIDM_132_F01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
p_table_name
p_mark_name
CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
DATA: l_ok     TYPE sy-ucomm,
l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
SEARCH p_ok FOR p_tc_name.
IF sy-subrc <> 0.
EXIT.
ENDIF.
l_offset = strlen( p_tc_name ) + 1.
l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
CASE l_ok.
WHEN 'INSR'.                      "insert row
PERFORM fcode_insert_row USING    p_tc_name
               p_table_name.
CLEAR p_ok.

WHEN 'DELE'.                      "delete row
PERFORM fcode_delete_row USING    p_tc_name
               p_table_name
               p_mark_name.
CLEAR p_ok.

WHEN 'P--' OR                     "top of list
'P-'  OR                     "previous page
'P+'  OR                     "next page
'P++'.                       "bottom of list
PERFORM compute_scrolling_in_tc USING p_tc_name
                   l_ok.
CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
WHEN 'MARK'.                      "mark all filled lines
PERFORM fcode_tc_mark_lines USING p_tc_name
               p_table_name
               p_mark_name   .
CLEAR p_ok.

WHEN 'DMRK'.                      "demark all filled lines
PERFORM fcode_tc_demark_lines USING p_tc_name
                 p_table_name
                 p_mark_name .
CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
USING    p_tc_name           TYPE dynfnam
p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
DATA l_lines_name       LIKE feld-name.
DATA l_selline          LIKE sy-stepl.
DATA l_lastline         TYPE i.
DATA l_line             TYPE i.
DATA l_table_name       LIKE feld-name.
FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
CONCATENATE p_table_name '[]' INTO l_table_name. "table body
ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
GET CURSOR LINE l_selline.
IF sy-subrc <> 0.                   " append line to table
l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
IF l_selline > <lines>.
<tc>-top_line = l_selline - <lines> + 1 .
ELSE.
<tc>-top_line = 1.
ENDIF.
ELSE.                               " insert line into table
l_selline = <tc>-top_line + l_selline - 1.
l_lastline = <tc>-top_line + <lines> - 1.
ENDIF.
*&SPWIZARD: set new cursor line                                        *
l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
INSERT INITIAL LINE INTO <table> INDEX l_selline.
<tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
USING    p_tc_name           TYPE dynfnam
p_table_name
p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
DATA l_table_name       LIKE feld-name.

FIELD-SYMBOLS <tc>         TYPE cxtab_control.
FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
FIELD-SYMBOLS <wa>.
FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
CONCATENATE p_table_name '[]' INTO l_table_name. "table body
ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
DESCRIBE TABLE <table> LINES <tc>-lines.

LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

IF <mark_field> = 'X'.
DELETE <table> INDEX syst-tabix.
IF sy-subrc = 0.
<tc>-lines = <tc>-lines - 1.
ENDIF.
ENDIF.
ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
             p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
DATA l_tc_new_top_line     TYPE i.
DATA l_tc_name             LIKE feld-name.
DATA l_tc_lines_name       LIKE feld-name.
DATA l_tc_field_name       LIKE feld-name.

FIELD-SYMBOLS <tc>         TYPE cxtab_control.
FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
l_tc_new_top_line = 1.
ELSE.
*&SPWIZARD: no, ...                                                    *
CALL FUNCTION 'SCROLLING_IN_TABLE'
EXPORTING
entry_act      = <tc>-top_line
entry_from     = 1
entry_to       = <tc>-lines
last_page_full = 'X'
loops          = <lines>
ok_code        = p_ok
overlapping    = 'X'
IMPORTING
entry_new      = l_tc_new_top_line
EXCEPTIONS
*        NO_ENTRY_OR_PAGE_ACT  = 01
*        NO_ENTRY_TO    = 02
*        NO_OK_CODE_OR_PAGE_GO = 03
OTHERS         = 0.
ENDIF.

*&SPWIZARD: get actual tc and column                                   *
GET CURSOR FIELD l_tc_field_name
AREA  l_tc_name.

IF syst-subrc = 0.
IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
SET CURSOR FIELD l_tc_field_name LINE 1.
ENDIF.
ENDIF.

*&SPWIZARD: set the new top line                                       *
<tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
      p_table_name
      p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
DATA l_table_name       LIKE feld-name.

FIELD-SYMBOLS <tc>         TYPE cxtab_control.
FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
FIELD-SYMBOLS <wa>.
FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
CONCATENATE p_table_name '[]' INTO l_table_name. "table body
ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

<mark_field> = 'X'.
ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
        p_table_name
        p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
DATA l_table_name       LIKE feld-name.

FIELD-SYMBOLS <tc>         TYPE cxtab_control.
FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
FIELD-SYMBOLS <wa>.
FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
CONCATENATE p_table_name '[]' INTO l_table_name. "table body
ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

<mark_field> = space.
ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

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
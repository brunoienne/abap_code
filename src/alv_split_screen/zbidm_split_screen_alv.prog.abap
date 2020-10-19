*&---------------------------------------------------------------------*
*& Report  ZBIDM_SPLIT_SCREEN_ALV
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zbidm_split_screen_alv.



DATA: custom_container    TYPE REF TO     cl_gui_custom_container,
      splitter            TYPE REF TO     cl_gui_splitter_container,
      graphic_parent1     TYPE REF TO     cl_gui_container,
      graphic_parent2     TYPE REF TO     cl_gui_container.

DATA ref_grid             TYPE REF TO     cl_gui_alv_grid.
DATA ref_grid1            TYPE REF TO     cl_gui_alv_grid.
**   create container in which to place splitter
**   (place it in the custom control named CONTAINER
**   defined using screenpainter in dynpro 100)

DATA: st_var              TYPE            disvariant,
      save                TYPE            char01  VALUE 'A',
      loyo                TYPE            lvc_s_layo,
      loyo1               TYPE            lvc_s_layo.

DATA: fcat                TYPE            lvc_t_fcat,
      fcat1               TYPE            lvc_t_fcat.

DATA: itab_final          TYPE TABLE OF   mara,
      itab_final1         TYPE TABLE OF   makt.


SELECT * FROM mara INTO TABLE itab_final.
IF itab_final[] IS NOT INITIAL.
  SELECT * FROM makt INTO TABLE itab_final1
    FOR ALL ENTRIES IN itab_final
    WHERE matnr  EQ  itab_final-matnr.
ENDIF.

loyo1-no_toolbar  = abap_true.
BREAK-POINT.

CREATE OBJECT custom_container
  EXPORTING
    container_name = 'CONTAINER'. "use uppercase letters!
*
**   create splitter container in which to place graphics
CREATE OBJECT splitter
  EXPORTING
    parent  = custom_container
    rows    = 2
    columns = 1
    align   = 15. " (splitter fills the hole custom container)
**   get part of splitter container for 1st table
CALL METHOD splitter->get_container
  EXPORTING
    row       = 1
    column    = 1
  RECEIVING
    container = graphic_parent1.

CALL METHOD splitter->set_row_height
  EXPORTING
    id                = 1
    height            = 10.

**   get part of splitter container for 2nd table
CALL METHOD splitter->get_container
  EXPORTING
    row       = 2
    column    = 1
  RECEIVING
    container = graphic_parent2.

CALL METHOD splitter->set_row_height
  EXPORTING
    id                = 2
    height            = 4.


PERFORM zf_field_cat.

CREATE OBJECT ref_grid
  EXPORTING
    i_parent = graphic_parent1.
**  Display first ALV
PERFORM set_display.
CREATE OBJECT ref_grid1
  EXPORTING
    i_parent = graphic_parent2.
**  Display second ALV
PERFORM set_display1.

CALL SCREEN 9000.

*&amp;--------------------------------------------------------------------*
*&amp;      Form  set_display
*&amp;--------------------------------------------------------------------*
*       text  Display first ALV
*---------------------------------------------------------------------*
FORM set_display.
  CALL METHOD ref_grid->set_table_for_first_display
    EXPORTING
      is_variant      = st_var
      i_save          = save
      is_layout       = loyo
    CHANGING
      it_outtab       = itab_final[]
      it_fieldcatalog = fcat.
ENDFORM.                    "set_display
*&--------------------------------------------------------------------*
*&      Form  set_display1
*&--------------------------------------------------------------------*
*       text Display second ALV
*---------------------------------------------------------------------*
FORM set_display1.
  CALL METHOD ref_grid1->set_table_for_first_display
    EXPORTING
      is_variant      = st_var
      i_save          = save
      is_layout       = loyo1
    CHANGING
      it_outtab       = itab_final1[]
      it_fieldcatalog = fcat1.
ENDFORM.                    "set_display1

*&---------------------------------------------------------------------*
*&      Form  ZF_FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_field_cat .

  DATA: ls_fcat           TYPE            lvc_s_fcat.


  ls_fcat-fieldname = 'MATNR'.
  ls_fcat-tabname   = 'ITAB_FINAL'.
  ls_fcat-ref_field = 'MATNR'.
  ls_fcat-ref_table = 'MARA'.
  APPEND ls_fcat TO  fcat. CLEAR ls_fcat.

  ls_fcat-fieldname = 'MATNR'.
  ls_fcat-tabname   = 'ITAB_FINAL1'.
  ls_fcat-ref_field = 'MATNR'.
  ls_fcat-ref_table = 'MAKT'.
  APPEND ls_fcat TO  fcat1. CLEAR ls_fcat.

  ls_fcat-fieldname = 'MAKTX'.
  ls_fcat-tabname   = 'ITAB_FINAL1'.
  ls_fcat-ref_field = 'MAKTX'.
  ls_fcat-ref_table = 'MAKT'.
  APPEND ls_fcat TO  fcat1. CLEAR ls_fcat.


ENDFORM.                    " ZF_FIELD_CAT

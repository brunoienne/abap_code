*&---------------------------------------------------------------------*
*&  Include           ZTESTE_BIDM_054_C01
*&---------------------------------------------------------------------*

CLASS zcl_evento_orig DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object.


ENDCLASS.                    "zcl_evento_orig DEFINITION

*----------------------------------------------------------------------*
*       CLASS zcl_evento_orig IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS zcl_evento_orig IMPLEMENTATION.

  METHOD handle_toolbar.

    DATA lw_botoes TYPE stb_button.

    DELETE e_object->mt_toolbar
    WHERE: function EQ '&SORT_ASC',
           function EQ '&SORT_DSC'.

  ENDMETHOD.                    "handle_toolbar

ENDCLASS.                    "zcl_evento_orig IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS zcl_evento_path DEFINITION
*----------------------------------------------------------------------*
CLASS zcl_evento_path DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object,

      handle_menu_button FOR EVENT menu_button OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_ucomm,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.

ENDCLASS.                    "zcl_evento_path DEFINITION

*----------------------------------------------------------------------*
*       CLASS zcl_evento_path IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS zcl_evento_path IMPLEMENTATION.

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN ''.
    ENDCASE.

  ENDMETHOD.                    "handle_user_command

  METHOD handle_toolbar.

    DATA lw_botoes TYPE stb_button.

    DELETE e_object->mt_toolbar
    WHERE: function EQ '&SORT_ASC',
           function EQ '&SORT_DSC'.

  ENDMETHOD.                    "handle_toolbar

  METHOD handle_menu_button.

    CASE e_ucomm.
      WHEN ''.
    ENDCASE.

  ENDMETHOD.                    "handle_menu_button


ENDCLASS.                    "zcl_evento_path IMPLEMENTATION

DATA: v_event_orig      TYPE REF TO zcl_evento_orig,
      v_event_path      TYPE REF TO zcl_evento_path.

*&---------------------------------------------------------------------*
*&  Include           ZTESTE_BIDM_005_CL
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------
* Classe relatório Definição
*----------------------------------------------------------------------

CLASS lcl_relat DEFINITION.

  PUBLIC SECTION.
    METHODS:
      change_alv
        CHANGING
          it_relat        TYPE            relat_type_table
          io_grid1        TYPE REF TO     cl_gui_alv_grid,

      save_data
        CHANGING
          it_relat        TYPE            relat_type_table
          io_grid1        TYPE REF TO     cl_gui_alv_grid.


  PRIVATE SECTION.




ENDCLASS.                    "lcl_relat DEFINITION

*----------------------------------------------------------------------
* Classe relatório Implementação
*----------------------------------------------------------------------
CLASS lcl_relat IMPLEMENTATION.


  METHOD change_alv.

    DATA:
      lt_fcat               TYPE            lvc_t_fcat.

    FIELD-SYMBOLS:
      <ls_fcat>             TYPE            lvc_s_fcat.

    DATA:
      ls_stable             TYPE            lvc_s_stbl.


    " Ativa o evento de mudar dados
    CALL METHOD io_grid1->check_changed_data.

    io_grid1->get_frontend_fieldcatalog(
      IMPORTING
        et_fieldcatalog = lt_fcat
    ).

    " Campos editaveis
    READ TABLE lt_fcat ASSIGNING <ls_fcat> WITH KEY fieldname = 'MODELO'.
    IF  <ls_fcat> IS ASSIGNED.
      <ls_fcat>-edit  = abap_true.
    ENDIF.

    READ TABLE lt_fcat ASSIGNING <ls_fcat> WITH KEY fieldname = 'ANO'.
    IF  <ls_fcat> IS ASSIGNED.
      <ls_fcat>-edit  = abap_true.
    ENDIF.

    io_grid1->set_frontend_fieldcatalog( lt_fcat ).

    io_grid1->refresh_table_display(
      EXPORTING
        is_stable      = ls_stable
    ).


  ENDMETHOD.                    "change_alv


  METHOD save_data.

    DATA:
     lt_fcat              TYPE            lvc_t_fcat.

    DATA:
     ls_stable            TYPE            lvc_s_stbl.


    DATA:
      lv_ok               TYPE            abap_bool,
      lv_nok              TYPE            abap_bool.

    FIELD-SYMBOLS:
      <ls_relat>          TYPE            relat_ty.

    FIELD-SYMBOLS:
      <ls_fcat>           TYPE            lvc_s_fcat.


    LOOP AT it_relat ASSIGNING <ls_relat>.


      UPDATE ztb_veiculos_20
      SET   modelo = <ls_relat>-modelo
            ano    = <ls_relat>-ano
      WHERE placa  = <ls_relat>-placa.

      IF sy-subrc IS INITIAL.
        lv_ok = abap_true.
      ELSE.
        lv_nok = abap_true.
      ENDIF.

    ENDLOOP.

    IF lv_ok IS INITIAL.
      ROLLBACK WORK.
      MESSAGE s000(cl) WITH 'Nenhum registro modificado' DISPLAY LIKE 'E'.
    ENDIF.

    IF lv_nok IS INITIAL.
      COMMIT WORK.
      MESSAGE s000(cl) WITH 'Modificações gravadas'.
    ENDIF.

    " Activate event data_changed
    CALL METHOD io_grid1->check_changed_data.

    io_grid1->get_frontend_fieldcatalog(
      IMPORTING
        et_fieldcatalog = lt_fcat
    ).

    " Campos editaveis
    READ TABLE lt_fcat ASSIGNING <ls_fcat> WITH KEY fieldname = 'MODELO'.
    IF  <ls_fcat> IS ASSIGNED.
      <ls_fcat>-edit  = abap_false.
    ENDIF.

    READ TABLE lt_fcat ASSIGNING <ls_fcat> WITH KEY fieldname = 'ANO'.
    IF  <ls_fcat> IS ASSIGNED.
      <ls_fcat>-edit  = abap_false.
    ENDIF.

    io_grid1->set_frontend_fieldcatalog( lt_fcat ).

    io_grid1->refresh_table_display(
      EXPORTING
        is_stable      = ls_stable
    ).

  ENDMETHOD.                    "save_data


ENDCLASS.                    "lcl_relat IMPLEMENTATION

*--------------------------------------------------------------------
* Objetos
*--------------------------------------------------------------------
DATA: go_relat            TYPE REF TO     lcl_relat.

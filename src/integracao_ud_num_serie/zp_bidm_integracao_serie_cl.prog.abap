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


*----------------------------------------------------------------------
* Tela de Seleção
*----------------------------------------------------------------------
PARAMETERS: p_varian      TYPE            slis_vari.

*----------------------------------------------------------------------
* Classe relatório Definição
*----------------------------------------------------------------------
CLASS lcl_integra_ud_serie DEFINITION.
  PUBLIC SECTION.

    METHODS:
      select_data,

      processing_data,

      output_data,

      create_alv,

      f4_variant
        RETURNING
          value(variant)  TYPE            disvariant-variant,

      set_variant
        IMPORTING
          variant         TYPE            disvariant-variant
          report          TYPE            disvariant-report OPTIONAL.

  PRIVATE SECTION.
    METHODS:
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
         IMPORTING
           e_ucomm,

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
         IMPORTING
           e_object,

      hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
         e_row_id,

      build_fieldcat
        CHANGING
          ct_fcat     TYPE           lvc_t_fcat,

      exclude_toolbar
        CHANGING
          ct_toolbar  TYPE          ui_functions,

      check_ud
        CHANGING
          c_relat     TYPE           relat_type,

      fill_bdc
         IMPORTING
           i_dynbegin     TYPE bdc_start    OPTIONAL
           i_dynpro       TYPE bdc_dynr     OPTIONAL
           i_program      TYPE bdc_prog     OPTIONAL
           i_fnam         TYPE bdcdata-fnam OPTIONAL
           i_fval         TYPE bdc_fval     OPTIONAL,

      call_transaction_bdc
        IMPORTING
          i_lqua      TYPE           lqua
          i_mseg      TYPE           mseg
        CHANGING
          c_relat     TYPE           relat_type,

      call_bapi_objcl_create
        IMPORTING
          i_lqua      TYPE          lqua
        CHANGING
          c_relat     TYPE          relat_type.


*--------------------------------------------------------------------
* Objetos ALV
*--------------------------------------------------------------------
    DATA: co_grid         TYPE REF TO     cl_gui_alv_grid.


*--------------------------------------------------------------------
* Tabelas Internas
*--------------------------------------------------------------------
    DATA:
      ct_relat            TYPE TABLE OF   relat_type,
      ct_marc             TYPE TABLE OF   marc,
      ct_makt             TYPE TABLE OF   makt,
      ct_lqua             TYPE TABLE OF   lqua,
      ct_ausp             TYPE TABLE OF   ausp,
      ct_bdc              TYPE TABLE OF   bdcdata.

*--------------------------------------------------------------------
* Work areas
*--------------------------------------------------------------------
    DATA:
      cs_relat            TYPE            relat_type ,
      cs_marc             TYPE            marc       ,
      cs_makt             TYPE            makt       ,
      cs_equi             TYPE            equi       ,
      cs_lqua             TYPE            lqua       ,
      cs_mseg             TYPE            mseg       ,
      cs_ausp             TYPE            ausp       .

*--------------------------------------------------------------------
* Atributos estáticos
*--------------------------------------------------------------------
    CLASS-DATA:
      ls_variant          TYPE            disvariant                  .


ENDCLASS.                    "lcl_integra_ud_serie DEFINITION


*----------------------------------------------------------------------
* Classe relatório Implementação
*----------------------------------------------------------------------
CLASS lcl_integra_ud_serie IMPLEMENTATION.

  " Seleção de dados
  METHOD select_data.

    SELECT * FROM marc INTO TABLE ct_marc
     WHERE matnr IN s_matnr
       AND werks IN s_werks
       AND sernp NE space.

    CHECK ct_marc[] IS NOT INITIAL.

    SELECT * FROM makt INTO TABLE ct_makt
      FOR ALL ENTRIES IN ct_marc
      WHERE matnr EQ ct_marc-matnr
        AND spras EQ sy-langu.

  ENDMETHOD.                    "select_data


  " Tratamento dos dados selecionados
  METHOD processing_data.

    SORT ct_makt BY matnr.

    LOOP AT ct_marc INTO cs_marc.

      CLEAR cs_makt.
      READ TABLE ct_makt INTO cs_makt
        WITH KEY matnr = cs_marc-matnr BINARY SEARCH.

      cs_relat-matnr = cs_marc-matnr.
      cs_relat-maktx = cs_makt-maktx.
      cs_relat-werks = cs_marc-werks.
      APPEND cs_relat TO ct_relat. CLEAR cs_relat.

    ENDLOOP.

  ENDMETHOD.                    "processing_data
  " Saída ALV
  METHOD output_data.

    IF ct_relat[] IS INITIAL.
      MESSAGE text-e00 TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      CALL SCREEN 9000.
    ENDIF.

  ENDMETHOD.                    "output_data

  METHOD create_alv.

    DATA: lt_toolbar      TYPE            ui_functions,
          lt_fcat         TYPE            lvc_t_fcat,
          ls_layout       TYPE            lvc_s_layo.

    IF co_grid IS BOUND.
      co_grid->refresh_table_display( ).
    ELSE.

      ls_layout-zebra = 'X'.

      me->build_fieldcat(
        CHANGING
          ct_fcat = lt_fcat
      ).

      me->exclude_toolbar(
        CHANGING
          ct_toolbar = lt_toolbar
      ).

      CREATE OBJECT co_grid
        EXPORTING
          i_parent = cl_gui_custom_container=>default_screen.

      " multiple selection
      co_grid->set_ready_for_input( 1 ).

      " registra edição
      co_grid->register_edit_event(
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter
      ).

      co_grid->set_table_for_first_display(
        EXPORTING
          is_variant                    = ls_variant
          it_toolbar_excluding          = lt_toolbar
          is_layout                     = ls_layout
          i_save                        = 'A'
        CHANGING
          it_outtab                     = me->ct_relat
          it_fieldcatalog               = lt_fcat
      ).

      SET HANDLER handle_toolbar FOR co_grid.       " adiciona botões
      SET HANDLER handle_user_command FOR co_grid.  " evento de clique
      SET HANDLER hotspot_click FOR co_grid.        " evento de clique
      CALL METHOD co_grid->set_toolbar_interactive. " ativa interação c/ barra de ferramentas

    ENDIF.

  ENDMETHOD.                    "create_alv

  " Busca variante de exibição do ALV
  METHOD f4_variant.

    CONSTANTS:
      lc_variant_save     TYPE            c  VALUE 'U'.

    DATA: ls_variant      TYPE            disvariant,
          vl_exit.

    ls_variant-report = sy-repid.

    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
      EXPORTING
        is_variant    = ls_variant
        i_save        = lc_variant_save
      IMPORTING
        e_exit        = vl_exit
        es_variant    = ls_variant
      EXCEPTIONS
        not_found     = 1
        program_error = 2
        OTHERS        = 3.

    IF sy-subrc IS INITIAL AND vl_exit IS INITIAL.
      variant   = ls_variant-variant.
      set_variant( variant = variant report = sy-repid ).
    ELSE.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "f4_variant

  METHOD set_variant.

    ls_variant-variant = variant.

    IF report IS NOT INITIAL.
      ls_variant-report = report.
    ELSE.
      ls_variant-report = sy-repid.
    ENDIF.

  ENDMETHOD.                    "set_variant

  METHOD handle_user_command.

    DATA: lt_rows  TYPE lvc_t_row,
          ls_row   TYPE lvc_s_row.

    FIELD-SYMBOLS:
        <fs_relat> TYPE relat_type.

    CASE e_ucomm.
      WHEN '_SEND'.
        co_grid->get_selected_rows(
          IMPORTING
            et_index_rows = lt_rows
        ).

        IF lt_rows[] IS INITIAL.
          MESSAGE text-e01 TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          LOOP AT lt_rows INTO ls_row.

            UNASSIGN <fs_relat>.
            READ TABLE ct_relat ASSIGNING <fs_relat> INDEX ls_row-index.

            CHECK <fs_relat> IS ASSIGNED.
            IF <fs_relat>-sernr IS NOT INITIAL.
              IF p_charg IS NOT INITIAL AND
                 <fs_relat>-licha IS INITIAL.
                <fs_relat>-status = c_yellow.
                <fs_relat>-mensg  = text-m01.
              ELSE.
                me->check_ud(
                  CHANGING
                    c_relat = <fs_relat>
                ).
              ENDIF.
            ELSE.
              <fs_relat>-status = c_yellow.
              <fs_relat>-mensg  = text-m02.
            ENDIF.

          ENDLOOP.

          co_grid->refresh_table_display( ).

        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "on_user_command

  METHOD handle_toolbar.

    DATA: ls_toolbar      TYPE            stb_button.

    CLEAR ls_toolbar                                   .
    ls_toolbar-function     =  '_SEND'                 .
    ls_toolbar-butn_type    =  0                       .
    ls_toolbar-icon         = icon_execute_object      .
    ls_toolbar-quickinfo    =  text-m03                .
    ls_toolbar-disabled     =  space                   .
    INSERT ls_toolbar INTO e_object->mt_toolbar INDEX 1.

    ls_toolbar-butn_type    = 3.
    INSERT ls_toolbar INTO e_object->mt_toolbar INDEX 2.

  ENDMETHOD.                    "handle_toolbar


  METHOD hotspot_click.

    DATA: ls_relat TYPE relat_type.

    READ TABLE ct_relat INTO ls_relat INDEX e_row_id-index.

    CHECK ls_relat-status IS NOT INITIAL.

    IF ls_relat-t_return IS INITIAL.
      MESSAGE s000(cl) WITH text-m04 DISPLAY LIKE 'E'.
    ELSE.
      CALL FUNCTION 'C14ALD_BAPIRET2_SHOW'
        TABLES
          i_bapiret2_tab = ls_relat-t_return.
    ENDIF.

  ENDMETHOD.                    "hotspot_click

  METHOD build_fieldcat.

    DATA: ls_fcat TYPE lvc_s_fcat.

    ls_fcat-fieldname = 'MATNR'.
    ls_fcat-ref_field = 'MATNR'.
    ls_fcat-ref_table = 'MARA'.
    ls_fcat-coltext   = space.
    ls_fcat-outputlen = 18.
    ls_fcat-edit      = space.
    APPEND ls_fcat TO ct_fcat. CLEAR ls_fcat.

    ls_fcat-fieldname = 'MAKTX'.
    ls_fcat-ref_field = 'MAKTX'.
    ls_fcat-ref_table = 'MAKT'.
    ls_fcat-coltext   = space.
    ls_fcat-outputlen = 40.
    ls_fcat-edit      = space.
    APPEND ls_fcat TO ct_fcat. CLEAR ls_fcat.

    ls_fcat-fieldname = 'WERKS'.
    ls_fcat-ref_field = 'WERKS'.
    ls_fcat-ref_table = 'MARC'.
    ls_fcat-coltext   = space.
    ls_fcat-outputlen = 6.
    ls_fcat-edit      = space.
    APPEND ls_fcat TO ct_fcat. CLEAR ls_fcat.

    ls_fcat-fieldname = 'SERNR'.
    ls_fcat-ref_field = 'SERNR'.
    ls_fcat-ref_table = 'SERI'.
    ls_fcat-coltext   = space.
    ls_fcat-outputlen = 18.
    ls_fcat-edit      = 'X'.
    APPEND ls_fcat TO ct_fcat. CLEAR ls_fcat.

    IF p_charg IS NOT INITIAL.
      ls_fcat-fieldname = 'LICHA'.
      ls_fcat-ref_field = 'LICHA'.
      ls_fcat-ref_table = 'MCHA'.
      ls_fcat-coltext   = space.
      ls_fcat-outputlen = 18.
      ls_fcat-edit      = 'X'.
      APPEND ls_fcat TO ct_fcat. CLEAR ls_fcat.
    ENDIF.

    ls_fcat-fieldname = 'STATUS'.
    ls_fcat-ref_field = space.
    ls_fcat-ref_table = space.
    ls_fcat-coltext   = text-t01.
    ls_fcat-outputlen = 5.
    ls_fcat-edit      = space.
    ls_fcat-hotspot   = 'X'.
    ls_fcat-icon      = 'X'.
    APPEND ls_fcat TO ct_fcat. CLEAR ls_fcat.

    ls_fcat-fieldname = 'MENSG'.
    ls_fcat-ref_field = space.
    ls_fcat-ref_table = space.
    ls_fcat-coltext   = text-t02.
    ls_fcat-outputlen = 80.
    ls_fcat-edit      = space.
    APPEND ls_fcat TO ct_fcat. CLEAR ls_fcat.

  ENDMETHOD.                    "build_fieldcat

  METHOD exclude_toolbar.
    "exclude buttons from toolbar
    APPEND cl_gui_alv_grid=>mc_fc_check          TO ct_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut        TO ct_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy       TO ct_toolbar.
    APPEND cl_gui_alv_grid=>mc_mb_paste          TO ct_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo       TO ct_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO ct_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO ct_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row   TO ct_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO ct_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_subtot         TO ct_toolbar.
    APPEND cl_gui_alv_grid=>mc_fc_print          TO ct_toolbar.

  ENDMETHOD.                    "exclude_toolbar

  METHOD check_ud.

    FIELD-SYMBOLS:
          <fs_lqua> TYPE  lqua.

    DATA:  lv_atinn TYPE  atinn.

    DATA:  lt_ausp_aux TYPE TABLE OF ausp.

    CLEAR: cs_equi.

    SELECT SINGLE * FROM equi INTO cs_equi
      WHERE sernr EQ c_relat-sernr
        AND matnr EQ c_relat-matnr.

    IF sy-subrc IS NOT INITIAL.

      SELECT * FROM lqua INTO TABLE ct_lqua
        WHERE matnr EQ c_relat-matnr
          AND werks EQ c_relat-werks
          AND lenum NE space.

      IF ct_lqua[] IS NOT INITIAL.

        LOOP AT ct_lqua INTO cs_lqua.
          cs_ausp-atwrt = cs_lqua-lenum.
          APPEND cs_ausp TO lt_ausp_aux. CLEAR cs_ausp.
        ENDLOOP.

        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = c_ud
          IMPORTING
            output = lv_atinn.

        SELECT * FROM ausp INTO TABLE ct_ausp
          FOR ALL ENTRIES IN lt_ausp_aux
          WHERE atinn EQ lv_atinn
            AND mafid EQ c_o
            AND klart EQ c_002
            AND atwrt EQ lt_ausp_aux-atwrt.

        IF ct_ausp[] IS NOT INITIAL.

          SORT: ct_lqua BY lenum,
                ct_ausp BY atwrt.

          LOOP AT ct_lqua ASSIGNING <fs_lqua>.

            CLEAR cs_ausp.
            READ TABLE ct_ausp INTO cs_ausp
              WITH KEY atwrt = <fs_lqua>-lenum BINARY SEARCH.

            IF sy-subrc IS INITIAL.
              CLEAR <fs_lqua>-lenum.
            ENDIF.

          ENDLOOP.

          SORT ct_lqua BY lenum.
          DELETE ct_lqua WHERE lenum IS INITIAL.

        ENDIF.

      ENDIF.

      IF ct_lqua[] IS INITIAL.
        c_relat-status = c_red.
        c_relat-mensg  = text-m05.
        RETURN.
      ELSE.

        CLEAR cs_lqua.
        READ TABLE ct_lqua INTO cs_lqua INDEX 1.

        CLEAR cs_mseg.
        SELECT SINGLE * FROM mseg INTO cs_mseg
          WHERE mblnr EQ cs_lqua-wenum
            AND mjahr EQ cs_lqua-wdatu+0(4)
            AND zeile EQ cs_lqua-wepos.

        me->call_transaction_bdc(
          EXPORTING
            i_lqua  = cs_lqua
            i_mseg  = cs_mseg
          CHANGING
            c_relat = c_relat
        ).

      ENDIF.

    ELSE.

      CONCATENATE 'Nº de série' c_relat-sernr 'já cadastrado para material' c_relat-matnr INTO c_relat-mensg SEPARATED BY space.
      c_relat-status = c_red.
      RETURN.

    ENDIF.

  ENDMETHOD.                    "check_ud

  METHOD fill_bdc.

    DATA: ls_bdc TYPE bdcdata.

    IF i_dynbegin IS NOT INITIAL.
      ls_bdc-dynbegin = i_dynbegin.
      ls_bdc-program  = i_program.
      ls_bdc-dynpro   = i_dynpro.
    ELSE.
      ls_bdc-fnam     = i_fnam.
      ls_bdc-fval     = i_fval.
    ENDIF.

    APPEND ls_bdc TO ct_bdc.

  ENDMETHOD.                    "fill_bdc

  METHOD call_transaction_bdc.

    DATA: lt_msg       TYPE tab_bdcmsgcoll.

    DATA: ls_opt       TYPE ctu_params    .

    DATA: lv_fval      TYPE bdc_fval      .

    REFRESH ct_bdc.

    me->fill_bdc( i_dynbegin = 'X' i_dynpro = '1000' i_program  = 'SAPMIEQ0').
    me->fill_bdc( i_fnam = 'BDC_CURSOR'  i_fval = 'RISA0-SERNR').
    me->fill_bdc( i_fnam = 'BDC_OKCODE'  i_fval = '/00').

    lv_fval = c_relat-matnr.
    me->fill_bdc( i_fnam = 'RISA0-MATNR' i_fval = lv_fval ).

    CLEAR lv_fval.
    lv_fval = c_relat-sernr.
    me->fill_bdc( i_fnam = 'RISA0-SERNR' i_fval = lv_fval ).
    me->fill_bdc( i_fnam = 'RM63E-EQTYP' i_fval = 'X').
*----------------------------------------------------------------------------------
    me->fill_bdc( i_dynbegin = 'X' i_dynpro = '0101' i_program  = 'SAPMIEQ0').
    me->fill_bdc( i_fnam = 'BDC_OKCODE'  i_fval  = '/00').
    me->fill_bdc( i_fnam = 'BDC_CURSOR'  i_fval  = 'EQBS-LIFNR').
    me->fill_bdc( i_fnam = 'EQBS-LBBSA'  i_fval  = '01'). " estoque de utilização livre

    CLEAR lv_fval.
    lv_fval = c_relat-werks.
    me->fill_bdc( i_fnam = 'EQBS-B_WERK' i_fval  = lv_fval ).

    CLEAR lv_fval.
    lv_fval = i_lqua-lgort.
    me->fill_bdc( i_fnam = 'EQBS-B_LAGER' i_fval  = lv_fval ).

    CLEAR lv_fval.
    lv_fval = i_lqua-charg.
    me->fill_bdc( i_fnam = 'EQBS-B_CHARGE' i_fval  = lv_fval ).

    CLEAR lv_fval.
    lv_fval = i_mseg-lifnr.
    me->fill_bdc( i_fnam = 'EQBS-LIFNR'    i_fval  = lv_fval ).
*----------------------------------------------------------------------------------
    me->fill_bdc( i_dynbegin = 'X' i_dynpro = '0101' i_program  = 'SAPMIEQ0').
    me->fill_bdc( i_fnam = 'BDC_OKCODE'     i_fval  = '=EQUI').
    me->fill_bdc( i_fnam = 'BDC_CURSOR'     i_fval  = 'EQBS-LIFNR').
    me->fill_bdc( i_fnam = 'EQBS-LBBSA'     i_fval  = '01').

    CLEAR lv_fval.
    lv_fval = c_relat-werks.
    me->fill_bdc( i_fnam = 'EQBS-B_WERK'    i_fval  = lv_fval ).

    CLEAR lv_fval.
    lv_fval = i_lqua-lgort.
    me->fill_bdc( i_fnam = 'EQBS-B_LAGER'   i_fval  = lv_fval ).

    CLEAR lv_fval.
    lv_fval = i_lqua-charg.
    me->fill_bdc( i_fnam = 'EQBS-B_CHARGE'  i_fval  = lv_fval ).
    me->fill_bdc( i_fnam = 'ITOB-CHARGE'    i_fval  = lv_fval ).

    CLEAR lv_fval.
    lv_fval = i_mseg-lifnr.
    me->fill_bdc( i_fnam = 'EQBS-LIFNR'      i_fval = lv_fval ).
*----------------------------------------------------------------------------------
    me->fill_bdc( i_dynbegin = 'X' i_dynpro = '0101' i_program  = 'SAPMIEQ0').
    me->fill_bdc( i_fnam = 'BDC_OKCODE'     i_fval   = '=BU' ).
    me->fill_bdc( i_fnam = 'BDC_CURSOR'     i_fval   = 'EQBS-LIFNR').
    me->fill_bdc( i_fnam = 'EQBS-LBBSA'     i_fval   = '01').

    CLEAR lv_fval.
    lv_fval = c_relat-werks.
    me->fill_bdc( i_fnam = 'EQBS-B_WERK'    i_fval   = lv_fval ).

    CLEAR lv_fval.
    lv_fval = i_lqua-lgort.
    me->fill_bdc( i_fnam = 'EQBS-B_LAGER'   i_fval   = lv_fval ).

    CLEAR lv_fval.
    lv_fval = i_lqua-charg.
    me->fill_bdc( i_fnam = 'EQBS-B_CHARGE'   i_fval  = lv_fval ).
    me->fill_bdc( i_fnam = 'ITOB-CHARGE'     i_fval  = lv_fval ).

    CLEAR lv_fval.
    lv_fval = i_mseg-lifnr.
    me->fill_bdc( i_fnam = 'EQBS-LIFNR'     i_fval   = lv_fval ).

    CLEAR lv_fval.
    lv_fval = c_relat-matnr.
    me->fill_bdc( i_fnam = 'ITOB-SHTXT'     i_fval   = lv_fval ).

    CLEAR lv_fval.
    WRITE sy-datum USING EDIT MASK '__.__.____' TO lv_fval.
    me->fill_bdc( i_fnam = 'ITOB-DATAB'     i_fval   = lv_fval ).

    ls_opt-dismode  = 'N'.
    ls_opt-defsize  = 'X'.
    ls_opt-updmode  = 'S'.
    ls_opt-racommit = 'X'.

    CALL TRANSACTION 'IQ01'
    USING ct_bdc
    OPTIONS FROM ls_opt
    MESSAGES INTO lt_msg.

    READ TABLE lt_msg TRANSPORTING NO FIELDS
      WITH KEY msgtyp = 'S'.

    IF sy-subrc IS INITIAL.

      me->call_bapi_objcl_create(
      EXPORTING
          i_lqua  = i_lqua
      CHANGING
          c_relat = c_relat
      ).

    ELSE.

      CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
        TABLES
          imt_bdcmsgcoll = lt_msg
          ext_return     = c_relat-t_return.

      c_relat-status = c_hist.
      c_relat-mensg = text-001.
      RETURN.

    ENDIF.

  ENDMETHOD.                    "call_transaction_bdc

  METHOD call_bapi_objcl_create.

    DATA: lt_allocchar TYPE TABLE OF bapi1003_alloc_values_char.

    DATA: ls_allocchar TYPE bapi1003_alloc_values_char.

    DATA: lv_equnr     TYPE equi-equnr,
          lv_lenum     TYPE lqua-lenum,
          lv_charg     TYPE lqua-charg,
          lv_objectkey TYPE bapi1003_key-object.


    SELECT SINGLE equnr FROM equi INTO lv_equnr
    WHERE sernr EQ c_relat-sernr
      AND matnr EQ c_relat-matnr.

    lv_objectkey = lv_equnr.

    ls_allocchar-charact    = c_ud.
    ls_allocchar-value_char = i_lqua-lenum.
    APPEND ls_allocchar TO lt_allocchar. CLEAR ls_allocchar.

    IF c_relat-licha IS NOT INITIAL.

      ls_allocchar-charact    = 'LOTE_FORNECEDOR'.
      ls_allocchar-value_char = c_relat-licha.
      APPEND ls_allocchar TO lt_allocchar. CLEAR ls_allocchar.

    ENDIF.

    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        objectkeynew    = lv_objectkey
        objecttablenew  = 'EQUI'
        classnumnew     = 'UDINTEGRAWM'
        classtypenew    = c_002
      TABLES
        allocvalueschar = lt_allocchar
        return          = c_relat-t_return.

    READ TABLE c_relat-t_return TRANSPORTING NO FIELDS
      WITH KEY type = 'S'.

    IF sy-subrc IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      lv_lenum = i_lqua-lenum.
      lv_charg = i_lqua-charg.

      SHIFT lv_lenum LEFT DELETING LEADING '0'.
      SHIFT lv_charg LEFT DELETING LEADING '0'.

      IF c_relat-licha IS NOT INITIAL.
        CONCATENATE 'Número de série' c_relat-sernr 'cadastrado com sucesso.'
            'UD lida:' lv_lenum
            '/ Lote:' lv_charg
            '/ Lote fornecedor:' c_relat-licha INTO c_relat-mensg SEPARATED BY space.
      ELSE.
        CONCATENATE 'Número de série' c_relat-sernr 'cadastrado com sucesso.'
            'UD lida:' lv_lenum
            '/ Lote:' lv_charg INTO c_relat-mensg SEPARATED BY space.
      ENDIF.

      c_relat-status = c_green.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      c_relat-status = c_hist.
      c_relat-mensg = text-001.
      RETURN.

    ENDIF.

  ENDMETHOD.                    "call_bapi_objcl_create

ENDCLASS.                    "lcl_integra_ud_serie IMPLEMENTATION


*--------------------------------------------------------------------
* Objetos
*--------------------------------------------------------------------
DATA: go_integra            TYPE REF TO     lcl_integra_ud_serie.
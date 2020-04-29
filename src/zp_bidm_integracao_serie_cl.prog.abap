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
* Classe IntegraUdSerie Definição
*----------------------------------------------------------------------
CLASS lcl_integra_ud_serie DEFINITION.
  PUBLIC SECTION.
    METHODS:
      check_marc,

      processing_data,

      call_transaction_bdc,

      call_bapi_objcl_create.


  PRIVATE SECTION.


*--------------------------------------------------------------------
* Variáveis
*--------------------------------------------------------------------

*--------------------------------------------------------------------
* Tabelas Internas
*--------------------------------------------------------------------
    DATA:
          gt_marc         TYPE TABLE OF   marc      ,
          gt_lqua         TYPE TABLE OF   lqua      ,
          gt_ausp         TYPE TABLE OF   ausp      .


*--------------------------------------------------------------------
* Work areas
*--------------------------------------------------------------------
    DATA:
      gs_itob             TYPE            itob       ,
      gs_lqua             TYPE            lqua       ,
      gs_mseg             TYPE            mseg       ,
      gs_ausp             TYPE            ausp       .

ENDCLASS.                    "lcl_relat DEFINITION


*----------------------------------------------------------------------
* Classe IntegraUdSerie Implementação
*----------------------------------------------------------------------
CLASS lcl_integra_ud_serie IMPLEMENTATION.

  " Verifica se material tem perfil p/ nº de série
  METHOD check_marc.

    SELECT * FROM marc INTO TABLE gt_marc
      WHERE matnr EQ p_matnr
        AND werks EQ p_werks
        AND sernp NE space.

    IF gt_marc[] IS  NOT INITIAL.

      v_matnr = p_matnr.
      v_werks = p_werks.
      CALL SCREEN 9000.

    ELSE.

      MESSAGE s000(cl) WITH 'Material não tem perfil de numero de série' DISPLAY LIKE 'E'.

    ENDIF.

  ENDMETHOD.                    "select_data


  " Verifica se nº de série já foi cadastrado e se tem UD dísponivel
  METHOD processing_data.

    DATA:  lv_tbix  TYPE  sy-tabix,
           lv_atinn TYPE  atinn.

    DATA:  lt_ausp_aux TYPE TABLE OF ausp.

    CLEAR: gs_itob, v_msg1, v_msg2, v_msg3.

    SELECT SINGLE * FROM itob INTO gs_itob
      WHERE sernr EQ v_sernr
        AND matnr EQ p_matnr.

    IF sy-subrc IS NOT INITIAL.

      SELECT * FROM lqua INTO TABLE gt_lqua
        WHERE matnr EQ p_matnr
          AND werks EQ p_werks
          AND lenum NE space.

      IF gt_lqua[] IS NOT INITIAL.

        LOOP AT gt_lqua INTO gs_lqua.
          gs_ausp-atwrt = gs_lqua-lenum.
          APPEND gs_ausp TO lt_ausp_aux. CLEAR gs_ausp.
        ENDLOOP.

        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'UD'
          IMPORTING
            output = lv_atinn.

        SELECT * FROM ausp INTO TABLE gt_ausp
          FOR ALL ENTRIES IN lt_ausp_aux
          WHERE atinn EQ lv_atinn
            AND mafid EQ c_o
            AND klart EQ c_002
            AND atwrt EQ lt_ausp_aux-atwrt.

        IF gt_ausp[] IS NOT INITIAL.

          SORT: gt_lqua BY lenum,
                gt_ausp BY atwrt.

          LOOP AT gt_lqua INTO gs_lqua.

            lv_tbix = sy-tabix.

            CLEAR gs_ausp.
            READ TABLE gt_ausp INTO gs_ausp
              WITH KEY atwrt = gs_lqua-lenum BINARY SEARCH.

            IF sy-subrc IS INITIAL.
              DELETE gt_lqua INDEX lv_tbix.
            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDIF.

      IF gt_lqua[] IS INITIAL.

        v_msg1 = 'UD não disponível'.
        MESSAGE s009(zgam) DISPLAY LIKE 'E'.

      ELSE.

        CLEAR gs_lqua.
        READ TABLE gt_lqua INTO gs_lqua INDEX 1.

        CLEAR gs_mseg.
        SELECT SINGLE * FROM mseg INTO gs_mseg
          WHERE mblnr EQ gs_lqua-wenum
            AND mjahr EQ gs_lqua-wdatu+0(4)
            AND zeile EQ gs_lqua-wepos.

        me->call_transaction_bdc( ).

      ENDIF.

    ELSE.

      CONCATENATE 'Nº de série' v_sernr 'já cadastrado para material' INTO v_msg1 SEPARATED BY space.
      MESSAGE s000(cl) WITH 'Número de série já cadastrado' DISPLAY LIKE 'E'.

    ENDIF.

    CLEAR: v_sernr, v_charg.

  ENDMETHOD.                    "processing_data

  " Criação do número de serie e UD
  METHOD call_transaction_bdc.

    DATA: lt_bdc       TYPE TABLE OF bdcdata,
          lt_msg       TYPE tab_bdcmsgcoll.

    DATA: ls_bdc       TYPE bdcdata       ,
          ls_opt       TYPE ctu_params    ,
          ls_msg       TYPE bdcmsgcoll    .

    "CALL TRANSACTION p/ cadastrar Nº de série
    ls_bdc-program  = 'SAPMIEQ0'.
    ls_bdc-dynpro   = '1000'.
    ls_bdc-dynbegin = 'X'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'BDC_CURSOR'.
    ls_bdc-fval     = 'RISA0-SERNR'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'BDC_OKCODE'.
    ls_bdc-fval     = '/00'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'RISA0-MATNR'.
    ls_bdc-fval     =  p_matnr.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'RISA0-SERNR'.
    ls_bdc-fval     =  v_sernr.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'RM63E-EQTYP'.
    ls_bdc-fval     =  'X'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

*----------------------------------------------------------------------------------
    ls_bdc-program  = 'SAPMIEQ0'.
    ls_bdc-dynpro   = '0101'.
    ls_bdc-dynbegin = 'X'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'BDC_OKCODE'.
    ls_bdc-fval     = '/00'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'BDC_CURSOR'.
    ls_bdc-fval     = 'EQBS-LIFNR'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-LBBSA'.
    ls_bdc-fval     =  '01'. " Estoque utilização livre
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-B_WERK'.
    ls_bdc-fval     =  p_werks.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-B_LAGER'.
    ls_bdc-fval     =  gs_lqua-lgort.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-B_CHARGE'.
    ls_bdc-fval     = gs_lqua-charg.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-LIFNR'.
    ls_bdc-fval     = gs_mseg-lifnr.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

*----------------------------------------------------------------------------------
    ls_bdc-program  = 'SAPMIEQ0'.
    ls_bdc-dynpro   = '0101'.
    ls_bdc-dynbegin = 'X'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'BDC_OKCODE'.
    ls_bdc-fval     = '=EQUI'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'BDC_CURSOR'.
    ls_bdc-fval     = 'EQBS-LIFNR'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-LBBSA'.
    ls_bdc-fval     = '01'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-B_WERK'.
    ls_bdc-fval     = p_werks.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-B_LAGER'.
    ls_bdc-fval     = gs_lqua-lgort.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-B_CHARGE'.
    ls_bdc-fval     = gs_lqua-charg.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'ITOB-CHARGE'.
    ls_bdc-fval     = gs_lqua-charg.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-LIFNR'.
    ls_bdc-fval     = gs_mseg-lifnr.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

*----------------------------------------------------------------------------------
    ls_bdc-program  = 'SAPMIEQ0'.
    ls_bdc-dynpro   = '0101'.
    ls_bdc-dynbegin = 'X'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'BDC_OKCODE'.
    ls_bdc-fval     = '=BU'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'BDC_CURSOR'.
    ls_bdc-fval     = 'EQBS-LIFNR'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-LBBSA'.
    ls_bdc-fval     = '01'.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-B_WERK'.
    ls_bdc-fval     = p_werks.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-B_LAGER'.
    ls_bdc-fval     = gs_lqua-lgort.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-B_CHARGE'.
    ls_bdc-fval     = gs_lqua-charg.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'ITOB-CHARGE'.
    ls_bdc-fval     = gs_lqua-charg.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'EQBS-LIFNR'.
    ls_bdc-fval     = gs_mseg-lifnr.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'ITOB-SHTXT'.
    ls_bdc-fval     = p_matnr.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_bdc-fnam     = 'ITOB-DATAB'.
    WRITE sy-datum USING EDIT MASK '__.__.____' TO ls_bdc-fval.
    APPEND ls_bdc TO lt_bdc. CLEAR ls_bdc.

    ls_opt-dismode  = 'N'.
    ls_opt-defsize  = 'X'.
    ls_opt-updmode  = 'S'.
    ls_opt-racommit = 'X'.

    CALL TRANSACTION 'IQ01'
    USING lt_bdc
    OPTIONS FROM ls_opt
    MESSAGES INTO lt_msg.

    CLEAR ls_msg.
    READ TABLE lt_msg INTO ls_msg
      WITH KEY msgtyp = 'S'.

    IF sy-subrc IS INITIAL.

      me->call_bapi_objcl_create( ).

    ELSE.

      CLEAR ls_msg.
      READ TABLE lt_msg INTO ls_msg
        WITH KEY msgtyp = 'E'.

      IF sy-subrc IS NOT INITIAL.

        CLEAR ls_msg.
        READ TABLE lt_msg INTO ls_msg INDEX 1.

      ENDIF.

      v_msg1 = 'Erro ao cadastrar Nº de série'.

      MESSAGE ID ls_msg-msgid  TYPE ls_msg-msgtyp  NUMBER ls_msg-msgnr
      WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.

    ENDIF.

  ENDMETHOD.                    "output_data

  METHOD call_bapi_objcl_create.

    DATA: lt_allocchar TYPE TABLE OF bapi1003_alloc_values_char,
          lt_return    TYPE TABLE OF bapiret2.

    DATA: ls_return    TYPE bapiret2      ,
          ls_allocchar TYPE bapi1003_alloc_values_char.

    DATA: lv_equnr     TYPE itob-equnr,
          lv_objectkey TYPE bapi1003_key-object.

    SELECT SINGLE equnr FROM itob INTO lv_equnr
    WHERE sernr EQ v_sernr
      AND matnr EQ p_matnr.

    lv_objectkey = lv_equnr.

    ls_allocchar-charact    = 'UD'.
    ls_allocchar-value_char = gs_lqua-lenum.
    APPEND ls_allocchar TO lt_allocchar. CLEAR ls_allocchar.

    IF p_charg IS NOT INITIAL.

      ls_allocchar-charact    = 'LOTE_FORNECEDOR'.
      ls_allocchar-value_char = v_charg.
      APPEND ls_allocchar TO lt_allocchar. CLEAR ls_allocchar.

    ENDIF.

    " Cria classificação e insere UD p/ equipamento
    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        objectkeynew    = lv_objectkey
        objecttablenew  = 'EQUI'
        classnumnew     = 'UDINTEGRAWM'
        classtypenew    = '002'
      TABLES
        allocvalueschar = lt_allocchar
        return          = lt_return.

    READ TABLE lt_return INTO ls_return
      WITH KEY type = 'S'.

    IF sy-subrc IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      CONCATENATE 'Número de série' v_sernr 'cadastrado com sucesso' INTO v_msg1 SEPARATED BY space.
      CONCATENATE 'UD lida' gs_lqua-lenum INTO v_msg2 SEPARATED BY space.
      CONCATENATE 'Lote'    gs_lqua-charg INTO v_msg3 SEPARATED BY space.
      MESSAGE s000(cl) WITH 'Número de série cadastrado' DISPLAY LIKE 'S'.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      CLEAR ls_return.
      READ TABLE lt_return INTO ls_return
        WITH KEY type = 'E'.

      IF sy-subrc IS NOT INITIAL.

        CLEAR ls_return.
        READ TABLE lt_return INTO ls_return INDEX 1.

      ENDIF.

      v_msg1 = ls_return-message.
      MESSAGE ID ls_return-id  TYPE ls_return-type  NUMBER ls_return-number
      WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.

    ENDIF.

  ENDMETHOD.                    "call_bapi_objcl_create

ENDCLASS.                    "lcl_relat IMPLEMENTATION

*--------------------------------------------------------------------
* Objetos
*--------------------------------------------------------------------
DATA: go_integra           TYPE REF TO     lcl_integra_ud_serie.

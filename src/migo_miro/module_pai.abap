*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHE_CELLTAB
*&---------------------------------------------------------------------*
*& Cria tabela de estilos do ALV
*&---------------------------------------------------------------------*
FORM zf_preenche_celltab .

    DATA: ls_fcat       TYPE lvc_s_fcat.
  
    REFRESH t_celltab.
    LOOP AT t_fcat_itm INTO ls_fcat.
      ls_cellrow-fieldname = ls_fcat-fieldname.
  
      CASE ls_fcat-fieldname.
        WHEN 'EBELN' OR 'EBELP' OR 'LGORT' OR 'MATNR'. " campos editaveis
          IF w_relath-processado IS NOT INITIAL. " Se NFe tiver sido processada
            ls_cellrow-style     = cl_gui_alv_grid=>mc_style_disabled.
          ELSE.
            ls_cellrow-style     = cl_gui_alv_grid=>mc_style_enabled.
          ENDIF.
  
        WHEN OTHERS.
          ls_cellrow-style     = cl_gui_alv_grid=>mc_style_disabled.
      ENDCASE.
      INSERT ls_cellrow INTO TABLE t_celltab. CLEAR ls_cellrow.
    ENDLOOP.
  
    PERFORM zf_trata_campos_9201.
  
  ENDFORM.                    " ZF_PREENCHE_CELLTAB


*&---------------------------------------------------------------------*
*&      Form  ZF_MODIFY_CELLTAB
*&---------------------------------------------------------------------*
FORM zf_modify_celltab USING p_field
                             p_flag.

  FIELD-SYMBOLS:
           <fs_celltab> TYPE lvc_s_styl.

  READ TABLE t_celltab ASSIGNING <fs_celltab>
    WITH KEY fieldname = p_field BINARY SEARCH.

  CHECK sy-subrc IS INITIAL AND <fs_celltab> IS ASSIGNED.

  IF p_flag IS NOT INITIAL. " habilita edição
    <fs_celltab>-style = cl_gui_alv_grid=>mc_style_enabled.
  ELSE.
    <fs_celltab>-style = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

ENDFORM.                    " ZF_MODIFY_CELLTAB

*&---------------------------------------------------------------------*
*&      Form  ZF_TRATA_CAMPOS_9201
*&---------------------------------------------------------------------*
*&  Mesma lógica MODULE trata_campos para tela 9201
*&---------------------------------------------------------------------*
FORM zf_trata_campos_9201 .

    FIELD-SYMBOLS: <fs_aux>  TYPE ty_relati.
  
  
    DATA: lv_grupo(3)      TYPE c.
  
    LOOP AT t_relati_aux ASSIGNING <fs_aux>.
  
      IF sy-tcode+07(01) EQ go_xmlin->c_migo.
        IF <fs_aux>-cfop IN go_xmlin->r_cfop_mb1c.
          lv_grupo = 'B1C'.
        ELSE.
          lv_grupo = 'MIR'.
        ENDIF.
      ELSE.
        lv_grupo = 'MIG'.
      ENDIF.
  
      CASE lv_grupo. "desativa campos que não são do grupo
        WHEN 'B1C'.
          PERFORM zf_modify_celltab USING: 'EBELN' '',
                                           'EBELP' '',
                                           'LGORT' ''.
        WHEN 'MIR'.
          PERFORM zf_modify_celltab USING: 'MATNR' '',
                                           'LAGMG' ''.
  
        WHEN 'MIG'.
          PERFORM zf_modify_celltab USING: 'EBELN' '',
                                           'EBELP' '',
                                           'LGORT' '',
                                           'MATNR' '',
                                           'LAGMG' ''.
      ENDCASE.
  
      "Chamado: 54014
      IF w_relath-lgort IS NOT INITIAL.
        PERFORM zf_modify_celltab USING 'LGORT' ''.
      ENDIF.
  
      <fs_aux>-celltab = t_celltab.
  
    ENDLOOP.
  
  ENDFORM.                    " ZF_TRATA_CAMPOS_9201

*&---------------------------------------------------------------------*
*&      Form  ZF_REFRESH_ALV
*&---------------------------------------------------------------------*
*& Atualiza ALV de itens
*&---------------------------------------------------------------------*
FORM zf_refresh_alv .

  DATA: ls_stable TYPE lvc_s_stbl.

  ls_stable = 'XX'.
  o_alv_itm->refresh_table_display( is_stable      = ls_stable
                                    i_soft_refresh = 'X' ).


ENDFORM.                    " ZF_REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  ZF_FILL_TOOLBAR
*&---------------------------------------------------------------------*
FORM zf_fill_toolbar  TABLES  pt_toolbar TYPE ui_functions.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO pt_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO pt_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO pt_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO pt_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_asc.
  APPEND ls_exclude TO pt_toolbar.
  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_dsc.
  APPEND ls_exclude TO pt_toolbar.

ENDFORM.                    " ZF_FILL_TOOLBAR


*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_ITM_FCAT
*&---------------------------------------------------------------------*
FORM zf_monta_itm_fcat .

  REFRESH t_fcat_itm.
  PERFORM zf_preenche_fieldcat USING:
    'NITEM'       'T_RELATI_AUX' 'Item'             ''    'X' ''   ''      ''      'C500' CHANGING t_fcat_itm,
    'CPROD'       'T_RELATI_AUX' 'Produto'          ''    'X' ''   ''      ''      'C500' CHANGING t_fcat_itm,
    'XPROD'       'T_RELATI_AUX' 'Descrição'        ''    ''  ''   ''      ''      'C500' CHANGING t_fcat_itm,
    'NCM'         'T_RELATI_AUX' 'NCM'              ''    ''  ''   ''      ''      'C500' CHANGING t_fcat_itm,
    'CFOP'        'T_RELATI_AUX' 'CFOP'             ''    ''  ''   ''      ''      'C500' CHANGING t_fcat_itm,
    'CST'         'T_RELATI_AUX' 'CST'              ''    ''  ''   ''      ''      'C500' CHANGING t_fcat_itm,
    'QCOM'        'T_RELATI_AUX' 'Qtd.NF '          ''    ''  ''   ''      ''      'C500' CHANGING t_fcat_itm,
    'UCOM'        'T_RELATI_AUX' 'UM'               ''    ''  ''   'MEINS' 'MARA'  'C500' CHANGING t_fcat_itm,
    'QCOM_CONV'   'T_RELATI_AUX' 'Qtd.Convertida '  ''    ''  ''   ''      ''      'C500' CHANGING t_fcat_itm,
    'UCOM_CONV'   'T_RELATI_AUX' 'UM'               ''    ''  ''   'MEINS' 'MARA'  'C500' CHANGING t_fcat_itm,
    'VUNCOM'      'T_RELATI_AUX' 'NFe Vl.Uni.'      ''    ''  ''   ''      ''      'C500' CHANGING t_fcat_itm,
    'ICM_P'       'T_RELATI_AUX' 'NFe % ICMS'       ''    ''  ''   ''      ''      'C500' CHANGING t_fcat_itm,
    'ICM_V'       'T_RELATI_AUX' 'NFe Vlr.ICMS'     ''    ''  ''   ''      ''      'C500' CHANGING t_fcat_itm,
    'VFRETE'      'T_RELATI_AUX' 'Frete NFe'        ''    ''  ''   ''      ''      'C500' CHANGING t_fcat_itm,
    'VPROD'       'T_RELATI_AUX' 'Vlr.Total NFe'    ''    ''  ''   ''      ''      'C500' CHANGING t_fcat_itm,
    'EBELN'       'T_RELATI_AUX' 'Pedido'           'X'   ''  'X'  ''      ''      ''     CHANGING t_fcat_itm,
    'EBELP'       'T_RELATI_AUX' 'Item pedido'      'X'   ''  ''   ''      ''      ''     CHANGING t_fcat_itm,
    'MATNR'       'T_RELATI_AUX' 'Material'         'X'   ''  'X'  'MATNR' 'MARA'  ''     CHANGING t_fcat_itm,
    'LGORT'       'T_RELATI_AUX' 'Dep'              'X'   ''  'X'  ''      ''      ''     CHANGING t_fcat_itm,
    'QPENDENTE'   'T_RELATI_AUX' 'Qtd.Pendente'     ''    ''  ''   ''      ''      ''     CHANGING t_fcat_itm,
    'MENGE'       'T_RELATI_AUX' 'Qtd.Pedido'       ''    ''  ''   ''      ''      ''     CHANGING t_fcat_itm,
    'MEINS'       'T_RELATI_AUX' 'UM básica'        ''    ''  ''   'MEINS' 'MARA'  ''     CHANGING t_fcat_itm,
    'LAGMG'       'T_RELATI_AUX' 'Qtd.Est.Interno'  ''    ''  ''   ''      ''      ''     CHANGING t_fcat_itm,
    'LMEIN'       'T_RELATI_AUX' 'UM básica'        ''    ''  ''   'MEINS' 'MARA'  ''     CHANGING t_fcat_itm,
    'ICMS_PO_P'   'T_RELATI_AUX' '% ICMS Ped.'      ''    ''  ''   ''      ''      ''     CHANGING t_fcat_itm,
    'ICMS_PO_V'   'T_RELATI_AUX' 'Vlr.ICMS Ped.'    ''    ''  ''   ''      ''      ''     CHANGING t_fcat_itm,
    'NETWR'       'T_RELATI_AUX' 'Total líquido'    ''    ''  ''   ''      ''      ''     CHANGING t_fcat_itm,
    'TOTAL_VALUE' 'T_RELATI_AUX' 'Vlr.Total Ped.'   ''    ''  ''   ''      ''      ''     CHANGING t_fcat_itm,
    'EXCH_RATE'   'T_RELATI_AUX' 'Taxa conversão'   ''    ''  ''   ''      ''      ''     CHANGING t_fcat_itm,
    'WWERT'       'T_RELATI_AUX' 'Dt.Conversão'     ''    ''  ''   ''      ''      ''     CHANGING t_fcat_itm,
    'MWSKZ'       'T_RELATI_AUX' 'Cód.imp'          ''    ''  ''   ''      ''      ''     CHANGING t_fcat_itm,
    'V_PO_FRETE'  'T_RELATI_AUX' 'Frete Ped.'       ''    ''  ''   ''      ''      ''     CHANGING t_fcat_itm.

ENDFORM.                    " ZF_MONTA_ITM_FCAT

*&---------------------------------------------------------------------*
*&      Form  ZF_SH_EBELN_9200
*&---------------------------------------------------------------------*
FORM zf_sh_ebeln_9200 USING p_row.

    DATA: lt_ekpo           TYPE TABLE OF   ty_ekpo,
          lt_wyt3           TYPE TABLE OF   ty_wyt3.
  
    DATA: lw_relati         TYPE            ty_relati,
          lw_ekpo           TYPE            ty_ekpo.
  
    FIELD-SYMBOLS:
          <ekpo>            TYPE            ty_ekpo,
          <fs_relati>       TYPE            ty_relati.
  
    DATA: lv_tabix          TYPE            sy-tabix,
          lv_index          TYPE            i.
  
    DATA: lt_return         TYPE TABLE OF ddshretval,
          ls_return         TYPE          ddshretval.
  
  
    REFRESH lt_ekpo.
    SELECT p~ebeln p~ebelp p~loekz p~matnr p~txz01 p~bukrs p~werks p~menge p~meins p~netwr p~netpr p~wepos p~mwskz
           p~umrez p~lmein p~peinh
           t~etenr t~eindt t~menge t~wemng
           k~lifnr l~name1 p~elikz
      FROM ekpo AS p INNER JOIN ekko AS k
        ON p~ebeln = k~ebeln
        INNER JOIN eket AS t
        ON p~ebeln = t~ebeln  AND
           p~ebelp = t~ebelp
        INNER JOIN lfa1 AS l
        ON k~lifnr = l~lifnr
      INTO TABLE lt_ekpo
      WHERE k~lifnr = w_relath-lifnr
        AND p~bukrs = w_relath-bukrs
        AND p~werks = w_relath-werks
        AND p~loekz = space.
  
  * >>> FEdM - 02.07.2015
    REFRESH lt_wyt3.
    SELECT lifnr parvw lifn2 FROM wyt3
      INTO TABLE lt_wyt3
      WHERE lifnr = w_relath-lifnr.
    DELETE lt_wyt3 WHERE parvw NE 'WL'.
  
    IF lt_wyt3[] IS NOT INITIAL.
  
      SORT lt_wyt3 BY lifn2.
      DELETE ADJACENT DUPLICATES FROM lt_wyt3 COMPARING lifn2.
  
      SELECT p~ebeln p~ebelp p~loekz p~matnr p~txz01 p~bukrs p~werks p~menge p~meins p~netwr p~netpr p~wepos p~mwskz
             p~umrez p~lmein p~peinh
             t~etenr t~eindt t~menge t~wemng
             k~lifnr l~name1 p~elikz
        FROM ekpo AS p INNER JOIN ekko AS k
          ON p~ebeln = k~ebeln
          INNER JOIN eket AS t
          ON p~ebeln = t~ebeln  AND
             p~ebelp = t~ebelp
          INNER JOIN lfa1 AS l
          ON k~lifnr = l~lifnr
        APPENDING TABLE lt_ekpo
        FOR ALL ENTRIES IN lt_wyt3
        WHERE k~lifnr = lt_wyt3-lifn2
          AND p~bukrs = w_relath-bukrs
          AND p~werks = w_relath-werks
          AND p~loekz = space.
  
    ENDIF.
  
    DELETE lt_ekpo WHERE elikz EQ abap_true.
  * <<< FEdM - 02.07.2015
  
    IF lt_ekpo[] IS NOT INITIAL.
  
  * >>> FEdM - 03.11.2015
      LOOP AT lt_ekpo ASSIGNING <ekpo>.
        <ekpo>-pendente = <ekpo>-menge_eket - <ekpo>-wemng.
      ENDLOOP.
      DELETE lt_ekpo WHERE pendente LE 0.
      SORT lt_ekpo BY ebeln ebelp etenr.
  * <<< FEdM - 03.11.2015
  
      LOOP AT lt_ekpo ASSIGNING <ekpo>.
        <ekpo>-tabix = sy-tabix.
      ENDLOOP.
  
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'TABIX' " retorna tabix p/ ler lt_ekpo
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          value_org       = 'S'
        TABLES
          value_tab       = lt_ekpo
          return_tab      = lt_return
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
  
      IF lt_return IS NOT INITIAL.
  
        READ TABLE lt_return INTO ls_return INDEX 1.
  
        TRANSLATE ls_return-fieldval USING '. '.
        CONDENSE ls_return-fieldval NO-GAPS.
  
        READ TABLE lt_ekpo INTO lw_ekpo
          WITH KEY tabix = ls_return-fieldval.
  
        READ TABLE t_relati_aux ASSIGNING <fs_relati> INDEX p_row.
        CHECK <fs_relati> IS ASSIGNED.
  
        <fs_relati>-modif       = abap_true.
        <fs_relati>-ebeln       = lw_ekpo-ebeln.
        <fs_relati>-ebelp       = lw_ekpo-ebelp.
        <fs_relati>-matnr       = lw_ekpo-matnr.
        <fs_relati>-mwskz       = lw_ekpo-mwskz.
        <fs_relati>-qpendente   = go_xmlin->get_quantity( <fs_relati> ).
        <fs_relati>-menge       = lw_ekpo-menge.
        <fs_relati>-meins       = lw_ekpo-meins.
        <fs_relati>-netpr       = lw_ekpo-netpr.
        <fs_relati>-umrez       = lw_ekpo-umrez.
        <fs_relati>-lmein       = lw_ekpo-lmein.
  
        <fs_relati>-ucom_conv = go_xmlin->convert_unit_to_internal( i_relath = w_relath
                                                                    i_relati = <fs_relati> ).
  
        go_xmlin->qcom_convertion(
          EXPORTING
            i_relati    = <fs_relati>
          CHANGING
            c_qcom_conv   = <fs_relati>-qcom_conv
            c_ucom_conv   = <fs_relati>-ucom_conv
            c_qcom_conv2  = <fs_relati>-qcom_conv2 ).
  
  
        <fs_relati>-lagmg       = go_xmlin->get_menge_convertion( <fs_relati> ).
        <fs_relati>-peinh       = lw_ekpo-peinh.
  
        go_xmlin->calculate_tax_item( <fs_relati> ).
  
        <fs_relati>-wwert       = go_xmlin->get_exch_date( i_relath = w_relath
                                                           i_relati = <fs_relati> ).
        <fs_relati>-exch_rate   = go_xmlin->get_exch_rate( i_relath = w_relath
                                                           i_relati = <fs_relati> ).
  
        <fs_relati>-total_value = go_xmlin->get_item_total_value( i_relath = w_relath
                                                                  i_relati = <fs_relati> ).
        <fs_relati>-v_po_frete  = go_xmlin->get_po_item_freight( i_relati  = <fs_relati> i_with_ipi = abap_true ).
        <fs_relati>-icms_po_p   = go_xmlin->get_item_icms_tax( <fs_relati> ).
        <fs_relati>-icms_po_v   = go_xmlin->get_item_icms_value( <fs_relati> ).
  
        <fs_relati>-waers       = go_xmlin->get_currency( <fs_relati> ).
  
  
        <fs_relati>-netwr       = go_xmlin->get_item_total_net_value( <fs_relati> ).
        CLEAR <fs_relati>-lgort.
        <fs_relati>-lgort       = go_xmlin->get_lgort( <fs_relati> ).
  
      ENDIF.
  
    ELSE.
      MESSAGE s000(cl) WITH 'Nenhum pedido de compras encontrado' 'para o fornecedor'
      DISPLAY LIKE 'E'.
    ENDIF.
  
  ENDFORM.                    " ZF_SH_EBELN_9200

  *&---------------------------------------------------------------------*
*&      Form  ZF_SH_LGORT
*&---------------------------------------------------------------------*
FORM zf_sh_lgort  USING p_row.

  DATA: lt_return TYPE TABLE OF ddshretval,
        ls_return TYPE ddshretval.

  FIELD-SYMBOLS:
      <fs_relati> TYPE ty_relati.

  SELECT lgort lgobe FROM t001l INTO TABLE t_deposito
    WHERE werks EQ w_relath-werks.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LGORT'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      value_org       = 'S'
    TABLES
      value_tab       = t_deposito
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF lt_return IS NOT INITIAL.

    READ TABLE lt_return INTO ls_return INDEX 1.
    READ TABLE t_relati_aux ASSIGNING <fs_relati> INDEX p_row.
    CHECK <fs_relati> IS ASSIGNED.

    CLEAR ls_cellrow.
    READ TABLE <fs_relati>-celltab INTO ls_cellrow WITH KEY fieldname = 'LGORT'.

    "verifica se coluna está habilitada para edição
    CHECK ls_cellrow-style EQ cl_gui_alv_grid=>mc_style_enabled.
    <fs_relati>-lgort = ls_return-fieldval.
    <fs_relati>-modif = abap_true.
    v_modif           = abap_true.

  ENDIF.

ENDFORM.                    " ZF_SH_LGORT

*&---------------------------------------------------------------------*
*&      Form  ZF_SH_MWSKZ
*&---------------------------------------------------------------------*
FORM zf_sh_mwskz USING p_row.

    DATA: lt_return TYPE TABLE OF ddshretval,
          ls_return TYPE ddshretval.
  
    FIELD-SYMBOLS <fs_relati> TYPE ty_relati.
  
    SELECT mwskz text1 INTO CORRESPONDING FIELDS OF TABLE t_iva
     FROM t007s
     WHERE spras EQ sy-langu
       AND kalsm EQ 'TAXBRA'.
  
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield   = 'MWSKZ'
        dynpprog   = sy-repid
        dynpnr     = sy-dynnr
        value_org  = 'S'
      TABLES
        value_tab  = t_iva
        return_tab = lt_return.
  
    IF lt_return IS NOT INITIAL.
  
      READ TABLE lt_return INTO ls_return INDEX 1.
      READ TABLE t_relati_aux ASSIGNING <fs_relati> INDEX p_row.
      CHECK <fs_relati> IS ASSIGNED.
      <fs_relati>-mwskz = ls_return-fieldval.
  
    ENDIF.
  
  ENDFORM.                    " ZF_SH_MWSKZ
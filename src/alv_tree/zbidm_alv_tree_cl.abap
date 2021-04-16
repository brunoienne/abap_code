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


*----------------------------------------------------------------------
* Classe relatório Definição
*----------------------------------------------------------------------
CLASS lcl_relat DEFINITION.
    PUBLIC SECTION.
      METHODS:
        select_data,
  
        processing_data,
  
        output_data_tree,
  
        get_data_row
          IMPORTING
            i_node     TYPE REF TO cl_salv_node
          RETURNING
            value(relat_tree) TYPE relat_tree_type,
  
        get_parent
          IMPORTING
            i_node    TYPE REF TO cl_salv_node
          RETURNING
           value(r_tree) TYPE relat_tree_type,
  
        delete_marca
          IMPORTING
            i_node      TYPE REF TO cl_salv_node,
  
  
        on_user_command FOR EVENT added_function OF cl_salv_events
          IMPORTING
            e_salv_function,
  
        set_key
          IMPORTING
            lo_column_list  TYPE REF TO     cl_salv_columns_table.
  
    PRIVATE SECTION.
  
      METHODS:
        add_items_node
          IMPORTING
            i_marca_key     TYPE salv_de_node_key
          RETURNING
            value(r_key)    TYPE salv_de_node_key,
  
        add_header_node
          IMPORTING
            i_root_key      TYPE salv_de_node_key
          RETURNING
            value(r_key)    TYPE salv_de_node_key,
  
        add_root_node
          RETURNING
            value(r_key)    TYPE salv_de_node_key,
  
        as_text_node
          IMPORTING
            i_text          TYPE any
          RETURNING
            value(r_text)   TYPE lvc_value,
  
       add_item_node
        IMPORTING
          i_item_key        TYPE salv_de_node_key.
  
  *--------------------------------------------------------------------
  * Objetos
  *--------------------------------------------------------------------
      DATA:
        co_salv_tree          TYPE REF TO     cl_salv_tree    ,
        co_salv_nodes         TYPE REF TO     cl_salv_nodes   .
  
  
  *--------------------------------------------------------------------
  * Tabelas Internas
  *--------------------------------------------------------------------
      DATA:
        ct_veiculo            TYPE TABLE OF   veiculo_type    ,
        ct_total              TYPE TABLE OF   total_type      ,
        ct_ano_marca          TYPE TABLE OF   ano_marca       ,
        ct_marca              TYPE TABLE OF   marca_type      .
  
  *--------------------------------------------------------------------
  * Work areas
  *--------------------------------------------------------------------
      DATA:
        cs_veiculo            TYPE            veiculo_type    ,
        cs_marca              TYPE            marca_type      ,
        cs_relat_tree         TYPE            relat_tree_type ,
        cs_ano_marca          TYPE            ano_marca       ,
        cs_total              TYPE            total_type      ,
        cs_variant            TYPE            disvariant      .
  
  
  ENDCLASS.                    "lcl_relat DEFINITION
  
  
  *----------------------------------------------------------------------
  * Classe relatório Implementação
  *----------------------------------------------------------------------
  CLASS lcl_relat IMPLEMENTATION.
  
    " Seleção de dados
    METHOD select_data.
  
      SELECT ano
             marca
             placa
             modelo
             categoria
             bloqueado
             valor
      FROM ztb_veiculos_20 INTO TABLE ct_veiculo
        WHERE categoria IN s_categ
          AND ano       IN s_ano.
  
      SELECT marca
             nome
             pais
      FROM ztb_marcas_20 INTO TABLE ct_marca
      FOR ALL ENTRIES IN ct_veiculo
         WHERE marca EQ ct_veiculo-marca.
  
    ENDMETHOD.                    "select_data
  
  
    " Tratamento dos dados selecionados
    METHOD processing_data.
  
      SORT: ct_veiculo    BY ano marca placa ,
            ct_ano_marca  BY ano marca       ,
            ct_marca      BY marca           ,
            ct_total      BY ano             .
  
      LOOP AT ct_veiculo INTO cs_veiculo.
        cs_total-ano   = cs_veiculo-ano.
        cs_total-total = cs_veiculo-valor.
  
        cs_ano_marca-ano   = cs_veiculo-ano.
        cs_ano_marca-marca = cs_veiculo-marca.
        cs_ano_marca-total = cs_veiculo-valor.
        COLLECT cs_total INTO ct_total. CLEAR cs_total.
        COLLECT cs_ano_marca INTO ct_ano_marca. CLEAR cs_ano_marca.
      ENDLOOP.
  
  
    ENDMETHOD.                    "processing_data
  
    METHOD on_user_command.
  
      DATA:
           lo_selections_tree  TYPE REF TO     cl_salv_selections_tree,
           lo_nodes            TYPE REF TO     cl_salv_nodes,
           lo_node             TYPE REF TO     cl_salv_node,
           lo_item             TYPE REF TO     cl_salv_item,
           lo_columns          TYPE REF TO     cl_salv_columns.
  
      DATA:
           lt_nodes            TYPE            salv_t_nodes.
  
      DATA:
           ls_node             TYPE            salv_s_nodes,
           ls_data             TYPE REF TO     data.
  
      DATA:
           lv_key              TYPE            lvc_nkey,
           lv_selected         TYPE            abap_bool.
  
      lo_nodes = co_salv_tree->get_nodes( ).
  *    lv_key   = 1.
  *    lo_node  = lo_nodes->get_node( lv_key ). " primeira raiz
  *    lt_nodes = lo_node->get_children( ).     " pega os nós da primeira raiz
      lt_nodes =  lo_nodes->get_all_nodes( ).
  
  
      LOOP AT lt_nodes INTO ls_node.
  
        lo_item = ls_node-node->get_hierarchy_item( ).
  
        CASE e_salv_function.
          WHEN '_SELALL'.
            lo_item->set_checked( abap_true ).
  
          WHEN '_DESALL'.
            lo_item->set_checked( abap_false ).
  
          WHEN '_EDIT'.
            IF lo_item->is_checked( ) EQ abap_true.
              cs_relat_tree = get_data_row( ls_node-node ).
            ENDIF.
  
          WHEN '_DEL'.
            IF lo_item->is_checked( ) EQ abap_true.
              delete_marca( ls_node-node ).
            ENDIF.
  
        ENDCASE.
  
      ENDLOOP.
  
      lo_columns = co_salv_tree->get_columns( ).
      lo_columns->set_optimize( abap_true ).
  
    ENDMETHOD.                    "on_user_command
  
  
    METHOD output_data_tree.
  
      DATA:
       lo_node             TYPE REF TO     cl_salv_node           ,
       lo_columns          TYPE REF TO     cl_salv_columns        ,
       lo_column           TYPE REF TO     cl_salv_column         ,
       lo_column_tree      TYPE REF TO     cl_salv_column_tree    ,
       lo_tree_settings    TYPE REF TO     cl_salv_tree_settings  ,
       lo_item             TYPE REF TO     cl_salv_item           ,
       lo_functions        TYPE REF TO     cl_salv_functions_tree ,
       lo_lout             TYPE REF TO     cl_salv_layout         ,
       lo_events           TYPE REF TO     cl_salv_events_tree    .
  
      DATA:
        lt_empty           TYPE TABLE OF   relat_tree_type        .
  
      DATA:
        li_ano_key         TYPE            salv_de_node_key       ,
        li_marca_key       TYPE            salv_de_node_key       ,
        li_carros_key      TYPE            salv_de_node_key       .
  
      TRY.
  
  *       1. Create instance with an empty table
          CALL METHOD cl_salv_tree=>factory
            IMPORTING
              r_salv_tree = co_salv_tree
            CHANGING
              t_table     = lt_empty.
  
  *       2. Add the nodes to the tree and set their relations
          co_salv_nodes = co_salv_tree->get_nodes( ).
  
          LOOP AT ct_total INTO cs_total.
  
            " raiz
            li_ano_key = add_root_node( ).
  
            READ TABLE ct_veiculo TRANSPORTING NO FIELDS
              WITH KEY ano = cs_total-ano BINARY SEARCH.
  
            IF sy-subrc IS INITIAL.
  
              LOOP AT ct_veiculo INTO cs_veiculo FROM sy-tabix.
                IF cs_veiculo-ano <> cs_total-ano.
                  EXIT.
                ENDIF.
  
                AT NEW marca.
                  CLEAR cs_marca.
                  READ TABLE ct_marca INTO cs_marca
                    WITH KEY marca = cs_veiculo-marca BINARY SEARCH.
  
                  CLEAR cs_ano_marca.
                  READ TABLE ct_ano_marca INTO cs_ano_marca
                    WITH KEY ano   = cs_veiculo-ano
                             marca = cs_marca-marca BINARY SEARCH.
                  " cabeçalho
                  li_marca_key  = add_header_node( li_ano_key ).
                  "txt. "Itens"
                  li_carros_key = add_items_node( li_marca_key ).
                ENDAT.
  
                "item
                add_item_node( li_carros_key ).
              ENDLOOP.
  
            ENDIF.
  
          ENDLOOP.
  
  *     Do some final format tasks
          lo_columns = co_salv_tree->get_columns( ).
          lo_columns->set_optimize( abap_true ).
  
          " Column on the left side not showed
          lo_column = lo_columns->get_column( 'NODE' ).
          lo_column->set_visible( abap_false ).
  
          lo_column = lo_columns->get_column( 'MARCA' ).
          lo_column->set_visible( abap_false ).
  
          lo_column = lo_columns->get_column( 'ANO' ).
          lo_column->set_visible( abap_false ).
  
          lo_column = lo_columns->get_column( 'NOME' ).
          lo_column->set_long_text( 'Marca' ).
          lo_column->set_medium_text( 'Marca' ).
          lo_column->set_short_text( 'Marca' ).
  
          lo_column = lo_columns->get_column( 'BLOQUEADO' ).
          lo_column->set_long_text( 'Bloqueado' ).
          lo_column->set_medium_text( 'Bloqueado' ).
          lo_column->set_short_text( 'Bloqueado' ).
  
          lo_column = lo_columns->get_column( 'VALOR' ).
          lo_column->set_long_text( 'Valor' ).
          lo_column->set_medium_text( 'Valor' ).
          lo_column->set_short_text( 'Valor' ).
  
          lo_column = lo_columns->get_column( 'MODELO' ).
          lo_column->set_long_text( 'Modelo' ).
          lo_column->set_medium_text( 'Modelo' ).
          lo_column->set_short_text( 'Modelo' ).
  
          lo_tree_settings = co_salv_tree->get_tree_settings( ).
          lo_tree_settings->set_hierarchy_header( 'Ano/Marca/Carros' ).
          lo_tree_settings->set_hierarchy_size( 30 ).
          lo_tree_settings->set_hierarchy_icon( '@3M@' ).
  
  *… register to the events of cl_salv_table
          lo_events = co_salv_tree->get_event( ).
  
  *… register to the event USER_COMMAND
          SET HANDLER on_user_command FOR lo_events.
  
          co_salv_tree->set_screen_status(
            EXPORTING
              report        = sy-repid
              pfstatus      = 'ZGUI_ALV_TREE'
              set_functions = co_salv_tree->c_functions_all
          ).
  
  *       Display Table
          co_salv_tree->display( ).
  
        CATCH cx_salv_error.
  
      ENDTRY.
  
    ENDMETHOD.                    "output_data_tree
  
  
    METHOD get_data_row.
  
      DATA: ls_data TYPE REF TO data.
  
      FIELD-SYMBOLS: <fs_data> TYPE any.
  
      ls_data = i_node->get_data_row( ).
      ASSIGN ls_data->* TO <fs_data>.
  
      relat_tree = <fs_data>.
  
    ENDMETHOD.                    "get_data_row
  
    METHOD get_parent.
  
      FIELD-SYMBOLS:
        <fs_data> TYPE any.
  
      DATA: ls_node TYPE REF TO cl_salv_node,
            ls_data TYPE REF TO data.
  
      ls_node = i_node->get_parent( ).
      ls_data = ls_node->get_data_row( ).
      ASSIGN ls_data->* TO <fs_data>.
  
      r_tree = <fs_data>.
  
    ENDMETHOD.                    "get_parent
  
    METHOD delete_marca.
  
      DATA: lv_tot        TYPE ztb_veiculos_20-valor,
            lv_exc        TYPE ztb_veiculos_20-valor,
            lo_pai        TYPE REF TO cl_salv_node,
            ls_relat_tree TYPE relat_tree_type.
  
      FIELD-SYMBOLS:
          <fs_tree>       TYPE relat_tree_type.
  
      cs_relat_tree = get_data_row( i_node ). " dados nó a ser excluido
      ls_relat_tree = get_parent( i_node ).   " nó pai (ano)
  
      LOOP AT ct_veiculo INTO cs_veiculo.
        CHECK cs_veiculo-marca EQ cs_relat_tree-marca AND
              cs_veiculo-ano   EQ cs_relat_tree-ano.
        lv_exc = lv_exc + cs_veiculo-valor.
      ENDLOOP.
  
      lv_tot = ls_relat_tree-valor.
  
      UNASSIGN <fs_tree>.
      ASSIGN ls_relat_tree TO <fs_tree>.
      IF <fs_tree> IS ASSIGNED.
        <fs_tree>-valor = lv_tot - lv_exc.
      ENDIF.
  
      lo_pai = i_node->get_parent( ).
      lo_pai->set_data_row( <fs_tree> ).
  
      DELETE ct_veiculo WHERE marca = cs_relat_tree-marca AND
                              ano   = cs_relat_tree-ano.
  
      i_node->delete(
        EXPORTING
          update_parent_expander = abap_true
          update_parent_folder   = abap_true
      ).
  
    ENDMETHOD.                    "delete_marca
  
  
    " Set coluna chave
    METHOD set_key.
  
      DATA: lo_columns      TYPE REF TO     cl_salv_column_table.
  
      DATA: lt_columns      TYPE            salv_t_column_ref.
  
      DATA: ls_column       TYPE            salv_s_column_ref.
  
      lt_columns = lo_column_list->get( ).
  
      LOOP AT lt_columns INTO ls_column.
  
        CHECK ls_column-columnname  EQ  'BUKRS' OR
              ls_column-columnname  EQ  'BUPLA'.
  
        TRY .
            lo_columns  ?= lo_column_list->get_column( ls_column-columnname ).
          CATCH cx_salv_not_found.
        ENDTRY.
  
        TRY .
          lo_columns->set_key( abap_true ).
        ENDTRY.
  
      ENDLOOP.
  
  
    ENDMETHOD.                    "set_key
  
    METHOD add_root_node.
  
      DATA:
        lo_node             TYPE REF TO     cl_salv_node.
  
  *   add the node as root
      lo_node = co_salv_nodes->add_node(
          related_node   = ''
          relationship   = if_salv_c_node_relation=>parent
          text           = me->as_text_node( cs_total-ano )
          expander       = abap_true
          folder         = abap_true
      ).
  
  * expanded node by default
      lo_node->expand( ).
  
  *… set the icon in the icon column
      lo_node->set_collapsed_icon( cc_icon_positive ). "icon_okay
      lo_node->set_expanded_icon( cc_icon_date ).      "icon_cancel
  
      CLEAR cs_relat_tree.
      cs_relat_tree-valor = cs_total-total.
      cs_relat_tree-ano   = cs_total-ano.
      lo_node->set_data_row( cs_relat_tree ). " exibe os dados no nível do nó
  
      r_key = lo_node->get_key( ).
  
    ENDMETHOD.                    "add_root_node
  
    METHOD as_text_node.
      WRITE i_text TO r_text.
    ENDMETHOD.                    "as_text_node
  
    METHOD add_header_node.
  
      DATA:lo_node             TYPE REF TO     cl_salv_node,
           lo_item             TYPE REF TO     cl_salv_item.
  
      lo_node = co_salv_nodes->add_node(
       related_node = i_root_key
       relationship = if_salv_c_node_relation=>first_child
       text         = me->as_text_node( cs_marca-marca )
       folder       = abap_false ).
  
      CLEAR cs_relat_tree.
      cs_relat_tree-nome  = cs_marca-nome.
      cs_relat_tree-ano   = cs_total-ano.
      cs_relat_tree-marca = cs_marca-marca.
      cs_relat_tree-valor = cs_ano_marca-total.
  
      r_key = lo_node->get_key( ).
  
      lo_node->set_data_row( cs_relat_tree ).
  
  *   set node as checkbox
      lo_item = lo_node->get_hierarchy_item( ).
      lo_item->set_type( if_salv_c_item_type=>checkbox ).
      lo_item->set_editable( abap_true ).
  
  * set the icon in the icon column
      lo_node->set_collapsed_icon( cc_icon_positive ).
      lo_node->set_expanded_icon( cc_icon_negative ).
  
    ENDMETHOD.                    "add_header_node
  
    METHOD add_items_node.
  
      DATA:
         lo_node       TYPE REF TO     cl_salv_node.
  
      " Itens
      lo_node = co_salv_nodes->add_node(
        related_node = i_marca_key
        relationship = if_salv_c_node_relation=>first_child
        text         = me->as_text_node( 'Carros' )
        folder       = abap_false ).
  
      r_key = lo_node->get_key( ).
  
      lo_node->set_collapsed_icon( cc_icon_positive ).
      lo_node->set_expanded_icon( cc_icon_negative ).
  
    ENDMETHOD.                    "add_items_node
  
    METHOD add_item_node.
  
      DATA:
       lo_node             TYPE REF TO     cl_salv_node,
       lo_node_itens       TYPE REF TO     cl_salv_node,
       lo_node_pedido      TYPE REF TO     cl_salv_node,
       lo_nodes            TYPE REF TO     cl_salv_nodes,
       lo_item             TYPE REF TO     cl_salv_item.
  
      lo_node = co_salv_nodes->add_node(
       related_node = i_item_key
       relationship = if_salv_c_node_relation=>first_child
       text         = me->as_text_node( cs_veiculo-placa )
       folder       = abap_false ).
  
      CLEAR cs_relat_tree.
      cs_relat_tree-modelo    = cs_veiculo-modelo.
      cs_relat_tree-categoria = cs_veiculo-categoria.
      cs_relat_tree-bloqueado = cs_veiculo-bloqueado.
      cs_relat_tree-valor     = cs_veiculo-valor.
      cs_relat_tree-ano       = cs_veiculo-ano.
      cs_relat_tree-marca     = cs_marca-marca.
  
      lo_node->set_collapsed_icon( cc_icon_car ).
  
      lo_node->set_data_row( cs_relat_tree ).
  
    ENDMETHOD.                    "add_item_node
  
  ENDCLASS.                    "lcl_relat IMPLEMENTATION
  
  *--------------------------------------------------------------------
  * Objetos
  *--------------------------------------------------------------------
  DATA: go_relat            TYPE REF TO     lcl_relat.
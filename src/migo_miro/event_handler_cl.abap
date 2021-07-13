*&---------------------------------------------------------------------*
*&  Include           ZPMMI005_HANDLER
*&---------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.

    PUBLIC SECTION.
  
      METHODS:
          handle_on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
            IMPORTING e_fieldname
                      es_row_no
                      er_event_data,
  
          handle_data_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
            IMPORTING e_modified
                      et_good_cells.
  
  ENDCLASS.                    "lcl_event_handler DEFINITION
  
  *----------------------------------------------------------------------*
  *       CLASS lcl_event_handler IMPLEMENTATION
  *----------------------------------------------------------------------*
  CLASS lcl_event_handler IMPLEMENTATION.
  
    METHOD handle_on_f4.
  
      CASE e_fieldname.
  
  *      WHEN 'MWSKZ'.
  *        PERFORM zf_sh_mwskz USING es_row_no-row_id.
  
        WHEN 'EBELN'.
          PERFORM zf_sh_ebeln_9200 USING es_row_no-row_id.
  
        WHEN 'LGORT'.
          PERFORM zf_sh_lgort USING es_row_no-row_id.
  
      ENDCASE.
  
      PERFORM zf_refresh_alv.
  
      " avoid standard search help
      er_event_data->m_event_handled = 'X'.
  
  
    ENDMETHOD.                    "handle_on_f4
  
  
    METHOD handle_data_finished.
  
      DATA: ls_cell  TYPE lvc_s_modi.
  
      CHECK e_modified EQ abap_true.
  
      LOOP AT et_good_cells INTO ls_cell.
  
        CLEAR w_relati.
        READ TABLE t_relati_aux INTO w_relati INDEX ls_cell-row_id.
  
        CASE ls_cell-fieldname.
          WHEN 'EBELN'.
            PERFORM zf_check_ebeln_9200 USING ls_cell-row_id.
          WHEN 'EBELP'.
            PERFORM zf_check_ebelp_9200 USING ls_cell-row_id.
          WHEN 'LGORT'.
            PERFORM zf_check_lgort USING ls_cell-row_id.
          WHEN 'MATNR'.
            PERFORM zf_check_material USING w_relati ls_cell-row_id.
        ENDCASE.
  
        IF ls_cell-fieldname <> 'LGORT'.
          PERFORM zf_check_pedido_9201 USING ls_cell-row_id.
        ENDIF.
  
      ENDLOOP.
  
      PERFORM zf_refresh_alv.
  
    ENDMETHOD.                    "handle_data_finished
  
  ENDCLASS.                    "lcl_event_handler IMPLEMENTATION
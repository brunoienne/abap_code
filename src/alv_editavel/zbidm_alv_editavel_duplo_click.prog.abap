*&---------------------------------------------------------------------*
*& Report  ZTESTE_BIDM_004
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zbidm_alv_editavel_duplo_click.

*&---------------------------------------------------------------------*
*& Tipos
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

TABLES: ztb_veiculos_20.

TYPES: BEGIN OF selfield_ty,
          index TYPE slis_selfield-tabindex,
       END OF selfield_ty,

       BEGIN OF veiculo_ty,
          mandt       TYPE  ztb_veiculos_20-mandt,
          placa       TYPE  ztb_veiculos_20-placa,
          marca       TYPE  ztb_veiculos_20-marca,
          modelo      TYPE  ztb_veiculos_20-modelo,
          ano         TYPE  ztb_veiculos_20-ano,
          categoria   TYPE  ztb_veiculos_20-categoria,
          bloqueado   TYPE  ztb_veiculos_20-bloqueado,
       END OF veiculo_ty.

*&---------------------------------------------------------------------*
*& Estruturas e tabelas internas
*&---------------------------------------------------------------------*
DATA:
      it_out      TYPE TABLE OF veiculo_ty,  " tbl interna
      it_index    TYPE TABLE OF selfield_ty, " tbl com indices
      it_fieldcat TYPE TABLE OF slis_fieldcat_alv, " recebe colunas da tabela
      st_layout   TYPE slis_layout_alv, " estrutura de layout
      st_out      TYPE veiculo_ty, " workarea interna
      st_index    TYPE selfield_ty. " workarea de indices

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
SELECT-OPTIONS s_placa FOR ztb_veiculos_20-placa.
SELECT-OPTIONS s_marca FOR ztb_veiculos_20-marca.
SELECT-OPTIONS s_cat   FOR ztb_veiculos_20-categoria.
SELECT-OPTIONS s_ano   FOR ztb_veiculos_20-ano.
SELECTION-SCREEN END OF BLOCK b01.
*&---------------------------------------------------------------------*
*& Inicio da seleção
*&---------------------------------------------------------------------

START-OF-SELECTION.

  PERFORM: f_select,
           f_alv.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  f_select
*&---------------------------------------------------------------------*
FORM f_select .

  SELECT mandt placa marca modelo ano categoria bloqueado
    FROM ztb_veiculos_20
    INTO TABLE it_out
    WHERE placa IN s_placa
    AND marca IN s_marca
    AND ano IN s_ano
    AND categoria IN s_cat.

ENDFORM.                    "f_select


*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
FORM f_alv .

  PERFORM: f_fieldcat, " retorna informações de tds campos
           f_layout.

  PERFORM: f_relatorio
  TABLES
    it_fieldcat " tbl com catalogo de campos
    it_out " tbl interna
    USING sy-repid
     'F_SET_PF_STATUS'
     'F_USER_COMMAND'
     st_layout.

ENDFORM.                    " F_ALV

*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZTB_VEICULOS_20'
    CHANGING
      ct_fieldcat            = it_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM.                    " F_FIELDCAT


*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
*&---------------------------------------------------------------------*
FORM f_layout .

  st_layout-zebra             = 'X'.
  st_layout-colwidth_optimize = 'X'.

ENDFORM.                    " F_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  F_RELATORIO
*&---------------------------------------------------------------------*
FORM f_relatorio  TABLES fieldcat
                         outtab
                  USING  repid
                         status
                         command
                         layout.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = repid
      i_callback_pf_status_set = status
      i_callback_user_command  = command
      is_layout                = layout
      it_fieldcat              = fieldcat[]
      i_save                   = 'X'
    TABLES
      t_outtab                 = outtab
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.




ENDFORM.                    " F_RELATORIO



*&---------------------------------------------------------------------*
*&      Form  F_SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM f_set_pf_status USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'STANDARD'.

ENDFORM.                    "F_SET_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
FORM f_user_command USING vl_ucomm    LIKE sy-ucomm
                          st_selfield TYPE slis_selfield.

  DATA:
        it_out_temp       TYPE TABLE OF ztb_veiculos_20,
        st_out_temp       TYPE ztb_veiculos_20,
        it_fieldcat_temp  TYPE TABLE OF slis_fieldcat_alv,
        st_fieldcat_temp  TYPE slis_fieldcat_alv.

  CASE vl_ucomm.

    WHEN '&IC1'. " duplo clique

      it_fieldcat_temp      = it_fieldcat.
      st_fieldcat_temp-edit = 'X'.
      MODIFY it_fieldcat_temp FROM st_fieldcat_temp
      TRANSPORTING edit
      WHERE key IS INITIAL.

      READ TABLE it_out INTO st_out
      INDEX st_selfield-tabindex.   " lê a linha que foi clicada e add na tbl
      APPEND st_out TO it_out_temp.

      PERFORM: f_relatorio TABLES it_fieldcat_temp  " tbl com colunas
                                  it_out_temp " linha clicada
                           USING  sy-repid
                                  ' '
                                  ' '
                                 st_layout.
      READ TABLE it_out_temp INTO st_out_temp
      INDEX 1.

      IF st_out_temp <> st_out. " se o vl da linha for diferente atualiza
        MODIFY it_out FROM st_out_temp
        INDEX st_selfield-tabindex.

        MOVE st_selfield-tabindex TO st_index-index.
        APPEND st_index TO it_index.
      ENDIF.

      st_selfield-refresh = 'X'.


    WHEN 'SAVE'.

      IF it_index IS NOT INITIAL.

        LOOP AT it_index INTO st_index.
          READ TABLE it_out INTO st_out
          INDEX st_index-index.

          MOVE-CORRESPONDING st_out TO st_out_temp.
          APPEND st_out_temp TO it_out_temp.

        ENDLOOP.

        MODIFY ztb_veiculos_20 FROM TABLE it_out_temp.
        MESSAGE 'Tabela de veículos alterada com sucesso' TYPE 'S'.

      ELSE.
        MESSAGE 'Tabela não teve alteração' TYPE 'W'.
      ENDIF.

    WHEN 'BACK' OR 'EXIT'.
      LEAVE PROGRAM.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDFORM.                    "f_user_command

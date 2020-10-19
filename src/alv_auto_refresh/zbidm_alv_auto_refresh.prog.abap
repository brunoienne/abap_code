*&---------------------------------------------------------------------*
*& Report  ZTESTE_BIDM_017
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zbidm_alv_auto_refresh.

DATA:
      i_data     TYPE TABLE OF spfli          ,
      o_grid     TYPE REF TO cl_gui_alv_grid  ,
      timer      TYPE REF TO cl_gui_timer     . " instância do temporizador

FIELD-SYMBOLS <fs_data> LIKE LINE OF i_data.


*----------------------------------------------------------------------*
*       CLASS cl_timer - Define quais metodos
*----------------------------------------------------------------------*
CLASS lcl_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
    run_handler FOR EVENT finished OF cl_gui_timer   , " método para registrar evento terminado de cl_gui_timer

    refresh_table                                    .

ENDCLASS.                    "cl_timer

DATA     receiver   TYPE REF TO lcl_receiver     . " instância da classe definida

*----------------------------------------------------------------------*
*       CLASS cl_timer IMPLEMENTATION - Descreve os metodos
*----------------------------------------------------------------------*
CLASS lcl_receiver IMPLEMENTATION.

  METHOD run_handler.

    me->refresh_table( ). " atualiza tabela

    timer->run( ). " inicia timer

  ENDMETHOD.                    "run_handler

  METHOD refresh_table.

    LOOP AT i_data ASSIGNING <fs_data>.
      <fs_data>-period = <fs_data>-period + 1.
    ENDLOOP.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = o_grid.

    IF o_grid IS NOT INITIAL.
      o_grid->refresh_table_display( ).
    ENDIF.


  ENDMETHOD.                    "refresh_table
ENDCLASS.                    "cl_timer IMPLEMENTATION

INITIALIZATION.
  CREATE OBJECT timer. " objeto temporizador
  CREATE OBJECT receiver. " objeto da classe

  SELECT * FROM spfli INTO TABLE i_data.

  SET HANDLER receiver->run_handler FOR timer. " registra método run_handler para obj da classe lcl_receiver
  timer->interval = 1.
  timer->run( ). " executa a primeira vez

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_structure_name = 'SPFLI'
    TABLES
      t_outtab         = i_data. " preenche o alv na primeira vez

*&---------------------------------------------------------------------*
*&  Include           ZTESTE_BIDM_054O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  zm_STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
MODULE zm_status_9001 OUTPUT.

  SET PF-STATUS 'Z_STANDARD'.
  SET TITLEBAR 'Z_TITULO' WITH v_usrname.

  IF v_alv_up IS BOUND. " verifica se obj alv foi criado

    CALL METHOD v_alv_up->refresh_table_display " limpa 1º alv
      EXPORTING
        i_soft_refresh = abap_true
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2.

  ENDIF.

ENDMODULE.                 " zm_STATUS_9001  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  ZM_CREATE_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
MODULE zm_create_screen OUTPUT.

  " verifica se container geral foi criado
  CHECK v_cont_pai IS INITIAL.

  PERFORM zf_criaoo_cont. " cria container maior
  PERFORM zf_criaoo_alv.  " associa container split up e down c/ alv's

ENDMODULE.                 " ZM_CREATE_SCREEN  OUTPUT

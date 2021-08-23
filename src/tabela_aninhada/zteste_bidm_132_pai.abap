*----------------------------------------------------------------------*
***INCLUDE ZTESTE_BIDM_132_PAI.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE sy-ucomm.
    WHEN '_BACK' OR '_EXIT' OR '_CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN '_TAXES'.
      PERFORM zf_alv_popup.
  ENDCASE.

ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TC_TESTE'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc_teste_mark INPUT.
  DATA: g_tc_teste_wa2 LIKE LINE OF it_nflin.
  IF tc_teste-line_sel_mode = 1
  AND wa_nflin-mark = 'X'.
    LOOP AT it_nflin INTO g_tc_teste_wa2
      WHERE mark = 'X'.
      g_tc_teste_wa2-mark = ''.
      MODIFY it_nflin
        FROM g_tc_teste_wa2
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY it_nflin
    FROM wa_nflin
    INDEX tc_teste-current_line
    TRANSPORTING mark.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC_TESTE'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc_teste_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_TESTE'
                              'IT_NFLIN'
                              'MARK'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SH_DOCNUM  INPUT
*&---------------------------------------------------------------------*
MODULE sh_nfenum INPUT.
  PERFORM zf_alv_popup.
ENDMODULE.
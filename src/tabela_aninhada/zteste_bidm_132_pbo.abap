*----------------------------------------------------------------------*
***INCLUDE ZTESTE_BIDM_132_PBO.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS '9001'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_TESTE'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TC_TESTE_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_NFLIN LINES TC_TESTE-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_TESTE'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE TC_TESTE_GET_LINES OUTPUT.
  G_TC_TESTE_LINES = SY-LOOPC.
ENDMODULE.
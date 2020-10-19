*&---------------------------------------------------------------------*
*&  Include           ZTESTE_BIDM_054I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  zm_USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
MODULE zm_user_command_9001 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                 " zm_USER_COMMAND_9001  INPUT

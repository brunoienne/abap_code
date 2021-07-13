
PROCESS BEFORE OUTPUT.
MODULE status_9100.
CALL SUBSCREEN sub INCLUDING sy-repid v_subscr.
MODULE fecha_campos.  " fecha alguns campos header


PROCESS AFTER INPUT.
CALL SUBSCREEN sub.

MODULE user_command.

FIELD w_relath-zterm MODULE check_cond_pagto ON REQUEST.
FIELD ztbmm_nfe_header-j_1bnftype MODULE check_ctg_nota ON REQUEST.

FIELD w_relath-lgort MODULE check_header_lgort ON REQUEST.

FIELD ztbmm_nfe_header-wwert  MODULE change_exch_rate ON REQUEST.

PROCESS ON VALUE-REQUEST.
FIELD w_relath-zterm MODULE sh_zterm.
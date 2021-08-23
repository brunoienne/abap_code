REPORT zteste_bidm_132.

INCLUDE zteste_bidm_132_top.
INCLUDE zteste_bidm_132_pbo.
INCLUDE zteste_bidm_132_pai.
INCLUDE zteste_bidm_132_f01.

START-OF-SELECTION.
  PERFORM zf_select_data.
  CALL SCREEN 9001.
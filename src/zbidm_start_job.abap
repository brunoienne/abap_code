*&---------------------------------------------------------------------*
*& Report  ZTESTE_FFF
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zbidm_start_job.


DATA : v TYPE mblnr,
       lt_seltab TYPE TABLE OF rsparams.

v = '5000002289'.

DATA: jobname TYPE tbtcjob-jobname VALUE
                         'JOB_MR21'.
DATA: jobcount TYPE tbtcjob-jobcount,
      host TYPE msxxlist-host.


DATA: starttimeimmediate TYPE btch0000-char1 VALUE 'X'.

** Job open
CALL FUNCTION 'JOB_OPEN'
  EXPORTING
    delanfrep        = ' '
    jobgroup         = ' '
    jobname          = jobname
    sdlstrtdt        = sy-datum
    sdlstrttm        = sy-uzeit
  IMPORTING
    jobcount         = jobcount
  EXCEPTIONS
    cant_create_job  = 01
    invalid_job_data = 02
    jobname_missing  = 03.
IF sy-subrc NE 0.
  "error processing

ENDIF.

*      ls_seltab-selname = ''.
*      ls_seltab-kind    = ''. S/P
*      ls_seltab-sign    = ''. I/E
*      ls_seltab-option  = ''. EQ/BT
*      ls_seltab-low     = p_bukrs.
*      APPEND ls_seltab TO pt_seltab. CLEAR ls_seltab.

SUBMIT zmm_job_mr21
          WITH p_mblnr = v
          VIA JOB jobname
          NUMBER jobcount AND RETURN. "WITH SELECTION-TABLE lt_seltab
IF sy-subrc EQ 0.
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = jobcount
      jobname              = jobname
      strtimmed            = 'X'
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      OTHERS               = 8.
  "COMMIT WORK AND WAIT.
  WAIT UP TO 1 SECONDS.
ENDIF.

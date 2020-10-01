REPORT zbidm_envio_email_html.

TYPES:BEGIN OF ty_sflight,
  carrid    TYPE s_carr_id,
  connid    TYPE s_conn_id,
  fldate    TYPE s_date,
  price     TYPE s_price,
  currency  TYPE s_currcode,
END OF ty_sflight.


*-------------------------------------------------------------------&
* Tabelas
*-------------------------------------------------------------------&
DATA: t_header      TYPE TABLE OF w3head,
      t_fields      TYPE TABLE OF w3fields,
      t_html_x      TYPE TABLE OF w3html,
      it_sflight    TYPE TABLE OF ty_sflight,
      it_fcat       TYPE lvc_t_fcat,
      it_attachment TYPE solix_tab,
      send_email    TYPE REF TO cl_bcs,
      send_request  TYPE REF TO cl_send_request_bcs,
      document      TYPE REF TO cl_document_bcs,
      recipient     TYPE REF TO if_recipient_bcs.

*-------------------------------------------------------------------&
* Workareas
*-------------------------------------------------------------------&
DATA: ls_fcat       TYPE lvc_s_fcat,
      wa_header     TYPE w3head,
      w_head        TYPE w3head,
      wa_sflight    TYPE ty_sflight, " Flights Details
      s_html_x      LIKE LINE OF t_html_x,
      wa_receivers  TYPE uiys_iusr,
      ld_subject    TYPE so_obj_des.

*-------------------------------------------------------------------&
* Envio de email
*-------------------------------------------------------------------&
DATA: lt_mailsubject     TYPE sodocchgi1,
      lt_mailrecipients  TYPE TABLE OF somlrec90,
      ls_mailrecipients  LIKE LINE OF lt_mailrecipients,
      lt_mailtxt         TYPE TABLE OF soli,
      ls_mailtxt         LIKE LINE OF lt_mailtxt.

*-------------------------------------------------------------------&
*Inicio
*-------------------------------------------------------------------&

* Recipients
ls_mailrecipients-rec_type  = 'U'.
ls_mailrecipients-receiver = 'bimorais@cliptech.com.br'.
APPEND ls_mailrecipients TO lt_mailrecipients . CLEAR ls_mailrecipients .

* Subject.
lt_mailsubject-obj_name  = 'TEST'.
lt_mailsubject-obj_descr = 'Seguem dados financeiros do processo de importação'.

START-OF-SELECTION.

  SELECT * FROM
    sflight
    INTO CORRESPONDING FIELDS OF TABLE it_sflight
    UP TO 20 ROWS.

  ls_fcat-coltext = 'Linha Aérea'.
  APPEND ls_fcat TO it_fcat.  CLEAR ls_fcat.
  ls_fcat-coltext = 'Nº Conexão'.
  APPEND ls_fcat TO it_fcat.CLEAR ls_fcat.
  ls_fcat-coltext = 'Data voo'.
  APPEND ls_fcat TO it_fcat. CLEAR ls_fcat.
  ls_fcat-coltext = 'Valor'.
  APPEND ls_fcat TO it_fcat.CLEAR ls_fcat.
  ls_fcat-coltext = 'Moeda'.
  APPEND ls_fcat TO it_fcat.CLEAR ls_fcat.

  LOOP AT it_fcat INTO ls_fcat.

    w_head-text = ls_fcat-coltext.

*-Populate the Column Headings
    CALL FUNCTION 'WWW_ITAB_TO_HTML_HEADERS'
      EXPORTING
        field_nr = sy-tabix
        text     = w_head-text
        fgcolor  = 'black'
        bgcolor  = 'yellow'
      TABLES
        header   = t_header.

*-Populate Column Properties
    CALL FUNCTION 'WWW_ITAB_TO_HTML_LAYOUT'
      EXPORTING
        field_nr = sy-tabix
        fgcolor  = 'black'
        size     = '3'
      TABLES
        fields   = t_fields.

  ENDLOOP.

* -title of the display
  wa_header-text = 'Detalhes voos' .
  wa_header-font = 'Arial'.
  wa_header-size = '2'.

*-Preparing the HTML from Intenal Table
  CALL FUNCTION 'WWW_ITAB_TO_HTML'
    EXPORTING
      table_header = wa_header
    TABLES
      html         = t_html_x
      fields       = t_fields
      row_header   = t_header
      itable       = it_sflight.


  LOOP AT t_html_x  INTO s_html_x.

    ls_mailtxt = s_html_x.
    APPEND ls_mailtxt TO lt_mailtxt.

  ENDLOOP.

* Send Mail
  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = lt_mailsubject
      document_type              = 'HTM'
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      object_content             = lt_mailtxt
      receivers                  = lt_mailrecipients
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  " I always add a line like this so that you can at least see your program ran.
  WRITE: / 'Programa executado'.

*&-----------------------------------------------------------------&*
*                             ClipTech                              *
*&-----------------------------------------------------------------&*
*&---------------------------------------------------------------- *&
* Descrição .: Classe para envio de email com anexo                 *
*&-----------------------------------------------------------------*&
* Objetivo ..:                                                      *
*&-----------------------------------------------------------------*&
*                           HISTÓRICO                               *
*&-----------------------------------------------------------------*&
* DATA       | REQUEST    | RESPONSÁVEL | DESCRIÇÃO                 *
* 13/11/2020 |            | BIMORAIS    |                           *
*&-----------------------------------------------------------------&*

REPORT  zbidm_send_email.

*--------------------------------------------------------------------
* Tabelas Banco de Dados
*--------------------------------------------------------------------

*--------------------------------------------------------------------
* Tipos
*--------------------------------------------------------------------
TYPES: BEGIN OF veiculo_ty,
         placa     TYPE ztb_veiculos_20-placa,
         marca     TYPE ztb_veiculos_20-marca,
         modelo    TYPE ztb_veiculos_20-modelo,
         ano       TYPE ztb_veiculos_20-ano,
         categoria TYPE ztb_veiculos_20-categoria,
         bloqueado TYPE ztb_veiculos_20-bloqueado,
       END OF veiculo_ty,

       BEGIN OF marca_ty,
         marca     TYPE ztb_marcas_20-marca,
         nome      TYPE ztb_marcas_20-nome,
         expand,
       END OF marca_ty,

       BEGIN OF relat_ty,
         placa     TYPE ztb_veiculos_20-placa,
         marca     TYPE ztb_veiculos_20-marca,
         nome      TYPE ztb_marcas_20-nome,
         modelo    TYPE ztb_veiculos_20-modelo,
         bloqueado TYPE ztb_veiculos_20-bloqueado,
       END OF relat_ty.

*----------------------------------------------------------------------
* Tela de Seleção
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
PARAMETERS: p_pdf RADIOBUTTON GROUP rad MODIF ID pdf DEFAULT 'X',
            p_exc RADIOBUTTON GROUP rad MODIF ID exc.
SELECTION-SCREEN END OF BLOCK b01.


*--------------------------------------------------------------------
* Classe relatório
*--------------------------------------------------------------------
INCLUDE zbidm_send_email_cl.


*--------------------------------------------------------------------
* Objetos
*--------------------------------------------------------------------
DATA: lv_out TYPE boolean.

*----------------------------------------------------------------------
* Eventos de tela
*----------------------------------------------------------------------
INITIALIZATION.
  CREATE OBJECT go_mail.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------

  " Seleção do conteudo necessario
  go_mail->select_data(
    IMPORTING
      e_out = lv_out
  ).

  IF lv_out IS NOT INITIAL.
    go_mail->send_email(
      EXPORTING
        i_subject      = go_mail->subject_email( )   " assunto
        i_recipient    = go_mail->recipient_email( ) " destinatário
        i_body         = go_mail->body_email( )      " corpo email
        i_attachment   = go_mail->get_attachment( )  " nome anexo OPCIONAL
        i_otf          = go_mail->get_otf( )         " conteudo otf(PDF) OPCIONAL
        i_excel        = go_mail->get_excel( )       " conteudo xls OPCIONAL
    ).

  ENDIF.
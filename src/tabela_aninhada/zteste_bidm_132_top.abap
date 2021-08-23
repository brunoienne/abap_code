*&---------------------------------------------------------------------*
*&  Include           ZTESTE_BIDM_132_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF taxes_ty,
         docnum TYPE bapi_j_1bnfstx-docnum,
         itmnum TYPE bapi_j_1bnfstx-itmnum,
         taxtyp TYPE bapi_j_1bnfstx-taxtyp,
         base   TYPE bapi_j_1bnfstx-base,
         rate   TYPE bapi_j_1bnfstx-rate,
         taxval TYPE bapi_j_1bnfstx-taxval,
         excbas TYPE bapi_j_1bnfstx-excbas,
         othbas TYPE bapi_j_1bnfstx-othbas,
         marki  TYPE c,
       END OF taxes_ty,

       BEGIN OF nflin_ty,
         nfenum  TYPE j_1bnfdoc-nfenum,
         docnum  TYPE j_1bnfdoc-docnum,
         itmnum  TYPE j_1bnflin-itmnum,
         matnr   TYPE j_1bnflin-matnr,
         maktx   TYPE j_1bnflin-maktx,
         nbm     TYPE j_1bnflin-nbm,
         menge   TYPE j_1bnflin-menge,
         t_taxes TYPE taxes_ty OCCURS 0,
         mark,
       END OF nflin_ty.

DATA: it_nflin TYPE TABLE OF nflin_ty,
      wa_nflin TYPE nflin_ty.

CONTROLS: tc_teste TYPE TABLEVIEW USING SCREEN 9001.
DATA: g_tc_teste_lines LIKE sy-loopc,
      ok_code          LIKE sy-ucomm.
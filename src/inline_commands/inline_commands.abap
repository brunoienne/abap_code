*&---------------------------------------------------------------------*
*& Report ZTESTE_BIDM_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zteste_bidm_001.

*--------------------------------------------------------------------*
* Tipos
*--------------------------------------------------------------------*
TYPES: BEGIN OF pessoa_ty,
         nome  TYPE char30,
         idade TYPE i,
         sexo  TYPE char1,
         empr  TYPE char10,
         sala  TYPE i,
       END OF pessoa_ty.

" Tipos de tabela (declarada uma vez)
TYPES: tt_pessoa TYPE TABLE OF pessoa_ty WITH EMPTY KEY.

*--------------------------------------------------------------------*
* Parâmetros
*--------------------------------------------------------------------*
PARAMETERS: p_date TYPE d DEFAULT sy-datum,
            p_hour TYPE t DEFAULT sy-uzeit,
            p_flag AS CHECKBOX.

*--------------------------------------------------------------------*
* Includes
*--------------------------------------------------------------------*
INCLUDE inline_commands_cl.

INITIALIZATION.
  lo_local = new lcl_local( ).

*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*

  IF lcl_local=>return_boolean( ). " if TRUE
    BREAK-POINT.
  ENDIF.

  IF NOT lcl_local=>return_boolean( ). " if FALSE
    BREAK-POINT.
  ENDIF.

  DATA(result) = |Olá { sy-uname } |.
  DATA(msg)    = |Hello| & | | & |world|.

  DATA(lt_pessoa) = VALUE tt_pessoa( ( nome = 'Bruno' idade = 23 sexo = 'M' empr = 'Clip' sala = 1500 )
                                     ( nome = 'Bruna' idade = 18 sexo = 'F' empr = 'Kmx'  sala = 2000 )
                                     ( nome = 'Tiago' idade = 19 sexo = 'M' empr = 'Clip' sala = 2500 ) ).

  " Linhas de uma tabela
  DATA(lines) =  lines( lt_pessoa ).

  "Mover valores de uma tabela p/outra com critério
  DATA(lt_pessoa2) = VALUE tt_pessoa( FOR ls_pessoa IN lt_pessoa "WHERE ( empr = 'Clip' )
                                     ( nome = ls_pessoa-nome idade = ls_pessoa-idade ) ).

  " move-corresponding
  DATA(lt_pessoa3) = CORRESPONDING tt_pessoa( lt_pessoa ).
  "DATA(lt_pessoa4) = CORRESPONDING #( lt_pessoa MAPPING fieldto = fieldfrom EXCEPT fieldnotincluded ).

  SELECT matnr, maktx FROM makt INTO TABLE @DATA(lt_makt) UP TO 10 ROWS
    WHERE spras EQ @sy-langu.

  DATA(ls_makt) = lt_makt[ 1 ]. " = READ TABLE lt_makt INTO DATA(ls_maktx) INDEX 1

  DATA(lv_matnr_out) = |{ ls_makt-matnr ALPHA = OUT }|.
  DATA(lv_matnr_in)  = |{ lv_matnr_out ALPHA  = IN }|.

  BREAK-POINT.

  IF line_exists( lt_makt[ matnr = lv_matnr_in ] ).           " verifica se linha existe

    DATA(idx) = line_index( lt_makt[ matnr = lv_matnr_in ] ). " recebe indice encontrado

  ENDIF.

  " Somar valores de uma coluna
  DATA(sum_sal) = REDUCE i( INIT i = 0 FOR ls_pessoa IN lt_pessoa NEXT i = i + ls_pessoa-sala ).

  " Conversões data
  DATA(dt_iso)  = |{ p_date DATE = ISO }|.
  DATA(dt_user) = |{ p_date DATE = USER }|.
  DATA(dt_env)  = |{ p_date DATE = ENVIRONMENT }|.

  " Conversões hora
  DATA(hr_iso)  = |{ p_hour TIME = ISO }|.
  DATA(hr_user) = |{ p_hour TIME = USER }|.
  DATA(hr_env)  = |{ p_hour TIME = ENVIRONMENT }|.

*DATA(etiq) = SWITCH #( p_etiq WHEN 'AD' THEN 'Etiqueta AD'
*                              WHEN 'OP' THEN 'Etiqueta OP').


  BREAK-POINT.
*&---------------------------------------------------------------------*
*&  Include           ZTESTE_BIDM_001_CL
*&---------------------------------------------------------------------*

CLASS lcl_local DEFINITION.
    PUBLIC SECTION.
      CLASS-METHODS:
        return_boolean
          RETURNING
            VALUE(r_valor) TYPE flag.
  
  ENDCLASS.
  
  CLASS lcl_local IMPLEMENTATION.
  
    METHOD return_boolean.
      r_valor = p_flag.
    ENDMETHOD.
  
  ENDCLASS.
  
  DATA: lo_local TYPE REF TO lcl_local.
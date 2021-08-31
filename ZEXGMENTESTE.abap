*&---------------------------------------------------------------------*
*& Report ZEXGMENTESTE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zexgmenteste MESSAGE-ID 000.

DATA:
  im_cons	TYPE zconsignment-consignment,
  im_land	TYPE land1,
  result  TYPE zconsignment.


"************************************************************************************"
*" User     |  Mod.Date    | SRQ Number/Description                                   "
*"------------------------------------------------------------------------------------"
*" EXGMEN   |  26.08.2021  | SQR # - ADBC Query                                       "
*"------------------------------------------------------------------------------------"
*"************************************************************************************"
DATA: wa_consignment TYPE zconsignment.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
TYPES:
  BEGIN OF ty_result,
    consignment TYPE zconsignment-consignment,
    department  TYPE zconsignment-department,
  END OF ty_result.

DATA: lt_result                 TYPE STANDARD TABLE OF ty_result,
      lr_data                   TYPE REF TO data,
      lv_where_clause_statement TYPE string,
      lv_table                  TYPE strng.

DATA(lo_db_select_cargolink) = NEW ziwf_cl_db_select_cargolink( ).

lo_db_select_cargolink->start_connection(
                          IMPORTING
                            ev_error          = DATA(lv_error)
                            eo_sql_connection = DATA(lo_sql_connection)
                            eo_sql_statement  = DATA(lo_sql_statement) ).
IF lv_error IS NOT INITIAL.
  RETURN.
ENDIF.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
CHECK im_cons IS NOT INITIAL.

CASE im_land.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
  WHEN 'DK'.
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZCPH_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZCPH_CONSIGNMENT'.

*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'BE'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZANT_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZANT_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'SE'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZMAL_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZMAL_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'ES'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZSPAN_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZSPAN_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'FR'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZPAR_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZPAR_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'NO'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZOSLO_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZOSLO_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'FI'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZHEL_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZHEL_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'NL'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZROT_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZROT_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'IE'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZDUB_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZDUB_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'GB'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZHAR_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZHAR_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'PL'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZWAW_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZWAW_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'DE'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZFRAN_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZFRAN_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'CH'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZSWIT_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZSWIT_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'TR'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZTURK_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZTURK_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'AT'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZAUST_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZAUST_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'RO'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZROMA_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZROMA_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'PT'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZPORT_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZPORT_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'SK'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZSLOK_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZSLOK_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'HU'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZHUNG_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZHUNG_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'CZ'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZPRAG_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZPRAG_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'EE'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZTAL_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZTAL_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'LT'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZVIL_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZVIL_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN 'LV'.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*      EXEC SQL.
*        OPEN dbcur FOR
*          SELECT consignment, department
*                 FROM ZRIG_CONSIGNMENT
*                 WHERE CONSIGNMENT = :im_cons
*      ENDEXEC.
    lv_table = 'ZRIG_CONSIGNMENT'.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
  WHEN OTHERS.
*      Add select here
    RETURN.

ENDCASE.
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*  EXEC SQL.
*    FETCH NEXT dbcur INTO :wa_consignment-consignment, :wa_consignment-department
*  ENDEXEC.

*  IF sy-subrc = 0.
*    result = wa_consignment.
*  ENDIF.
lv_where_clause_statement = |SELECT CONSIGNMENT, DEPARTMENT |
                         && | { lv_table } |
                         && | WHERE CONSIGNMENT = '{ :im_cons }' |.

GET REFERENCE OF lt_result INTO lr_data.

DATA(lo_sql_result_set) = lo_db_select_cargolink->get_data(
                            EXPORTING
                              iv_where_clause_statement = lv_where_clause_statement
                              io_sql_statement          = lo_sql_statement
                            CHANGING
                              co_data                   = lr_data ).

lo_db_select_cargolink->close(
                         EXPORTING
                           io_sql_connection = lo_sql_connection
                           io_sql_result_set = lo_sql_result_set ).

IF lt_result IS NOT INITIAL.
  result = CORRESPONDING #( VALUE #( lt_result[ 1 ] OPTIONAL ) ).
ENDIF.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query
* 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query ->
*  EXEC SQL.
*    CLOSE dbcur
*  ENDEXEC.
*<- 26.08.2021 - EXGMEN(Gilberto Mendonça) - SRQ # - ADBC Query

BREAK-POINT.

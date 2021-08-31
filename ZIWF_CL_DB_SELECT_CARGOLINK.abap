class ZIWF_CL_DB_SELECT_CARGOLINK definition
  public
  final
  create public .

public section.

  methods START_CONNECTION
    exporting
      !EV_ERROR type STRING
      value(EO_SQL_CONNECTION) type ref to CL_SQL_CONNECTION
      value(EO_SQL_STATEMENT) type ref to CL_SQL_STATEMENT .
  methods GET_DATA
    importing
      !IV_WHERE_CLAUSE_STATEMENT type STRING
      !IO_SQL_STATEMENT type ref to CL_SQL_STATEMENT
    exporting
      !EV_ERROR type STRING
    changing
      !CO_DATA type DATA
    returning
      value(RTO_SQL_RESULT_SET) type ref to CL_SQL_RESULT_SET .
  methods CLOSE
    importing
      !IO_SQL_CONNECTION type ref to CL_SQL_CONNECTION
      !IO_SQL_RESULT_SET type ref to CL_SQL_RESULT_SET
    exporting
      !EV_ERROR type STRING .
protected section.
private section.

  methods MAKE_DB_CONNECTION
    exporting
      !EV_ERROR type STRING
    returning
      value(RT_SQL_CONNECTION) type ref to CL_SQL_CONNECTION
    raising
      CX_SQL_EXCEPTION .
  methods INIT_SQL_STATEMENT
    importing
      !IO_SQL_CONNECTION type ref to CL_SQL_CONNECTION
    exporting
      !EV_ERROR type STRING
    returning
      value(RTO_SQL_STATEMENT) type ref to CL_SQL_STATEMENT .
  methods ISSUE_NATIVE_SQL_CALL
    importing
      !IV_WHERE_CLAUSE_STATEMENT type STRING
      !IO_SQL_STATEMENT type ref to CL_SQL_STATEMENT
    returning
      value(RTO_SQL_RESULT_SET) type ref to CL_SQL_RESULT_SET
    raising
      CX_SQL_EXCEPTION .
  methods ASSIGN_TARGET_RESULT
    importing
      !IO_SQL_RESULT_SET type ref to CL_SQL_RESULT_SET
    changing
      !CO_DATA type ref to DATA
    raising
      CX_PARAMETER_INVALID .
  methods CLOSE_QUERY
    importing
      !IO_SQL_RESULT_SET type ref to CL_SQL_RESULT_SET .
  methods CLOSE_DB_CONNECTION
    importing
      !IO_SQL_CONNECTION type ref to CL_SQL_CONNECTION
    exporting
      !EV_ERROR type STRING .
ENDCLASS.



CLASS ZIWF_CL_DB_SELECT_CARGOLINK IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZIWF_CL_DB_SELECT_CARGOLINK->ASSIGN_TARGET_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SQL_RESULT_SET              TYPE REF TO CL_SQL_RESULT_SET
* | [<-->] CO_DATA                        TYPE REF TO DATA
* | [!CX!] CX_PARAMETER_INVALID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD assign_target_result.

    io_sql_result_set->set_param_table(
      EXPORTING
        itab_ref = co_data ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZIWF_CL_DB_SELECT_CARGOLINK->CLOSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SQL_CONNECTION              TYPE REF TO CL_SQL_CONNECTION
* | [--->] IO_SQL_RESULT_SET              TYPE REF TO CL_SQL_RESULT_SET
* | [<---] EV_ERROR                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD close.

    CLEAR ev_error.

    IF io_sql_connection IS BOUND AND
       io_sql_result_set IS BOUND.

      TRY.
          close_query( EXPORTING io_sql_result_set = io_sql_result_set ).
          close_db_connection( EXPORTING io_sql_connection = io_sql_connection ).
        CATCH cx_sql_exception INTO DATA(lo_error).
          ev_error = lo_error->get_text( ).
      ENDTRY.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZIWF_CL_DB_SELECT_CARGOLINK->CLOSE_DB_CONNECTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SQL_CONNECTION              TYPE REF TO CL_SQL_CONNECTION
* | [<---] EV_ERROR                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD close_db_connection.

    CLEAR ev_error.

    TRY.
        io_sql_connection->close( ).
      CATCH cx_sql_exception INTO DATA(lo_error).
        ev_error = lo_error->get_text( ).
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZIWF_CL_DB_SELECT_CARGOLINK->CLOSE_QUERY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SQL_RESULT_SET              TYPE REF TO CL_SQL_RESULT_SET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD close_query.

    io_sql_result_set->close( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZIWF_CL_DB_SELECT_CARGOLINK->GET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_WHERE_CLAUSE_STATEMENT      TYPE        STRING
* | [--->] IO_SQL_STATEMENT               TYPE REF TO CL_SQL_STATEMENT
* | [<---] EV_ERROR                       TYPE        STRING
* | [<-->] CO_DATA                        TYPE        DATA
* | [<-()] RTO_SQL_RESULT_SET             TYPE REF TO CL_SQL_RESULT_SET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_data.

    CLEAR ev_error.

    TRY.
        rto_sql_result_set = issue_native_sql_call(
                              EXPORTING
                                iv_where_clause_statement = iv_where_clause_statement
                                io_sql_statement          = io_sql_statement  ).

        assign_target_result(
          EXPORTING
            io_sql_result_set = rto_sql_result_set
          CHANGING
        co_data           = co_data ).

        rto_sql_result_set->next_package( ).

      CATCH cx_sql_exception INTO DATA(lo_error).
        ev_error = lo_error->get_text( ).
      CATCH cx_parameter_invalid_type INTO DATA(lo_error_type).
        ev_error = lo_error_type->get_text( ).
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZIWF_CL_DB_SELECT_CARGOLINK->INIT_SQL_STATEMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SQL_CONNECTION              TYPE REF TO CL_SQL_CONNECTION
* | [<---] EV_ERROR                       TYPE        STRING
* | [<-()] RTO_SQL_STATEMENT              TYPE REF TO CL_SQL_STATEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD init_sql_statement.

    CLEAR ev_error.

    IF io_sql_connection IS BOUND.
      rto_sql_statement = io_sql_connection->create_statement( ).
    ELSE.
      ev_error = TEXT-001.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZIWF_CL_DB_SELECT_CARGOLINK->ISSUE_NATIVE_SQL_CALL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_WHERE_CLAUSE_STATEMENT      TYPE        STRING
* | [--->] IO_SQL_STATEMENT               TYPE REF TO CL_SQL_STATEMENT
* | [<-()] RTO_SQL_RESULT_SET             TYPE REF TO CL_SQL_RESULT_SET
* | [!CX!] CX_SQL_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD issue_native_sql_call.

    io_sql_statement->execute_query(
      EXPORTING
        statement             =  iv_where_clause_statement
        hold_cursor           = space
      RECEIVING
        result_set            = rto_sql_result_set
    ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZIWF_CL_DB_SELECT_CARGOLINK->MAKE_DB_CONNECTION
* +-------------------------------------------------------------------------------------------------+
* | [<---] EV_ERROR                       TYPE        STRING
* | [<-()] RT_SQL_CONNECTION              TYPE REF TO CL_SQL_CONNECTION
* | [!CX!] CX_SQL_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD make_db_connection.

    CLEAR ev_error.
    "Get DB
    rt_sql_connection = cl_sql_connection=>get_connection( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZIWF_CL_DB_SELECT_CARGOLINK->START_CONNECTION
* +-------------------------------------------------------------------------------------------------+
* | [<---] EV_ERROR                       TYPE        STRING
* | [<---] EO_SQL_CONNECTION              TYPE REF TO CL_SQL_CONNECTION
* | [<---] EO_SQL_STATEMENT               TYPE REF TO CL_SQL_STATEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD start_connection.

    CLEAR ev_error.

    TRY.
        eo_sql_connection = make_db_connection( ).
        eo_sql_statement = init_sql_statement(
                            EXPORTING
                              io_sql_connection = eo_sql_connection ).
      CATCH cx_sql_exception INTO DATA(lo_error).
        ev_error = lo_error->get_text( ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

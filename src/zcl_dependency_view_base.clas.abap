class ZCL_DEPENDENCY_VIEW_BASE definition
  public
  create public .

  public section.

    " construction
    class-methods create_simple
      importing
        !it_content type any table
        !iv_title type csequence default 'VIEW'
        !iv_technames type abap_bool default abap_false
        !iv_popup type abap_bool default abap_false
        !iv_popup_width type i default 45
        !iv_popup_height type i default 10
        !iv_pfstatus type string optional
        !iv_selection_mode type salv_de_constant default if_salv_c_selection_mode=>none
        !iv_hide_fields type string optional
      returning
        value(ro_alv_view) type ref to zcl_dependency_view_base
      raising
        cx_static_check .

    " controls
    methods display
      raising
        cx_static_check .
    methods close .
    methods update_content
      importing
        !it_content type any table .
    methods refresh .
    methods set_sorting
      importing
        !iv_fields type any
      returning
        value(ro_alv_view) type ref to zcl_dependency_view_base
      raising
        cx_static_check .
    methods set_aggregations
      importing
        !iv_fields type any
      returning
        value(ro_alv_view) type ref to zcl_dependency_view_base
      raising
        cx_static_check .

    " events
    methods handle_double_click
      for event double_click of cl_salv_events_table
      importing
        !row
        !column .
    methods on_alv_user_command
      for event added_function of cl_salv_events
      importing
        !e_salv_function .

  protected section.
    data mo_alv  type ref to cl_salv_table.
    data mr_data type ref to data.
    data mt_hide_fields type string_table.

    methods set_column_tech_names.

    methods set_default_layout
      importing
        iv_title type lvc_title.

    methods set_default_handlers.

     methods copy_content
      importing
        it_contents type any table.

    methods create_alv
      raising
        cx_static_check.

    methods set_columns_default
      raising
        cx_static_check.

    methods enable_layout_variants
      importing
        iv_variant_key type repid.

  private section.
    methods normalize_list_of_fields
      importing
        iv_fields type any
      returning
        value(rt_fields) type string_table
      raising
        cx_static_check.

ENDCLASS.



CLASS ZCL_DEPENDENCY_VIEW_BASE IMPLEMENTATION.


  method close.

    mo_alv->close_screen( ).

  endmethod.


  method copy_content.

    data lo_ttype type ref to cl_abap_tabledescr.
    data lo_stype type ref to cl_abap_structdescr.

    if mr_data is initial.
      lo_ttype ?= cl_abap_typedescr=>describe_by_data( it_contents ).
      lo_stype ?= lo_ttype->get_table_line_type( ).
      lo_ttype = cl_abap_tabledescr=>create( lo_stype ). "ensure standard table
      create data mr_data type handle lo_ttype.
    endif.

    field-symbols <tab> type standard table.
    assign mr_data->* to <tab>.
    <tab> = it_contents.

  endmethod.


  method create_alv.

    data lx_alv type ref to cx_salv_error.
    field-symbols <tab> type standard table.
    assign mr_data->* to <tab>.

    try.
      cl_salv_table=>factory(
        importing
          r_salv_table = mo_alv
        changing
          t_table      = <tab> ).
    catch cx_salv_msg into lx_alv.
      zcx_dependency_error=>raise( lx_alv->get_text( ) ).
    endtry.

  endmethod.


  method create_simple.

    create object ro_alv_view.

    ro_alv_view->copy_content( it_content ).
    ro_alv_view->create_alv( ).

    if iv_hide_fields is not initial.
      data lv_hide_fields type string.
      lv_hide_fields = to_upper( iv_hide_fields ).
      condense lv_hide_fields no-gaps.
      split lv_hide_fields at ',' into table ro_alv_view->mt_hide_fields.
    endif.

    if iv_technames = abap_true.
      ro_alv_view->set_column_tech_names( ).
    endif.

    ro_alv_view->set_default_layout( |{ iv_title }| ).
    ro_alv_view->set_columns_default( ).

    if iv_popup = abap_true.
      ro_alv_view->mo_alv->set_screen_popup(
        start_column = 5
        end_column   = 5 + iv_popup_width
        start_line   = 5
        end_line     = 1 + iv_popup_height ).
    endif.

    if iv_pfstatus is not initial.
      data lv_prog type string.
      data lv_pfstatus type string.

      split iv_pfstatus at '/' into lv_prog lv_pfstatus.
      if lv_pfstatus is initial.
        lv_pfstatus = lv_prog.
        lv_prog     = sy-cprog.
      endif.

      ro_alv_view->mo_alv->set_screen_status(
        pfstatus = |{ lv_pfstatus }|
        report   = |{ lv_prog }| ).
    endif.

*    if ii_callbacks is bound.
*      ro_alv_view->set_callbacks( ii_callbacks ).
*      ro_alv_view->set_default_handlers( ).
*    endif.

    if iv_selection_mode is not initial.
      ro_alv_view->mo_alv->get_selections( )->set_selection_mode( iv_selection_mode ).
    endif.

  endmethod.


  method display.
    mo_alv->display( ).
  endmethod.


  method enable_layout_variants.

    data lo_layout type ref to cl_salv_layout.
    data ls_layout_key type salv_s_layout_key.

    " Add possibility to save table layout
    lo_layout = mo_alv->get_layout( ).
    ls_layout_key-report = iv_variant_key.
    lo_layout->set_key( ls_layout_key ).
    lo_layout->set_default( abap_true ).
    lo_layout->set_save_restriction( ).

  endmethod.


  method handle_double_click.

    " Skipped

  endmethod.


  method normalize_list_of_fields.

    data lt_fields type string_table.
    data lv_ftype  type c.
    data lv_fld    type string.
    field-symbols <sorts> type string_table.

    describe field iv_fields type lv_ftype.

    if lv_ftype ca 'Cg'. " Value, assume char like
      split iv_fields at ',' into table lt_fields.
      assign lt_fields to <sorts>.
    elseif lv_ftype = 'h'. " Table, assume string table
      assign iv_fields to <sorts>.
    else.
      zcx_dependency_error=>raise( 'Wrong field list parameter' ).
    endif.

    loop at <sorts> into lv_fld.
      lv_fld = to_upper( lv_fld ).
      condense lv_fld.
      check lv_fld is not initial.
      append lv_fld to rt_fields.
    endloop.

  endmethod.


  method on_alv_user_command.

     " Skipped

  endmethod.


  method refresh.
    mo_alv->refresh( ).
  endmethod.


  method set_aggregations.

    data lx_alv    type ref to cx_salv_error.
    data lo_agg type ref to cl_salv_aggregations.
    data lt_fields type string_table.
    data lv_field  type string.

    lo_agg = mo_alv->get_aggregations( ).
    lt_fields = normalize_list_of_fields( iv_fields ).

    try.
      loop at lt_fields into lv_field.
        lo_agg->add_aggregation(
          columnname  = |{ lv_field }|
          aggregation = if_salv_c_aggregation=>total ).
      endloop.
    catch cx_salv_data_error cx_salv_not_found cx_salv_existing into lx_alv.
      zcx_dependency_error=>raise( lx_alv->get_text( ) ).
    endtry.

    ro_alv_view = me.

  endmethod.


  method set_columns_default.

    assert mr_data is not initial.

    data lo_ttype type ref to cl_abap_tabledescr.
    data lo_stype type ref to cl_abap_structdescr.
    field-symbols <c> like line of lo_stype->components.
    data lo_cols type ref to cl_salv_columns.
    data lo_col type ref to cl_salv_column.

    lo_cols = mo_alv->get_columns( ).
    lo_cols->set_optimize( abap_true ).

    lo_ttype ?= cl_abap_typedescr=>describe_by_data_ref( mr_data ).
    lo_stype ?= lo_ttype->get_table_line_type( ).

    try.
      loop at lo_stype->components assigning <c>.
        if <c>-type_kind = cl_abap_typedescr=>typekind_packed.
          lo_col = lo_cols->get_column( columnname = <c>-name ).
          lo_col->set_sign( abap_true ).
        endif.

        read table mt_hide_fields with key table_line = <c>-name transporting no fields.
        if sy-subrc = 0 or <c>-name = 'MANDT'.
          lo_col = lo_cols->get_column( columnname = <c>-name ).
          lo_col->set_visible( abap_false ).
        endif.
      endloop.
    catch cx_salv_not_found.
      " Ignore
    endtry.

  endmethod.


  method set_column_tech_names.

    data lo_cols type ref to cl_salv_columns.
    data lt_columns type salv_t_column_ref.

    lo_cols = mo_alv->get_columns( ).
    lo_cols->set_optimize( abap_true ).
    lt_columns = lo_cols->get( ).

    field-symbols <c> like line of lt_columns.
    loop at lt_columns assigning <c>.
      <c>-r_column->set_short_text( |{ <c>-columnname }| ).
      <c>-r_column->set_medium_text( |{ <c>-columnname }| ).
      <c>-r_column->set_long_text( |{ <c>-columnname }| ).
    endloop.

  endmethod.


  method set_default_handlers.

    data lo_event type ref to cl_salv_events_table.
    lo_event = mo_alv->get_event( ).
    set handler handle_double_click for lo_event.
    set handler on_alv_user_command for lo_event.

  endmethod.


  method set_default_layout.

    data lo_functions type ref to cl_salv_functions_list.
    lo_functions = mo_alv->get_functions( ).
*    lo_functions->set_default( abap_true ).
    lo_functions->set_all( abap_true ).

    data lo_display type ref to cl_salv_display_settings.
    lo_display = mo_alv->get_display_settings( ).
    lo_display->set_striped_pattern( abap_true ).
    lo_display->set_list_header( iv_title ).

  endmethod.


  method set_sorting.

    data lx_alv    type ref to cx_salv_error.
    data lo_sorts  type ref to cl_salv_sorts.
    data lt_fields type string_table.
    data lv_field  type string.
    data lv_subtotal type abap_bool.

    lo_sorts = mo_alv->get_sorts( ).
    lt_fields = normalize_list_of_fields( iv_fields ).

    try.
      loop at lt_fields into lv_field.
        lv_subtotal = boolc( '^' = substring( val = lv_field len = 1 ) ).
        if lv_subtotal = abap_true.
          lv_field = substring( val = lv_field off = 1 ).
        endif.
        lo_sorts->add_sort(
          columnname = |{ lv_field }|
          subtotal   = lv_subtotal ).
      endloop.
    catch cx_salv_error into lx_alv.
      zcx_dependency_error=>raise( lx_alv->get_text( ) ).
    endtry.

    ro_alv_view = me.

  endmethod.


  method update_content.
    copy_content( it_content ).
    mo_alv->refresh( ).
  endmethod.
ENDCLASS.

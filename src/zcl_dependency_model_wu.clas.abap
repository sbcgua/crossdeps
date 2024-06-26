class ZCL_DEPENDENCY_MODEL_WU definition
  public
  final
  create public.

  public section.

    methods select_external_usages
      importing
        i_package type tadir-devclass
        ir_package_scope type zif_dependency_types=>ty_devc_range optional
      returning
        value(rt_objs) type zif_dependency_types=>tty_dependency.

  protected section.
  private section.

    types tty_where_used type standard table of rsfindlst with default key.
    types ty_seu_obj type standard table of seu_obj with default key.
    types:
      begin of ty_dev_object,
        type type seu_stype,
        tadir type trobjtype,
      end of ty_dev_object.

    data mt_object_packages type hashed table of zif_dependency_types=>ty_obj_signature with unique key obj_type obj_name.
    data mt_dev_obj_cache type hashed table of ty_dev_object with unique key type.

    methods get_where_used
      importing
        iv_obj_type      type euobj-id
        iv_obj_name      type tadir-obj_name
        it_scope         type ty_seu_obj optional
        ir_package_scope type zif_dependency_types=>ty_devc_range optional
      returning
        value(rt_findings) type tty_where_used.

    methods get_obj_package
      importing
        iv_obj_type      type tadir-object
        iv_obj_name      type tadir-obj_name
      returning
        value(rv_package) type tadir-devclass.

    methods get_func_package
      importing
        iv_func_name      type tadir-obj_name
      returning
        value(rv_package) type tadir-devclass.

    methods get_incl_package
      importing
        iv_prog_name      type tadir-obj_name
      returning
        value(rv_package) type tadir-devclass.

    methods build_package_scope
      importing
        it_tadir type standard table
        ir_package_scope type zif_dependency_types=>ty_devc_range
      returning
        value(rt_package_scope) type zif_dependency_types=>ty_devc_range.

    methods collect_where_used
      importing
        it_tadir type standard table
        ir_package_scope type zif_dependency_types=>ty_devc_range
      returning
        value(rt_objs) type zif_dependency_types=>tty_dependency.

    methods convert_list
      importing
        iv_package  type zif_dependency_types=>ty_dependency-dep_package
        iv_obj_type type zif_dependency_types=>ty_dependency-dep_obj_type
        iv_obj_name type zif_dependency_types=>ty_dependency-dep_obj_name
        it_where_used type tty_where_used
      returning
        value(rt_objs) type zif_dependency_types=>tty_dependency.

    methods decode_obj_type
      importing
        iv_type type rsfindlst-object_cls
      returning
        value(rv_type) type ty_dev_object-tadir.

ENDCLASS.



CLASS ZCL_DEPENDENCY_MODEL_WU IMPLEMENTATION.


  method build_package_scope.

    field-symbols <tadir> type zcl_abapgit_tadir_clone=>ty_tadir.
    field-symbols <pkg> like line of rt_package_scope.

    rt_package_scope = ir_package_scope.
    loop at it_tadir assigning <tadir>.
      check <tadir>-object = 'DEVC'.
      append initial line to rt_package_scope assigning <pkg>.
      <pkg>-sign   = 'E'.
      <pkg>-option = 'EQ'.
      <pkg>-low    = <tadir>-obj_name.
    endloop.

  endmethod.


  method collect_where_used.

    data li_progress type ref to zif_abapgit_progress_clone.
    data lt_where_used type tty_where_used.
    data lt_objs_portion like rt_objs.

    field-symbols <tadir> type zcl_abapgit_tadir_clone=>ty_tadir.

    li_progress = zcl_abapgit_progress_clone=>get_instance( lines( it_tadir ) ).

    loop at it_tadir assigning <tadir>.
      check <tadir>-object <> 'DEVC'.

      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |{ <tadir>-object } { <tadir>-obj_name }| ).

      lt_where_used = get_where_used(
        iv_obj_type = |{ <tadir>-object }|
        iv_obj_name = <tadir>-obj_name
        ir_package_scope = ir_package_scope ).

      lt_objs_portion = convert_list(
        iv_package    = <tadir>-devclass
        iv_obj_type   = <tadir>-object
        iv_obj_name   = <tadir>-obj_name
        it_where_used = lt_where_used ).

      append lines of lt_objs_portion to rt_objs.

    endloop.

    li_progress->off( ).

  endmethod.


  method convert_list.

    " See also CL_FINB_GN_BBI=>GET_CROSSREF

    field-symbols <dep> like line of rt_objs.
    field-symbols <use> like line of it_where_used.

    loop at it_where_used assigning <use>.

      append initial line to rt_objs assigning <dep>.
      <dep>-cnt = 1.
      <dep>-dep_package  = iv_package.
      <dep>-dep_obj_type = iv_obj_type.
      <dep>-dep_obj_name = iv_obj_name.

      <dep>-dep_used_obj = <use>-used_obj.
      <dep>-dep_used_cls = <use>-used_cls.

      <dep>-obj_cls  = <use>-object_cls.
      <dep>-obj_name = <use>-encl_objec.
      if <dep>-obj_name is initial.
        <dep>-obj_name = <use>-object.
      endif.

      if <use>-object_cls = 'FF'. " Function module
        <dep>-obj_type = 'FUNC'.
        <dep>-package = get_func_package( <dep>-obj_name ).

      else.
        <dep>-obj_type = decode_obj_type( <use>-object_cls ).

        <dep>-package = get_obj_package(
          iv_obj_type = <dep>-obj_type
          iv_obj_name = <dep>-obj_name ).

        if <dep>-package is initial and <dep>-obj_type = 'CLAS'.
          <dep>-package = get_obj_package(
            iv_obj_type = 'INTF'
            iv_obj_name = <dep>-obj_name ).
          if <dep>-package is not initial.
            <dep>-obj_type = 'INTF'.
          endif.
        endif.
      endif.

      if <dep>-package is initial.
        if <dep>-obj_type = 'PROG'. " Maybe it is an include

          <dep>-package = get_incl_package( <dep>-obj_name ).
          if <dep>-package is initial.
            select single subc into <dep>-obj_prog_type from trdir where name = <dep>-obj_name.
            if <dep>-obj_prog_type is not initial and <dep>-obj_prog_type <> '1'. " Exec. prog
              <dep>-obj_type = 'INCL'.
            endif.
          endif.

        endif.

        if <dep>-package is initial.
          <dep>-package = '????'.
        endif.
      endif.

    endloop.

    " some includes are FUGR and some are ENHO ...
    " include detection TRDIR, D010INC ???
    " How to find FUGR by main program name ???
    " how to find connection with ENHO ?
    " Useful: https://github.com/abaplint/abaplint-sci-client/blob/main/src/deps/zcl_abaplint_deps_find.clas.abap
    " And cl_wb_manager->if_wb_manager~request_tool_access
    " And discussions in https://github.com/abapGit/abapGit/pull/6897

  endmethod.


  method decode_obj_type.

    field-symbols <devobj> like line of mt_dev_obj_cache.

    if mt_dev_obj_cache is initial.
      select type tadir into table mt_dev_obj_cache
        from euobjedit.
    endif.

    read table mt_dev_obj_cache assigning <devobj> with key type = iv_type.
    if sy-subrc = 0.
      rv_type = <devobj>-tadir.
    endif.

  endmethod.


  method get_func_package.

    " See also: FUNCTION_INCLUDE_INFO, TFDIR, find main program -> get its pkg

    data ls_obj_sig like line of mt_object_packages.

    read table mt_object_packages into ls_obj_sig with key obj_type = 'FUNC' obj_name = iv_func_name.

    if sy-subrc <> 0.
      select single devclass into ls_obj_sig-package
        from info_func
        where funcname = iv_func_name.
      if ls_obj_sig-package is not initial.
        ls_obj_sig-obj_type = 'FUNC'.
        ls_obj_sig-obj_name = iv_func_name.
        insert ls_obj_sig into table mt_object_packages.
      endif.
    endif.

    rv_package = ls_obj_sig-package.

  endmethod.


  method get_incl_package.

    data lv_program type progname.
    data lv_area    type rs38l_area.

    lv_program = iv_prog_name.

    call function 'FUNCTION_INCLUDE_CONCATENATE'
      changing
        program                  = lv_program
        complete_area            = lv_area
      exceptions
        not_enough_input         = 1
        no_function_pool         = 2
        delimiter_wrong_position = 3
        others                   = 4.

    if lv_area is initial.
      select single master from d010inc into lv_program
        where include = iv_prog_name.

      call function 'FUNCTION_INCLUDE_CONCATENATE'
        changing
          program                  = lv_program
          complete_area            = lv_area
        exceptions
          not_enough_input         = 1
          no_function_pool         = 2
          delimiter_wrong_position = 3
          others                   = 4.
    endif.

    if lv_area is not initial.
      rv_package = get_obj_package(
        iv_obj_type = 'FUGR'
        iv_obj_name = |{ lv_area }| ).
      return.
    endif.

    " TODO more ...

  endmethod.


  method get_obj_package.

    " see also zcl_abapgit_tadir->get_object_package for checks

    data ls_obj_sig like line of mt_object_packages.

    read table mt_object_packages into ls_obj_sig with key obj_type = iv_obj_type obj_name = iv_obj_name.

    if sy-subrc <> 0.
      ls_obj_sig-package = zcl_abapgit_tadir_clone=>new( )->read_single(
        iv_object   = iv_obj_type
        iv_obj_name = iv_obj_name )-devclass.
      if ls_obj_sig-package is not initial.
        ls_obj_sig-obj_type = iv_obj_type.
        ls_obj_sig-obj_name = iv_obj_name.
        insert ls_obj_sig into table mt_object_packages.
      endif.
    endif.

    rv_package = ls_obj_sig-package.

  endmethod.


  method get_where_used.

    data lt_findstrings type string_table.
    data lt_scope       like it_scope.
    data lv_findstring  like line of lt_findstrings.

    if iv_obj_name is initial.
      return.
    endif.

    lt_scope = it_scope.

    lv_findstring = iv_obj_name.
    insert lv_findstring into table lt_findstrings.

    call function 'RS_EU_CROSSREF'
      exporting
        i_find_obj_cls           = iv_obj_type
        no_dialog                = abap_true
        without_text             = abap_true
      tables
        i_findstrings            = lt_findstrings
        o_founds                 = rt_findings
        i_scope_object_cls       = lt_scope
        i_scope_devclass         = ir_package_scope
      exceptions
        not_executed             = 1
        not_found                = 2
        illegal_object           = 3
        no_cross_for_this_object = 4
        batch                    = 5
        batchjob_error           = 6
        wrong_type               = 7
        object_not_exist         = 8
        others                   = 9.

    if sy-subrc = 1 or sy-subrc = 2 or lines( rt_findings ) = 0.
      return.
    elseif sy-subrc > 2.
      zcx_dependency_error=>raise( |RS_EU_CROSSREF({ sy-subrc }) for { iv_obj_type } { iv_obj_name }| ).
    endif.

  endmethod.


  method select_external_usages.

    data lt_tadir type zcl_abapgit_tadir_clone=>ty_tadir_tt.
    data lt_where_used type tty_where_used.
    data lt_package_scope like ir_package_scope.

    lt_tadir = zcl_abapgit_tadir_clone=>new( )->read( i_package ).

    lt_package_scope = build_package_scope(
      ir_package_scope = ir_package_scope
      it_tadir         = lt_tadir ).

    rt_objs = collect_where_used(
      ir_package_scope = lt_package_scope
      it_tadir         = lt_tadir ).

  endmethod.
ENDCLASS.

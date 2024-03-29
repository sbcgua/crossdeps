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

ENDCLASS.



CLASS ZCL_DEPENDENCY_MODEL_WU IMPLEMENTATION.


  method get_obj_package.

    " see also zcl_abapgit_tadir->get_object_package for checks

    data ls_obj_sig like line of mt_object_packages.

    read table mt_object_packages into ls_obj_sig with key obj_type = iv_obj_type obj_name = iv_obj_name.

    if sy-subrc <> 0.
      select single devclass from tadir into ls_obj_sig-package
        where pgmid  = 'R3TR'
        and object   = iv_obj_type
        and obj_name = iv_obj_name.
      if sy-subrc = 0 and ls_obj_sig-package is not initial.
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

    " See also CL_FINB_GN_BBI=>GET_CROSSREF

    " TODO interface detection
    " include detection
    " FF

*INTF - check by tadir
*FF -> find main program / or indicate this is FM and find PKG in another way (FUNC), TFDIR?
  " FUNCTION_INCLUDE_INFO
  " view INFO_FUNC

*some includes are FUGR and some are ENHO ...


    data lt_tadir type zcl_abapgit_tadir_clone=>ty_tadir_tt.
    data lt_where_used type tty_where_used.
    data lt_where_used_portion type tty_where_used.
    data lt_package_scope like ir_package_scope.
    data lt_dev_obj_cache type hashed table of ty_dev_object with unique key type.

    field-symbols <tadir> like line of lt_tadir.
    field-symbols <dep> like line of rt_objs.
    field-symbols <use> like line of lt_where_used.
    field-symbols <pkg> like line of lt_package_scope.
    field-symbols <devobj> like line of lt_dev_obj_cache.

    data lo_tadir type ref to zcl_abapgit_tadir_clone.

    lo_tadir = zcl_abapgit_tadir_clone=>new( ).
    lt_tadir = lo_tadir->read( i_package ).

    " Define package scope
    lt_package_scope = ir_package_scope.
    loop at lt_tadir assigning <tadir> where object = 'DEVC'.
      append initial line to lt_package_scope assigning <pkg>.
      <pkg>-sign   = 'E'.
      <pkg>-option = 'EQ'.
      <pkg>-low    = <tadir>-obj_name.
    endloop.

    " Collect where used
    data li_progress type ref to zif_abapgit_progress_clone.
    li_progress = zcl_abapgit_progress_clone=>get_instance( lines( lt_tadir ) ).
    loop at lt_tadir assigning <tadir> where object <> 'DEVC'.

      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |{ <tadir>-object } { <tadir>-obj_name }| ).

      lt_where_used_portion = get_where_used(
        iv_obj_type = |{ <tadir>-object }|
        iv_obj_name = <tadir>-obj_name
        ir_package_scope = lt_package_scope ).

      append lines of lt_where_used_portion to lt_where_used.

    endloop.
    li_progress->off( ).

    select type tadir from euobjedit into table lt_dev_obj_cache.

    " Convert
    loop at lt_where_used assigning <use>.

      append initial line to rt_objs assigning <dep>.
      <dep>-cnt = 1.
      <dep>-dep_package  = <tadir>-devclass.
      <dep>-dep_obj_type = <tadir>-object.
      <dep>-dep_obj_name = <tadir>-obj_name.
      <dep>-dep_used_obj = <use>-used_obj.
      <dep>-dep_used_cls = <use>-used_cls.

      <dep>-obj_cls = <use>-object_cls.

      read table lt_dev_obj_cache assigning <devobj> with key type = <use>-object_cls.
      if sy-subrc = 0 and <devobj>-tadir is not initial.
        <dep>-obj_type = <devobj>-tadir.
      endif.

      <dep>-obj_name = <use>-encl_objec.
      if <dep>-obj_name is initial.
        <dep>-obj_name = <use>-object.
      endif.

      <dep>-package = lo_tadir->read_single(
        iv_object   = <dep>-obj_type
        iv_obj_name = <dep>-obj_name )-devclass.

    endloop.

  endmethod.
ENDCLASS.

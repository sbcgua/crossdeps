* Inspired and reuses some code of this
* https://github.com/larshp/dependencies/blob/master/zdependencies3.prog.abap

class lcl_app definition final.
  public section.

    types ty_devc_range type range of tadir-devclass.

    methods run
      importing
        i_packages type ty_devc_range
        i_external_only type abap_bool
        i_only_deps_from type ty_devc_range optional.

  private section.

    types:
      begin of ty_dependency,
        package type devclass,
        obj_type type tadir-object,
        obj_name type tadir-obj_name,
        dep_package type devclass,
        dep_obj_type type tadir-object,
        dep_obj_name type tadir-obj_name,
        cnt type i,
      end of ty_dependency.
    types:
      tty_dependency type standard table of ty_dependency with default key.

    methods select
      importing
        i_package type devclass
*        i_only_deps_from type ty_devc_range
      returning
        value(rt_objs) type tty_dependency.

    methods display
      importing
        it_objs type tty_dependency.

    methods collect_dependencies
      importing
        i_tadir_obj type zif_abapgit_definitions=>ty_tadir
      changing
        ct_objs type tty_dependency.

endclass.

class lcl_app implementation.
  method run.

    data lt_objs_all type tty_dependency.
    data lt_objs type tty_dependency.
    data lt_packages type table of devclass.
    data lv_package like line of lt_packages.
    field-symbols <obj> like line of lt_objs_all.

    if i_packages is initial.
      message 'Enter a package' type 'E' display like 'S'.
      return.
    endif.

    select devclass into table lt_packages from tdevc where devclass in i_packages.

    loop at lt_packages into lv_package.
      lt_objs = select( i_package = lv_package ).
      append lines of lt_objs to lt_objs_all.
    endloop.

    if i_only_deps_from is not initial.
      delete lt_objs_all where dep_package not in i_only_deps_from.
    endif.

    if i_external_only = abap_true.
      loop at lt_objs_all assigning <obj>.
        if <obj>-package = <obj>-dep_package.
          delete lt_objs_all index sy-tabix.
        endif.
      endloop.
    endif.

    display( lt_objs_all ).

  endmethod.


  method display.
    data lx type ref to cx_root.
    try .
      data grid type ref to zcl_dependency_view_base.
      grid = zcl_dependency_view_base=>create_simple(
        it_content   = it_objs
        iv_title     = 'Cross dependencies'
        iv_technames = abap_true ).
      grid->set_aggregations( 'cnt' ).
      grid->set_sorting( 'obj_type, ^obj_name, dep_package, dep_obj_type, dep_obj_name' ).
      grid->display( ).
    catch cx_static_check into lx.
      data msg type string.
      msg = lx->get_text( ).
      message msg type 'E' display like 'S'.
    endtry.
  endmethod.

  method collect_dependencies.

    data lt_environment type senvi_tab.
    data obj like line of ct_objs.
    data lv_obj_type type euobj-id.
    field-symbols <env> like line of lt_environment.

    lv_obj_type  = i_tadir_obj-object.

    call function 'REPOSITORY_ENVIRONMENT_SET'
      exporting
        obj_type       = lv_obj_type
        object_name    = i_tadir_obj-obj_name
      tables
        environment    = lt_environment
      exceptions
        batch          = 1
        batchjob_error = 2
        not_executed   = 3
        others         = 4.
    if sy-subrc = 3.
      return.
    elseif sy-subrc <> 0.
      break-point.
    endif.

    data lt_deps type if_ris_environment_types=>ty_t_senvi_tadir.
    cl_wb_ris_environment=>convert_senvi_to_tadir(
      exporting
        senvi       = lt_environment
      importing
        senvi_tadir = lt_deps ).
    sort lt_deps by ref_obj_type ascending ref_obj_name ascending.
    delete adjacent duplicates from lt_deps comparing ref_obj_type ref_obj_name.

    field-symbols <dep> like line of lt_deps.
    obj-cnt      = 1.
    obj-package  = i_tadir_obj-devclass.
    obj-obj_type = i_tadir_obj-object.
    obj-obj_name = i_tadir_obj-obj_name.

    loop at lt_deps assigning <dep>.
      if not <dep>-ref_obj_name+0(1) co 'ZY'.
        continue.
      endif.

      obj-dep_obj_type = <dep>-ref_obj_type.
      obj-dep_obj_name = <dep>-ref_obj_name.

      select single devclass from tadir into obj-dep_package
        where pgmid  = 'R3TR'
        and object   = <dep>-ref_obj_type
        and obj_name = <dep>-ref_obj_name.
      if sy-subrc <> 0.
        continue.
      endif.

      if not obj-dep_package+0(1) co '$ZY'.
        continue.
      endif.

      append obj to ct_objs.
    endloop.

  endmethod.

  method select.

    data obj like line of rt_objs.
    data lt_tadir type zif_abapgit_definitions=>ty_tadir_tt.
    field-symbols <tadir> like line of lt_tadir.

    try .
      lt_tadir = zcl_abapgit_factory=>get_tadir( )->read( i_package ).
      " Selects subpackages ??? to check
    catch zcx_abapgit_exception.
    endtry.

    loop at lt_tadir assigning <tadir> where object <> 'DEVC'.

      obj-package  = i_package.
      obj-obj_type = <tadir>-object.
      obj-obj_name = <tadir>-obj_name.
      collect_dependencies(
        exporting
          i_tadir_obj = <tadir>
        changing
          ct_objs = rt_objs ).

    endloop.

  endmethod.
endclass.

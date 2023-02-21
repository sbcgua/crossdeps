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

    methods display
      importing
        it_objs type zcl_dependency_model=>tty_dependency.

endclass.

class lcl_app implementation.

  method run.

    data lt_objs_all type zcl_dependency_model=>tty_dependency.
    data lt_objs type zcl_dependency_model=>tty_dependency.
    data lt_packages type table of devclass.
    data lv_package like line of lt_packages.
    data lo_model type ref to zcl_dependency_model.

    field-symbols <obj> like line of lt_objs_all.

    if i_packages is initial.
      message 'Enter a package' type 'E' display like 'S'.
      return.
    endif.

    select devclass into table lt_packages from tdevc where devclass in i_packages.

    create object lo_model.

    loop at lt_packages into lv_package.
      lt_objs = lo_model->select_by_package( i_package = lv_package ).
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

    try.
      data grid type ref to zcl_dependency_view_base.
      grid = zcl_dependency_view_base=>create_simple(
        it_content   = it_objs
        iv_title     = 'Cross dependencies'
        iv_technames = abap_true ).
      grid->set_aggregations( 'cnt' ).
      grid->set_sorting( 'obj_type, ^obj_name, dep_package, dep_obj_type, dep_obj_name' ).
      grid->display( ).
    catch cx_static_check into lx.
      message lx type 'E' display like 'S'.
    endtry.
  endmethod.

endclass.

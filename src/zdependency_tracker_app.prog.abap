* Inspired and reuses some code of this
* https://github.com/larshp/dependencies/blob/master/zdependencies3.prog.abap

class lcl_app definition final.
  public section.

    class-methods new
      returning
        value(ro_instance) type ref to lcl_app.

    methods run_for_package
      importing
        i_packages type zcl_dependency_model=>ty_devc_range
        i_external_only type abap_bool
        i_only_deps_from type zcl_dependency_model=>ty_devc_range.

    methods run_for_object
      importing
        i_obj_type type tadir-object
        i_obj_name type tadir-obj_name
        i_deep     type abap_bool
        i_only_deps_from type zcl_dependency_model=>ty_devc_range optional.

  private section.

    methods display
      importing
        it_objs type zcl_dependency_model=>tty_dependency.

endclass.

class lcl_app implementation.

  method new.
    create object ro_instance.
  endmethod.

  method run_for_package.

    data lt_objs_all type zcl_dependency_model=>tty_dependency.
    data lt_objs type zcl_dependency_model=>tty_dependency.
    data lt_packages type table of devclass.
    data lv_package like line of lt_packages.
    data lo_model type ref to zcl_dependency_model.

    field-symbols <obj> like line of lt_objs_all.

    if i_packages is initial.
      message 'Enter a package' type 'S' display like 'E'.
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

  method run_for_object.

    data lt_objs_all type zcl_dependency_model=>tty_dependency.
    data lv_obj_package type tadir-devclass.
    data lo_model type ref to zcl_dependency_model.

    if i_obj_type is initial or i_obj_name is initial.
      message 'Enter a object name and type' type 'S' display like 'E'.
      return.
    endif.

    select single devclass into lv_obj_package
      from tadir
      where pgmid = 'R3TR'
      and object = i_obj_type
      and obj_name = i_obj_name.

    if sy-subrc <> 0.
      message 'Object does not exist' type 'S' display like 'E'.
      return.
    endif.

    create object lo_model.

    if i_deep = abap_true.
      lt_objs_all = lo_model->select_by_object(
        i_package  = lv_obj_package
        i_obj_type = i_obj_type
        i_obj_name = i_obj_name ).
    else.
      lo_model->collect_dependencies(
        exporting
          i_devclass = lv_obj_package
          i_obj_type = i_obj_type
          i_obj_name = i_obj_name
        changing
          ct_objs    = lt_objs_all ).
    endif.

    if i_only_deps_from is not initial.
      delete lt_objs_all where dep_package not in i_only_deps_from.
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

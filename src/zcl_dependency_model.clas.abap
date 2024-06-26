class ZCL_DEPENDENCY_MODEL definition
  public
  final
  create public .

  public section.

    methods select_by_package
      importing
        i_package type tadir-devclass
      returning
        value(rt_objs) type zif_dependency_types=>tty_dependency.

    methods select_by_object
      importing
        i_package  type tadir-devclass
        i_obj_type type tadir-object
        i_obj_name type tadir-obj_name
        ir_package_scope type zif_dependency_types=>ty_devc_range optional
      returning
        value(rt_objs) type zif_dependency_types=>tty_dependency.

    methods collect_dependencies
      importing
        i_devclass type tadir-devclass
        i_obj_type type tadir-object
        i_obj_name type tadir-obj_name
      changing
        ct_objs type zif_dependency_types=>tty_dependency.

  protected section.
  private section.


ENDCLASS.



CLASS ZCL_DEPENDENCY_MODEL IMPLEMENTATION.


  method collect_dependencies.

    data lt_environment type senvi_tab.
    data obj like line of ct_objs.
    data lv_obj_type type euobj-id.
    field-symbols <env> like line of lt_environment.

    lv_obj_type  = i_obj_type.

    call function 'REPOSITORY_ENVIRONMENT_SET'
      exporting
        obj_type       = lv_obj_type
        object_name    = i_obj_name
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
      return.
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
    obj-package  = i_devclass.
    obj-obj_type = i_obj_type.
    obj-obj_name = i_obj_name.

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


  method select_by_object.

    data lt_queue type standard table of zif_dependency_types=>ty_obj_signature.
    data lt_processed type sorted table of zif_dependency_types=>ty_dependency
          with unique key dep_package dep_obj_type dep_obj_name.
    data lt_portion type zif_dependency_types=>tty_dependency.
    data ls_obj_sig like line of lt_queue.

    field-symbols <obj> like line of lt_processed.

    ls_obj_sig-package  = i_package.
    ls_obj_sig-obj_type = i_obj_type.
    ls_obj_sig-obj_name = i_obj_name.
    append ls_obj_sig to lt_queue.

    loop at lt_queue into ls_obj_sig.
      clear lt_portion.
      collect_dependencies(
        exporting
          i_devclass = ls_obj_sig-package
          i_obj_type = ls_obj_sig-obj_type
          i_obj_name = ls_obj_sig-obj_name
        changing
          ct_objs = lt_portion ).
      if ir_package_scope is not initial.
        delete lt_portion where dep_package not in ir_package_scope.
      endif.

      loop at lt_portion assigning <obj>.
        read table lt_processed
          transporting no fields
          with key
            dep_package  = <obj>-dep_package
            dep_obj_type = <obj>-dep_obj_type
            dep_obj_name = <obj>-dep_obj_name.
        if sy-subrc <> 0.
          ls_obj_sig-package  = <obj>-dep_package.
          ls_obj_sig-obj_type = <obj>-dep_obj_type.
          ls_obj_sig-obj_name = <obj>-dep_obj_name.
          append ls_obj_sig to lt_queue.
          insert <obj> into table lt_processed.
        endif.
      endloop.
    endloop.

    rt_objs = lt_processed.

  endmethod.


  method select_by_package.

    data lt_tadir type zcl_abapgit_tadir_clone=>ty_tadir_tt.
    field-symbols <tadir> like line of lt_tadir.

    lt_tadir = zcl_abapgit_tadir_clone=>new( )->read( i_package ).

    loop at lt_tadir assigning <tadir> where object <> 'DEVC'.

      collect_dependencies(
        exporting
          i_devclass = <tadir>-devclass
          i_obj_type = <tadir>-object
          i_obj_name = <tadir>-obj_name
        changing
          ct_objs = rt_objs ).

    endloop.

  endmethod.
ENDCLASS.

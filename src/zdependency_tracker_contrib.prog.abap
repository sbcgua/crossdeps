*&---------------------------------------------------------------------*
*&  This include containes external components
*&  taken from abapgit code repository (MIT license)
*&---------------------------------------------------------------------*

**********************************************************************
* CONTRIB: ZCL_ABAPGIT_TADIR
**********************************************************************

CLASS zcl_abapgit_tadir_clone DEFINITION FINAL.
  " this class is a reduced version of
  " https://github.com/abapGit/abapGit/blob/main/src/objects/core/zcl_abapgit_tadir.clas.abap

  PUBLIC SECTION.

    types:
      begin of ty_tadir,
        pgmid     type tadir-pgmid,
        object    type tadir-object,
        obj_name  type tadir-obj_name,
        devclass  type tadir-devclass,
        korrnum   type tadir-korrnum,
        delflag   type tadir-delflag,
        genflag   type tadir-genflag,
        path      type string,
        srcsystem type tadir-srcsystem,
      end of ty_tadir .
    types:
      ty_tadir_tt type standard table of ty_tadir with default key .
    types:
      ty_devclass_tt type standard table of devclass with default key .

    CLASS-METHODS new RETURNING VALUE(ro_instance) TYPE REF TO zcl_abapgit_tadir_clone.
    METHODS read
      IMPORTING
        !iv_package            TYPE tadir-devclass
        !iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
        !iv_only_local_objects TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_tadir)        TYPE ty_tadir_tt.
    METHODS read_single
      IMPORTING
        !iv_pgmid       TYPE tadir-pgmid DEFAULT 'R3TR'
        !iv_object      TYPE tadir-object
        !iv_obj_name    TYPE tadir-obj_name
      RETURNING
        VALUE(rs_tadir) TYPE zif_abapgit_definitions=>ty_tadir.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS build
      IMPORTING
        !iv_package            TYPE tadir-devclass
        !iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
        !iv_only_local_objects TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_tadir)        TYPE ty_tadir_tt.
    METHODS select_objects
      IMPORTING
        !iv_package            TYPE tadir-devclass
        !iv_ignore_subpackages TYPE abap_bool DEFAULT abap_false
        !iv_only_local_objects TYPE abap_bool
      EXPORTING
        !et_packages           TYPE ty_devclass_tt
        !et_tadir              TYPE ty_tadir_tt.
    METHODS add_local_packages
      IMPORTING
        !it_packages TYPE ty_devclass_tt
      CHANGING
        !ct_tadir    TYPE ty_tadir_tt.
    METHODS add_namespaces
      IMPORTING
        !iv_package TYPE devclass
      CHANGING
        !ct_tadir   TYPE ty_tadir_tt.
    METHODS list_subpackages
      IMPORTING
        iv_package  TYPE tadir-devclass
      RETURNING
        VALUE(rt_list) TYPE ty_devclass_tt .

ENDCLASS.



CLASS zcl_abapgit_tadir_clone IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_instance.
  ENDMETHOD.

  METHOD list_subpackages.

    DATA: lt_list     LIKE rt_list.

    SELECT devclass FROM tdevc
      INTO TABLE lt_list
      WHERE parentcl = iv_package
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    rt_list = lt_list.
    WHILE lines( lt_list ) > 0.

      SELECT devclass FROM tdevc
        INTO TABLE lt_list
        FOR ALL ENTRIES IN lt_list
        WHERE parentcl = lt_list-table_line
        ORDER BY PRIMARY KEY.             "#EC CI_SUBRC "#EC CI_GENBUFF
      APPEND LINES OF lt_list TO rt_list.

    ENDWHILE.

  ENDMETHOD.

  METHOD add_local_packages.

    FIELD-SYMBOLS:
      <lv_package> LIKE LINE OF it_packages,
      <ls_tadir>   LIKE LINE OF ct_tadir.

    LOOP AT it_packages ASSIGNING <lv_package>.

      " Local packages are not in TADIR, only in TDEVC, act as if they were
      IF <lv_package> CP '$*'. " OR <package> CP 'T*' ).
        APPEND INITIAL LINE TO ct_tadir ASSIGNING <ls_tadir>.
        <ls_tadir>-pgmid    = 'R3TR'.
        <ls_tadir>-object   = 'DEVC'.
        <ls_tadir>-obj_name = <lv_package>.
        <ls_tadir>-devclass = <lv_package>.
        <ls_tadir>-srcsystem = sy-sysid.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD add_namespaces.

    DATA:
      lv_name           TYPE progname,
      lv_namespace      TYPE namespace,
      lv_prev_namespace TYPE namespace,
      lt_tadir_nspc     TYPE ty_tadir_tt.

    FIELD-SYMBOLS:
      <ls_tadir> LIKE LINE OF ct_tadir,
      <ls_nspc>  LIKE LINE OF ct_tadir.

    LOOP AT ct_tadir ASSIGNING <ls_tadir> WHERE obj_name(1) = '/'.

      " Namespaces are not in TADIR, but are necessary for creating objects in transportable packages
      lv_name = <ls_tadir>-obj_name.

      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace = lv_name
        IMPORTING
          namespace           = lv_namespace
        EXCEPTIONS
          delimiter_error     = 1
          OTHERS              = 2.

      IF sy-subrc = 0 AND lv_namespace IS NOT INITIAL
         AND lv_namespace <> lv_prev_namespace.

        READ TABLE lt_tadir_nspc TRANSPORTING NO FIELDS
          WITH KEY pgmid = 'R3TR' object = 'NSPC' obj_name = lv_namespace.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO ct_tadir ASSIGNING <ls_nspc>.
          <ls_nspc>-pgmid    = 'R3TR'.
          <ls_nspc>-object   = 'NSPC'.
          <ls_nspc>-obj_name = lv_namespace.
          <ls_nspc>-devclass = iv_package.
          <ls_nspc>-srcsystem = sy-sysid.

          INSERT <ls_nspc> INTO TABLE lt_tadir_nspc.
        ENDIF.
        lv_prev_namespace = lv_namespace.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD build.

    DATA lt_packages TYPE ty_devclass_tt.

    select_objects(
      EXPORTING
        iv_package            = iv_package
        iv_ignore_subpackages = iv_ignore_subpackages
        iv_only_local_objects = iv_only_local_objects
      IMPORTING
        et_tadir              = rt_tadir
        et_packages           = lt_packages ).

*    skip_objects. STRIPPED

    add_local_packages(
      EXPORTING
        it_packages = lt_packages
      CHANGING
        ct_tadir    = rt_tadir ).

    add_namespaces(
      EXPORTING
        iv_package = iv_package
      CHANGING
        ct_tadir   = rt_tadir ).

*    determine_path. STRIPPED
*    adjust_objects. STRIPPED

  ENDMETHOD.

  METHOD select_objects.

    DATA:
      lt_excludes  TYPE RANGE OF trobjtype,
      ls_exclude   LIKE LINE OF lt_excludes,
      lt_srcsystem TYPE RANGE OF tadir-srcsystem,
      ls_srcsystem LIKE LINE OF lt_srcsystem.

    " Determine packages to read
    IF iv_ignore_subpackages = abap_false.
      et_packages = list_subpackages( iv_package ).
    ENDIF.
    INSERT iv_package INTO et_packages INDEX 1.

    " Exclude object types with tadir entries that are included elsewhere
    ls_exclude-sign   = 'I'.
    ls_exclude-option = 'EQ'.
    ls_exclude-low    = 'SOTR'. " automatically create for sap packages (DEVC)
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low    = 'SFB1'. " covered by business function sets (SFBS)
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low    = 'SFB2'. " covered by business functions (SFBF)
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low    = 'STOB'. " auto generated by core data services (DDLS)
    APPEND ls_exclude TO lt_excludes.

    " Limit to objects belonging to this system
    IF iv_only_local_objects = abap_true.
      ls_srcsystem-sign   = 'I'.
      ls_srcsystem-option = 'EQ'.
      ls_srcsystem-low    = sy-sysid.
      APPEND ls_srcsystem TO lt_srcsystem.
    ENDIF.

    IF et_packages IS NOT INITIAL.
      SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE et_tadir
        FOR ALL ENTRIES IN et_packages
        WHERE devclass = et_packages-table_line
        AND pgmid      = 'R3TR'
        AND object     NOT IN lt_excludes
        AND delflag    = abap_false
        AND srcsystem  IN lt_srcsystem
        ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS. "#EC CI_GENBUFF "#EC CI_SUBRC
    ENDIF.

    SORT et_tadir BY devclass pgmid object obj_name.

  ENDMETHOD.

  METHOD read.

    " Start recursion
    " hmm, some problems here, should TADIR also build path?
    rt_tadir = build(
      iv_package            = iv_package
      iv_ignore_subpackages = iv_ignore_subpackages
      iv_only_local_objects = iv_only_local_objects ).

*    rt_tadir = check_exists( rt_tadir ).

  ENDMETHOD.

  METHOD read_single.

    SELECT SINGLE * FROM tadir INTO CORRESPONDING FIELDS OF rs_tadir
      WHERE pgmid = iv_pgmid
      AND object = iv_object
      AND obj_name = iv_obj_name.                         "#EC CI_SUBRC
    CLEAR rs_tadir-korrnum.

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* CONTRIB: zcl_abapgit_progress_clone
**********************************************************************

INTERFACE zif_abapgit_progress_clone.

  METHODS show
    IMPORTING
      !iv_current TYPE i
      !iv_text    TYPE csequence .
  METHODS set_total
    IMPORTING
      !iv_total TYPE i .
  METHODS off .
ENDINTERFACE.

  " this class is a reduced version of
  " https://github.com/abapGit/abapGit/main/src/ui/progress/zcl_abapgit_progress_clone.clas.abap

CLASS zcl_abapgit_progress_clone DEFINITION
  FINAL
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_progress_clone .

    CLASS-METHODS get_instance
      IMPORTING
        !iv_total          TYPE i
      RETURNING
        VALUE(ri_progress) TYPE REF TO zif_abapgit_progress_clone .
  PROTECTED SECTION.

    DATA mv_total TYPE i .
    CLASS-DATA gi_progress TYPE REF TO zif_abapgit_progress_clone .

    METHODS calc_pct
      IMPORTING
        !iv_current   TYPE i
      RETURNING
        VALUE(rv_pct) TYPE i .
  PRIVATE SECTION.

    DATA mv_cv_time_next TYPE sy-uzeit .
    DATA mv_cv_datum_next TYPE sy-datum .
ENDCLASS.



CLASS zcl_abapgit_progress_clone IMPLEMENTATION.
  METHOD calc_pct.

    DATA: lv_f TYPE f.

    lv_f = ( iv_current / mv_total ) * 100.
    rv_pct = lv_f.

    IF rv_pct = 100.
      rv_pct = 99.
    ELSEIF rv_pct = 0.
      rv_pct = 1.
    ENDIF.

  ENDMETHOD.

  METHOD get_instance.

* max one progress indicator at a time is supported

    IF gi_progress IS INITIAL.
      CREATE OBJECT gi_progress TYPE zcl_abapgit_progress_clone.
    ENDIF.

    gi_progress->set_total( iv_total ).

    ri_progress = gi_progress.

  ENDMETHOD.

  METHOD zif_abapgit_progress_clone~off.

    " Clear the status bar
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'.

  ENDMETHOD.

  METHOD zif_abapgit_progress_clone~set_total.

    mv_total = iv_total.

    CLEAR mv_cv_time_next.
    CLEAR mv_cv_datum_next.

  ENDMETHOD.

  METHOD zif_abapgit_progress_clone~show.

    DATA: lv_pct  TYPE i,
          lv_time TYPE t.

    CONSTANTS: lc_wait_secs TYPE i VALUE 1.

    GET TIME.
    lv_time = sy-uzeit.
    IF mv_cv_time_next IS INITIAL AND mv_cv_datum_next IS INITIAL.
      mv_cv_time_next  = lv_time.
      mv_cv_datum_next = sy-datum.
    ENDIF.

    "We only do a progress indication if enough time has passed
    IF lv_time >= mv_cv_time_next
        AND sy-datum = mv_cv_datum_next
        OR sy-datum > mv_cv_datum_next.

      lv_pct = calc_pct( iv_current ).

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lv_pct
          text       = iv_text.
      mv_cv_time_next = lv_time + lc_wait_secs.

    ENDIF.
    IF sy-datum > mv_cv_datum_next.
      mv_cv_datum_next = sy-datum.
    ENDIF.
    IF mv_cv_time_next < lv_time.
      mv_cv_datum_next = sy-datum + 1.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

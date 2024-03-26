report zdependency_tracker.

include zdependency_tracker_app.
include zdependency_tracker_sel.

form main.

  data lx type ref to cx_root.

  try.

    if p_m_pkg = abap_true.
      lcl_app=>new( )->run_for_package(
        i_packages       = s_devc[]
        i_only_deps_from = s_dp[]
        i_external_only  = p_extern ).
    elseif p_m_deep = abap_true.
      lcl_app=>new( )->run_for_object(
        i_obj_type       = p_otype
        i_obj_name       = p_oname
        i_deep           = p_deep
        i_only_deps_from = s_dp[] ).
    elseif p_m_wuse = abap_true.
      lcl_app=>new( )->run_where_used(
        i_packages       = s_devc[]
        i_only_deps_from = s_dp[] ).
    else.
      zcx_dependency_error=>raise( 'Mode undefined' ).
    endif.

  catch cx_root into lx.
    message lx type 'S' display like 'E'.
  endtry.

endform.

start-of-selection.
  perform main.

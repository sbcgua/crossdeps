report zdependency_tracker.

include zdependency_tracker_app.
include zdependency_tracker_sel.

start-of-selection.

  if p_m_pkg = abap_true.
    lcl_app=>new( )->run_for_package(
      i_packages       = s_devc[]
      i_only_deps_from = s_dp[]
      i_external_only  = p_extern ).
  elseif p_m_deep = abap_true.
    lcl_app=>new( )->run_for_object(
      i_obj_type       = p_otype
      i_obj_name       = p_oname
      i_only_deps_from = s_dp[] ).
  else.
    message 'Mode undefined' type 'S' display like 'E'.
  endif.

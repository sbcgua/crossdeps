report zdependency_tracker.

include zdependency_tracker_app.
include zdependency_tracker_sel.

start-of-selection.

  data g_app type ref to lcl_app.
  create object g_app.
  g_app->run(
    i_packages       = s_devc[]
    i_only_deps_from = s_dp[]
    i_external_only  = p_extern ).

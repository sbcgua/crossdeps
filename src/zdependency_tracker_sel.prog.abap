tables: tadir.

selection-screen begin of block b1 with frame title txt_b1.

parameters p_m_pkg type xfeld radiobutton group mode default 'X' user-command sw.
parameters p_m_deep type xfeld radiobutton group mode.
parameters p_m_wuse type xfeld radiobutton group mode.

selection-screen end of block b1.

selection-screen begin of block b2 with frame title txt_b2.

parameters p_otype type tadir-object modif id md.
parameters p_oname type tadir-obj_name modif id md.
select-options s_devc for tadir-devclass modif id mp.
select-options s_dp for tadir-devclass.
parameters p_extern type xfeld modif id mp.
parameters p_deep type xfeld modif id md default 'X'.

selection-screen end of block b2.

initialization.
  txt_b1 = 'Mode'.              "#EC NOTEXT
  txt_b2 = 'Parameters'.        "#EC NOTEXT

at selection-screen output.
  perform modify_screen.

form modify_screen.
  data active_group like screen-group1.
  if p_m_pkg = abap_true or p_m_wuse = abap_true.
    active_group = 'MP'.
  else.
    active_group = 'MD'.
  endif.

  loop at screen.
    check screen-group1 is not initial.
    if screen-group1 = active_group.
      screen-active = '1'.
    else.
      screen-active = '0'.
    endif.
    if p_m_wuse = abap_true and screen-name = 'P_EXTERN'.
      screen-active = '0'.
    endif.
    modify screen.
  endloop.
endform.

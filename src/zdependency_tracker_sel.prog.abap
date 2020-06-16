tables: tadir.

selection-screen begin of block b1 with frame title txt_b1.

*selection-screen begin of line.
**selection-screen comment (20) t_devc for field s_devc.
**selection-screen comment (20) t_dp for field s_dp.
*selection-screen end of line.

select-options: s_devc for tadir-devclass obligatory default 'ZUA_VAT_VF'.
select-options: s_dp for tadir-devclass.
parameters: p_extern type xfeld.

selection-screen end of block b1.

initialization.
  txt_b1   = 'Parameters'.        "#EC NOTEXT
*  t_devc   = 'Package'.           "#EC NOTEXT
*  t_dp     = 'Dependent on'.      "#EC NOTEXT

*at selection-screen on value-request for p_devc.
*  perform f4_srcdir_path changing p_dir.

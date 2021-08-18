tables: tadir.

selection-screen begin of block b1 with frame title txt_b1.

select-options: s_devc for tadir-devclass obligatory.
select-options: s_dp for tadir-devclass.
parameters: p_extern type xfeld.

selection-screen end of block b1.

initialization.
  txt_b1 = 'Parameters'.        "#EC NOTEXT

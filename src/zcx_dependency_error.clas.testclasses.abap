class ltcl_error_test definition final
  for testing
  duration short
  risk level harmless.

  private section.

    methods test for testing.

endclass.

class ltcl_error_test implementation.

  method test.

    data lx type ref to zcx_dependency_error.

    try.
      zcx_dependency_error=>raise( 'hello' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_dependency_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->get_text( )
        exp = 'hello' ).
    endtry.

  endmethod.

endclass.

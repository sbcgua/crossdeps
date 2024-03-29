interface zif_dependency_types
  public.

  types ty_devc_range type range of tadir-devclass.
  types:
    begin of ty_obj_signature,
      package type devclass,
      obj_type type tadir-object,
      obj_name type tadir-obj_name,
    end of ty_obj_signature.
  types:
    begin of ty_dependency,
      package type devclass,
      obj_type type tadir-object,
      obj_name type tadir-obj_name,
      obj_cls  type rsfindlst-object_cls,
      dep_package type devclass,
      dep_obj_type type tadir-object,
      dep_obj_name type tadir-obj_name,
      dep_used_cls type rsfindlst-used_cls,
      dep_used_obj type rsfindlst-used_obj,
      cnt type i,
    end of ty_dependency.
  types:
    tty_dependency type standard table of ty_dependency with default key.

endinterface.

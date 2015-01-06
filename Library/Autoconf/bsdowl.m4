dnl autoconf macros for BSD Owl
dnl
dnl Copyright © 2015 Michael Grünewald

AC_DEFUN([AC_WITH_OCAML_SITE_LIB],
[dnl
  AC_ARG_WITH([ocaml-site-lib],
    [AS_HELP_STRING([--with-ocaml-site-lib],
      [install under OCaml site-lib's directory])],
    [WITH_OCAML_SITE_LIB=yes],
    [WITH_OCAML_SITE_LIB=no])
  AC_SUBST([WITH_OCAML_SITE_LIB])
])

AC_INIT(README)
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(fsharp-refactor, 0.1)
AM_MAINTAINER_MODE

AC_PROG_INSTALL

dnl pkg-config
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test "x$PKG_CONFIG" = "xno"; then
	AC_MSG_ERROR([You need to install pkg-config])
fi

dnl F# compiler
AC_PATH_PROG(FSC, fsharpc, no)
AC_PATH_PROG(MONO, mono, no)

FS="F#"
if test "x$FSC" = "xno" ; then
	dnl AC_MSG_ERROR([You need to install an F# compiler])
	AC_MSG_ERROR([No $FS compiler found])
fi

if test "x$MONO" = "xno"; then
	AC_MSG_ERROR([No mono runtime found])
fi
AC_SUBST(FSC)

AC_OUTPUT([
Makefile
src/AssemblyInfo.cs
])

dnl Configuration summary
echo ""
echo "Configuration summary"
echo ""
echo "   * Installation prefix = $prefix"
echo "   * $FS compiler: $FSC"
echo ""

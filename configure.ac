# -*- coding: utf-8 -*-
dnl 	$Id: configure.ac,v 1.2.2.2.2.1 2018/03/08 06:22:08 doug Exp $	

# 
# Copyright © 2015, 2017, 2018 Douglas Lewan, d.lewan2000@gmail.com.
# All rights reserved.
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 

dnl Autoconf requirements
AC_INIT(cpio-mode, 0.02, d.lewan2000@gmail.com)
AC_PREFIX_DEFAULT(${HOME}/local)
dnl information on the package
dnl checks for programs

AC_PROG_INSTALL
AC_PROG_MKDIR_P

if tar --version >/dev/null 2>&1; then 
    tar_opts="-z -c"
    tar_suffix="tar.gz"
else
    tar_opts="-c"
    tar_suffix="tar"
fi
AC_SUBST(tar_opts)
AC_SUBST(tar_suffix)

HAVE_MAKEINFO=yes

if test "$MAKEINFO" = "no"; then
  MAKEINFO=makeinfo
  if test "x${with_makeinfo}" = "xno"; then
    HAVE_MAKEINFO=no
  elif test ! -e "$srcdir/info/emacs" && test ! -e "$srcdir/info/emacs.info"; then
    AC_MSG_ERROR( [You do not seem to have makeinfo >= 4.13, and your
source tree does not seem to have pre-built manuals in the 'info' directory.
Either install a suitable version of makeinfo, or re-run configure
with the '--without-makeinfo' option to build without the manuals.] )
  fi
fi
AC_SUBST(HAVE_MAKEINFO)

dnl checks for libraries
dnl checks for header files
dnl checks for types
dnl checks for structures
dnl checks for compiler characteristics
dnl checks for library functions
dnl checks for system services
AC_CONFIG_FILES([Makefile
		 stamp-h],
		 [echo timestamp > stamp-h])
AC_OUTPUT



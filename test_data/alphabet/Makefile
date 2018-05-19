# -*- mode: makefile; coding: utf-8 -*-
#	$Id: Makefile,v 1.4 2018/05/18 23:55:31 doug Exp $	

# Copyright © 2013, 2014, 2015, 2016, 2017, 2018
# Douglas Lewan, d.lewan2000@gmail.com.
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

PROJECT		=	cpio-mode
PROJECT_VERSION	=	0.01

################################
# 
# What we're building
# 
CFILES		=	
HFILES		=	
OFILES		=	${CFILES:.o=.c}


SHELLS		=	
EL_FILES	=	

################################
# 
# Where things are
# 
prefix		=	${HOME}/local
distdir		=	

# And where they go
bindir		=	${HOME}/local/bin
docdir		=	${HOME}/local/share/doc
exec_prefix	=	${prefix}
execdir		=	${exec_prefix}
libdir		=	${exec_prefix}/lib
libexecdir	=	${exec_prefix}/libexec
sbindir		=	${exec_prefix}/sbin

datarootdir	=	${prefix}/share
datadir		=	${datarootdir}
infodir		=	${datarootdir}/info
sysconfdir	=	${prefix}/etc
sharedstatedir	=	${prefix}/com
localstatdir	=	${prefix}/var
runstatedir	=	${localstatdir}/run

includedir	=	${prefix}/include
oldincludedir	=	/usr/include
docdir		=	${datarootdir}/doc/${PACKAGE_TARNAME}
infodir		=	${datarootdir}/info
htmldir		=	${docdir}
dvidir		=	${docdir}
pdfdir		=	${docdir}
psdir		=	${docdir}
libdir		=	${exec_prefix}/lib
lispdir		=	${prefix}/share/${PROJECT}/${PROJECT_VERSION}/lisp
localedir	=	${datarootdir}/locale
mandir		=	${datarootdir}/man
man1dir		=	${mandir}/man1
man2dir		=	${mandir}/man2
man3dir		=	${mandir}/man3
man4dir		=	${mandir}/man4
man5dir		=	${mandir}/man5
man6dir		=	${mandir}/man6
man7dir		=	${mandir}/man7
manext		=	.1
man1ext		=	.1
man2ext		=	.2
man3ext		=	.3
man4ext		=	.4
man5ext		=	.5
man6ext		=	.6
man7ext		=	.7
srcdir		=	@srcdir@
abs_srcdir	=	@abs_srcdir@

################################
# 
# Distribution information
# 
TAR_OPTS	=	${shell if tar --version >/dev/null 2>&1; then echo -z -c ; else echo -c ; fi}
TAR_SUFFIX	=	${shell if tar --version >/dev/null 2>&1; then echo tar.gz ; else echo tar ; fi}

TARBALL		=	${PROJECT}-${PROJECT_VERSION}-${shell date +%Y%m%d%H%M%S}.${TAR_SUFFIX}


################################
# 
# Programs we use
# 

INSTALL	=	true
CC	=	true
CFLAGS	=	


################################################################

################################
# 
# Building
# 

.PHONY: default help all alphabet nonsense check_gnu

default:	check_gnu
	@echo "This Makefile has no defaults."
	@echo "Please type one of the archive format targets:"
	@echo "    ${MAKE} all"
	@echo "    ${MAKE} bin"
	@echo "    ${MAKE} crc"
	@echo "    ${MAKE} hpbin"
	@echo "    ${MAKE} hpodc"
	@echo "    ${MAKE} newc"
	@echo "    ${MAKE} odc"
	@echo "    ${MAKE} tar"
	@echo "    ${MAKE} ustar"
	@echo "instead."

help:	check_gnu	default

all:	nonesense

nonsense:
	@echo "The target [[${@}]] does not make sense in this directory."

check_gnu:
	@${MAKE} --version >/dev/null 2>&1 || ( echo "This makefile requires GNU Make." && exit 1 )

.PHONY: alphabet a b c d e f g h i j k l m n o p q r s t u v w x y z

alphabet: check_gnu a b c d e f g h i j k l m n o p q r s t u v w x y z

small_alphabet: check_gnu a b c

a:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
b:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
c:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
d:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
e:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
f:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
g:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
h:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
i:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
j:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
k:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
l:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
m:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
n:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
o:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
p:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
		@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 				\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
q:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
r:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
s:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
t:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
u:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
v:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
w:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
x:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
y:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then 					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi
z:
	@for i in ${@} ${@}${@} ${@}${@}${@} ${@}${@}${@}${@} ${@}${@}${@}${@}${@} ; do	\
		>  $${i} ; 								\
		echo       >> $${i} ; 							\
		echo $${i} >> $${i} ; 							\
		echo       >> $${i} ; 							\
	done
	@if ! [ -d ${@}${@}${@}${@}${@}.d ] ; then					\
		mkdir ${@}${@}${@}${@}${@}.d ; 						\
	fi



################################
#
# Cpio formats
#
.PHONY: all bin crc hpbin hpodc newc odc tar ustar
all: bin crc hpbin hpodc newc odc tar ustar
# Yes, these regular expressions are a little brittle,
# but they work in this directory.
bin:	alphabet
	ls -1 -d ? ?? ??? ???? ????? ?????.d | grep -v CVS | cpio -o -H ${@} > alphabet.${@}.cpio
	ls -1 -d a aa aaa aaaa aaaaa aaaaa.d b bb bbb bbbb bbbbb bbbbb.d c cc ccc cccc ccccc ccccc.d | cpio -o -H ${@} > alphabet_small.${@}.cpio
	ls -1 -d a aa aaaaa.d b bb | cpio -o -H ${@} > alphabet_tiny.${@}.cpio
crc:	alphabet
	ls -1 -d ? ?? ??? ???? ????? ?????.d | grep -v CVS | cpio -o -H ${@} > alphabet.${@}.cpio
	ls -1 -d a aa aaa aaaa aaaaa aaaaa.d b bb bbb bbbb bbbbb bbbbb.d c cc ccc cccc ccccc ccccc.d | cpio -o -H ${@} > alphabet_small.${@}.cpio
	ls -1 -d a aa aaaaa.d b bb | cpio -o -H ${@} > alphabet_tiny.${@}.cpio
hpbin:	alphabet
	ls -1 -d ? ?? ??? ???? ????? ?????.d | grep -v CVS | cpio -o -H ${@} > alphabet.${@}.cpio
	ls -1 -d a aa aaa aaaa aaaaa aaaaa.d b bb bbb bbbb bbbbb bbbbb.d c cc ccc cccc ccccc ccccc.d | cpio -o -H ${@} > alphabet_small.${@}.cpio
	ls -1 -d a aa aaaaa.d b bb | cpio -o -H ${@} > alphabet_tiny.${@}.cpio
hpodc:	alphabet
	ls -1 -d ? ?? ??? ???? ????? ?????.d | grep -v CVS | cpio -o -H ${@} > alphabet.${@}.cpio
	ls -1 -d a aa aaa aaaa aaaaa aaaaa.d b bb bbb bbbb bbbbb bbbbb.d c cc ccc cccc ccccc ccccc.d | cpio -o -H ${@} > alphabet_small.${@}.cpio
	ls -1 -d a aa aaaaa.d b bb | cpio -o -H ${@} > alphabet_tiny.${@}.cpio
newc:	alphabet 
	ls -1 -d ? ?? ??? ???? ????? ?????.d | grep -v CVS | cpio -o -H ${@} > alphabet.${@}.cpio
	ls -1 -d a aa aaa aaaa aaaaa aaaaa.d b bb bbb bbbb bbbbb bbbbb.d c cc ccc cccc ccccc ccccc.d | cpio -o -H ${@} > alphabet_small.${@}.cpio
	ls -1 -d a aa aaaaa.d b bb | cpio -o -H ${@} > alphabet_tiny.${@}.cpio
odc:	alphabet
	ls -1 -d ? ?? ??? ???? ????? ?????.d | grep -v CVS | cpio -o -H ${@} > alphabet.${@}.cpio
	ls -1 -d a aa aaa aaaa aaaaa aaaaa.d b bb bbb bbbb bbbbb bbbbb.d c cc ccc cccc ccccc ccccc.d | cpio -o -H ${@} > alphabet_small.${@}.cpio
	ls -1 -d a aa aaaaa.d b bb | cpio -o -H ${@} > alphabet_tiny.${@}.cpio
tar:	alphabet
	ls -1 -d ? ?? ??? ???? ????? ?????.d | grep -v CVS | cpio -o -H ${@} > alphabet.${@}.cpio
	ls -1 -d a aa aaa aaaa aaaaa aaaaa.d b bb bbb bbbb bbbbb bbbbb.d c cc ccc cccc ccccc ccccc.d | cpio -o -H ${@} > alphabet_small.${@}.cpio
	ls -1 -d a aa aaaaa.d b bb | cpio -o -H ${@} > alphabet_tiny.${@}.cpio
ustar:	alphabet
	ls -1 -d ? ?? ??? ???? ????? ?????.d | grep -v CVS | cpio -o -H ${@} > alphabet.${@}.cpio
	ls -1 -d a aa aaa aaaa aaaaa aaaaa.d b bb bbb bbbb bbbbb bbbbb.d c cc ccc cccc ccccc ccccc.d | cpio -o -H ${@} > alphabet_small.${@}.cpio
	ls -1 -d a aa aaaaa.d b bb | cpio -o -H ${@} > alphabet_tiny.${@}.cpio

################################
# 
# Testing
# 

check: nonsense

installcheck: nonsense


################################
# 
# Installation
# 

install:	nonsense

installdirs:	nonsense

uninstall:	nonsense

install-html:	nonsense
install-dvi:	nonsense
install-pdf:	nonsense
install-ps:	nonsense
install-strip:	nonsense


################################
# 
# Hygiene
# 

clean:
	-rm *.cpio
	-for l in a b c d e f g h i j k l m n o p q r s t u v w x y z ; do				\
		rm $${l} $${l}$${l} $${l}$${l}$${l} $${l}$${l}$${l}$${l} $${l}$${l}$${l}$${l}$${l} ; 	\
	done

distclean: clean

mostlyclean: clean

maintainer-clean: clean


################################
# 
# Documentation
# 

dvi:	nonsense

html:	nonsense

pdf:	nonsense

ps:	nonsense

################################
# 
# Distribution
# 

dist:	nonsense

distdir:	nonsense

################################
# 
# Miscellany
# 

info:	nonsense

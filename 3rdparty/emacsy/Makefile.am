# Emacsy --- An embeddable Emacs-like library using  GNU Guile.
# Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
# Copyright (C) 2019 Jan Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of Emacsy.
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

EXTRA_DIST =					\
 COPYING.CC-BY-SA				\
 guix.scm

dist_emacsy_scm_DATA =				\
 emacsy/advice.scm				\
 emacsy/agenda.scm				\
 emacsy/block.scm				\
 emacsy/buffer.scm				\
 emacsy/command.scm				\
 emacsy/core.scm				\
 emacsy/coroutine.scm				\
 emacsy/cursor-list.scm				\
 emacsy/emacsy.scm				\
 emacsy/event.scm				\
 emacsy/help.scm				\
 emacsy/introspection.scm			\
 emacsy/job.scm					\
 emacsy/kbd-macro.scm				\
 emacsy/keymap.scm				\
 emacsy/klecl.scm				\
 emacsy/line-pragma.scm				\
 emacsy/minibuffer.scm				\
 emacsy/mode.scm				\
 emacsy/mru-stack.scm				\
 emacsy/self-doc.scm				\
 emacsy/text.scm				\
 emacsy/util.scm				\
 emacsy/vector-math.scm				\
 emacsy/window.scm				\
 emacsy/windows.scm

emacsy_scmdir = $(guilemoduledir)/emacsy
emacsy_godir = $(guileobjectdir)/emacsy
emacsy_go_DATA = $(dist_emacsy_scm_DATA:%.scm=%.go)

GUILEC_FLAGS = -Warity-mismatch -Wformat --load-path=$(abs_srcdir)

AM_V_GUILEC = $(AM_V_GUILEC_@AM_V@)
AM_V_GUILEC_ = $(AM_V_GUILEC_@AM_DEFAULT_V@)
AM_V_GUILEC_0 = @echo "  GUILEC     " $@;

.scm.go:
	$(AM_V_GUILEC)GUILE_AUTO_COMPILE=0			\
	LTDL_LIBRARY_PATH=$(abs_builddir)/.libs			\
	$(GUILD) compile --target="$(host)" $(GUILEC_FLAGS)	\
	-o "$@" "$<"

%.c.x: %.c
	$(AM_V_GEN) $(guile_snarf) -o "$@" $(GUILE_CFLAGS) "$<"


CLEANFILES =						\
 $(BUILT_SOURCES)					\
 $(emacsy_go_DATA)

lib_LTLIBRARIES = libemacsy.la
AM_CFLAGS = -I $(srcdir)/emacsy $(GUILE_CFLAGS)
libemacsy_la_LIBADD  = $(GUILE_LIBS)
dist_include_HEADERS = emacsy/emacsy.h
dist_libemacsy_la_SOURCES = emacsy/emacsy.c

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = emacsy/emacsy.pc

TESTS =						\
 test/advice.scm				\
 test/block.scm					\
 test/buffer.scm				\
 test/command.scm				\
 test/core.scm					\
 test/coroutine.scm				\
 test/emacsy.scm				\
 test/event.scm					\
 test/help.scm					\
 test/job.scm					\
 test/kbd-macro.scm				\
 test/keymap.scm				\
 test/klecl.scm					\
 test/minibuffer.scm				\
 test/mru-stack.scm				\
 test/self-doc.scm				\
 test/text.scm					\
 test/window.scm				\
 test/windows.scm

XFAIL_TESTS = test/windows.scm

EXTRA_DIST +=					\
 $(TESTS)					\
 test/check.scm\
 test/minibuffer-test-dir

TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = $(top_builddir)/pre-inst-env $(GUILE)

bin_PROGRAMS =

EMACSY_CFLAGS = -I $(srcdir)/emacsy
EMACSY_LDADD = libemacsy.la
BUILT_SOURCES =

hello_emacsy_SOURCES = example/hello-emacsy.c
emacsy_webkit_gtk_SOURCES = example/emacsy-webkit-gtk.c
emacsy_webkit_gtk_w_buffers_SOURCES = example/emacsy-webkit-gtk-w-buffers.c
emacsy_webkit_gtk_w_windows_SOURCES = example/emacsy-webkit-gtk-w-windows.c

etc_emacsydir = $(sysconfdir)/emacsy
etc_emacsy_DATA =				\
 example/hello-emacsy.scm			\
 example/emacsy-webkit-gtk.scm			\
 example/emacsy-webkit-gtk-w-buffers.scm	\
 example/emacsy-webkit-gtk-w-windows.scm

EXTRA_DIST += $(etc_emacsy_DATA)

if HELLO_EMACSY

bin_PROGRAMS += hello-emacsy

hello_emacsy_CFLAGS  = $(EMACSY_CFLAGS) $(GUILE_CFLAGS) $(FREEGLUT_CFLAGS) $(GL_CFLAGS) $(GLU_CFLAGS)
hello_emacsy_LDADD   = $(EMACSY_LDADD) $(GUILE_LIBS) $(FREEGLUT_LIBS) $(GL_LIBS) $(GLU_LIBS)
hello_emacsy_LDFLAGS = $(GUILE_LDFLAGS) $(FREEGLUT_LDFLAGS) $(GL_LDFLAGS) $(GLU_LDFLAGS)
#nodist_hello_emacsy_SOURCES = example/hello-emacsy.c.x
BUILT_SOURCES += example/hello-emacsy.c.x

endif

if EMACSY_WEBKIT_GTK

bin_PROGRAMS +=					\
 emacsy-webkit-gtk				\
 emacsy-webkit-gtk-w-buffers			\
 emacsy-webkit-gtk-w-windows

emacsy_webkit_gtk_CFLAGS  = $(EMACSY_CFLAGS) $(GUILE_CFLAGS) $(WEBKIT_CFLAGS)
emacsy_webkit_gtk_LDADD   = $(EMACSY_LDADD) $(GUILE_LIBS) $(WEBKIT_LIBS)
#nodist_emacsy_webkit_gtk_SOURCES = example/emacsy-webkit-gtk.c.x
BUILT_SOURCES += example/emacsy-webkit-gtk.c.x

emacsy_webkit_gtk_w_buffers_CFLAGS  = $(EMACSY_CFLAGS) $(EMACSY_CFLAGS) $(GUILE_CFLAGS) $(WEBKIT_CFLAGS)
emacsy_webkit_gtk_w_buffers_LDADD   = $(EMACSY_LDADD) $(GUILE_LIBS) $(WEBKIT_LIBS)
#nodist_emacsy_webkit_gtk_w_buffers_SOURCES = example/emacsy-webkit-gtk-w-buffers.c.x
BUILT_SOURCES += example/emacsy-webkit-gtk-w-buffers.c.x

emacsy_webkit_gtk_w_windows_CFLAGS  = $(EMACSY_CFLAGS) $(GUILE_CFLAGS) $(WEBKIT_CFLAGS)
emacsy_webkit_gtk_w_windows_LDADD   = $(EMACSY_LDADD) $(GUILE_LIBS) $(WEBKIT_LIBS)
#nodist_emacsy_webkit_gtk_w_windows_SOURCES = example/emacsy-webkit-gtk-w-windows.c.x
BUILT_SOURCES += example/emacsy-webkit-gtk-w-windows.c.x

endif

# Documentation
###############

TEXI2PDF_FLAGS = -I doc
info_TEXINFOS = doc/emacsy.texi

emacsy_TEXINFOS =				\
 doc/api.texi					\
 doc/hello-emacsy.texi				\
 doc/fdl-1.3.texi

emacsy_IMAGES =					\
 doc/images/README				\
 doc/images/child-window-diagram.graffle	\
 doc/images/child-window-diagram.pdf		\
 doc/images/emacsy-logo.pdf			\
 doc/images/emacsy-logo.png			\
 doc/images/minimal-emacsy-example.png		\
 doc/images/minimal-emacsy-figure.pdf		\
 doc/images/screenshot-small.png		\
 doc/images/screenshot.png			\
 doc/images/the-garden-1.png			\
 doc/images/the-garden-2.png			\
 doc/images/the-garden-3.png			\
 doc/images/the-garden-4.png			\
 doc/images/the-garden-5.png			\
 doc/images/the-garden-6.png			\
 doc/images/the-garden-7.png			\
 doc/images/the-garden-8.png			\
 doc/images/the-garden.pdf			\
 doc/images/the-garden.png			\
 doc/images/window-diagram.graffle		\
 doc/images/window-diagram.pdf

infoimagedir = $(infodir)/images
dist_infoimage_DATA =				\
  $(emacsy_IMAGES)

EXTRA_DIST +=					\
  $(emacsy_TEXINFOS)				\
  $(emacsy_IMAGES)

if HAVE_PDFLATEX
dist_doc_DATA = doc/emacsy.pdf
endif

DOC_SNARF = $(top_builddir)/pre-inst-env $(GUILD) doc-snarf
define DOC_SNARF_HEADER
@c Generated by doc-snarf, do not edit.

@c Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
@c Copyright (C) 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>

@c Permission is granted to copy, distribute and/or modify this document
@c under the terms of the GNU Free Documentation License, Version 1.3 or
@c any later version published by the Free Software Foundation; with no
@c Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.  A
@c copy of the license is included in the section entitled ``GNU Free
@c Documentation License''.

endef
export DOC_SNARF_HEADER

doc-snarf:
	@$(MAKE) --quiet doc-snarf-hello-emacsy > doc/hello-emacsy.texi
	@$(MAKE) --quiet doc-snarf-api > doc/api.texi

doc-snarf-hello-emacsy:
	@echo "$$DOC_SNARF_HEADER"
	@for i in				\
                example/hello-emacsy.c		\
	; do					\
	  $(GUILD) display-commentary $$i	\
             | tail +2				\
             | sed 's,^ ,,';			\
	  $(DOC_SNARF) -l c -t $$i;		\
	done
	@for i in				\
                example/hello-emacsy.scm	\
	; do					\
	  $(GUILD) display-commentary $$i	\
             | tail +2				\
             | sed 's,^ ,,';			\
	  $(DOC_SNARF) -l scheme -t $$i;	\
	done

doc-snarf-api:
	@echo "$$DOC_SNARF_HEADER"
	@for i in				\
                emacsy/emacsy.c			\
	; do					\
	  $(GUILD) display-commentary $$i	\
             | tail +2				\
             | sed 's,^ ,,';			\
	  $(DOC_SNARF) -l c -t $$i;		\
	done
	@for i in				\
                emacsy/emacsy.scm		\
                emacsy/event.scm		\
                emacsy/keymap.scm		\
                emacsy/command.scm		\
                emacsy/block.scm		\
                emacsy/klecl.scm		\
                emacsy/kbd-macro.scm		\
                emacsy/buffer.scm		\
                emacsy/mru-stack.scm		\
                emacsy/text.scm			\
                emacsy/minibuffer.scm		\
                emacsy/core.scm			\
                emacsy/advice.scm		\
                emacsy/window.scm		\
                emacsy/help.scm			\
                emacsy/self-doc.scm		\
	; do					\
	  $(GUILD) display-commentary $$i	\
             | tail +2				\
             | sed 's,^ ,,';			\
	  $(DOC_SNARF) -l scheme -t $$i;	\
	done

# Distribution
##############

# Reproducible tarball
GZIP_ENV = --no-name
am__tar = $${TAR-tar} --sort=name --mtime=@0 --owner=0 --group=0 --numeric-owner -cf - "$$tardir" dist

tag:
	git tag -s v$(PACKAGE_VERSION) -m "$(PACKAGE_NAME) $(PACKAGE_VERSION)."

dist-hook: gen-ChangeLog
	echo $(VERSION) > $(distdir)/.tarball-version
	git ls-tree -r --name-only HEAD > $(distdir)/.tarball-manifest

.PHONY: gen-ChangeLog
gen-ChangeLog $(distdir)/ChangeLog: config.status
	$(AM_V_GEN)if test -e .git; then		\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    > $(distdir)/cl-t;				\
	  rm -f $(distdir)/ChangeLog;			\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;	\
	fi

sign-dist: dist
	gpg -a --output $(distdir).tar.gz.sig --detach-sig $(distdir).tar.gz

verify-sig: $(distdir).tar.gz.sig $(distdir).tar.gz
	gpg --verify $(distdir).tar.gz.sig

GPG_KEY_ID = 1A858392E331EAFDB8C27FFBF3C1A0D9C1D65273
GIT_HOME = $(HOME)/src
gen-announce:
	$(GIT_HOME)/gnulib/build-aux/announce-gen\
	    --release-type=alpha\
	    --package-name=$(PACKAGE_TARNAME)\
	    --previous-version='0.1.1 '\
            --current-version=$(PACKAGE_VERSION)\
	    --gpg-key-id=$(GPG_KEY_ID)\
	    --url-directory=http://download.savannah.nongnu.org/releases/emacsy\
	    --news=NEWS\
            > doc/announce/ANNOUNCE-$(PACKAGE_VERSION)-

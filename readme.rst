iconv interface library for Objective-Caml
==========================================

What's this?
------------

Objective-Caml binding to the International Codeset Conversion Library.

Prerequisites
-------------

OCaml >= 4.08
 https://ocaml.org/
libiconv (Citrus version)
 https://wiki.freebsd.org/G%C3%A1borSoC2009
Or libiconv (GNU version) >= 1.11
 http://www.gnu.org/software/libiconv/
Or glibc >= 2.24
 https://www.gnu.org/software/libc/

How to make
-----------

Install
+++++++

::

 make install PREFIX=/usr/local

Specify your preferred directory to ``PREFIX``.
The libraries would be installed into ``$PREFIX/lib/ocaml`` (default is
``ocamlc -where``).

If iconv is not installed in the default search path, specify the directory
containing iconv to ``WITH_ICONV``.
``$WITH_ICONV/include`` and ``$WITH_ICONV/lib`` would be used.

Uninstall
+++++++++

::

 make uninstall PREFIX=/usr/local

Build examples
++++++++++++++

::

 make -C examples

License
-------

It is dual-licensed under the New BSD License and LGPL, see below.
Please apply LGPL when static linking libiconv.a of GNU version.

**license of iconv-ocaml (1)** ::

 Copyright 2010-2024 YT. All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 
 THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

**license of iconv-ocaml (2) and GNU libiconv** ::

 This file is part of iconv-ocaml.
 
 iconv-ocaml is free software: you can redistribute it and/or modify
 it under the terms of the GNU Library General Public License as published by
 the Free Software Foundation, either version 2 of the License, or
 (at your option) any later version.
 
 iconv-ocaml is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU Library General Public License for more details.
 
 You should have received a copy of the GNU Library General Public License
 along with iconv-ocaml.  If not, see <http://www.gnu.org/licenses/>.

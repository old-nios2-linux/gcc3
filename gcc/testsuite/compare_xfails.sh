#!/bin/sh

#
#   Copyright (C) 2003
#   by Jonah Graham (jgraham@altera.com).
#
#   This file is part of GCC.
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place - Suite 330,
#   Boston, MA 02111-1307, USA.  


set -u
#set -x



if [ "x$#" != "x2" ]
then
  echo "Hello. Welcome to the NIOS2 XFAIL regression lister"
  echo "Invocation is $0 xfail.lst gcc.sum"
fi

cat $1 | grep -e'^FAIL:' | sort -i | uniq -i > $TEMP/xfail_$$__
cat $2 | grep -e'^FAIL:' | sort -i | uniq -i > $TEMP/possible_xfail_$$__

diff -uiwbBd $TEMP/xfail_$$__ $TEMP/possible_xfail_$$__  \
  | grep -e'^+FAIL:' | sort -i | sed -es@^+@@

rm -f $TEMP/xfail_$$__ $TEMP/possible_xfail_$$__

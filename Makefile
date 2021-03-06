# hyc - Hydrogen Compiler written in C
# Copyright (C) 2021  Kacper Kocot <kocotian@kocotian.pl>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published hyc
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

include config.mk

SRC = hyc.c util.c err.c str.c tokenize.c ast.c target/${TARGET}.c
OBJ = ${SRC:.c=.o}

all: hyc

%.o: %.c include/%.h
	${CC} ${CFLAGS} -c -o $@ $<

hyc: ${OBJ}
	${CC} ${CFLAGS} -o $@ $^

install: hyc
	mkdir -p ${DESTDIR}${PREFIX}/bin
	install -Dm755 hyc ${DESTDIR}${PREFIX}/bin/hyc-core
	install -Dm755 hyc-wrapper ${DESTDIR}${PREFIX}/bin/hyc

clean:
	rm -f ${OBJ} hyc

.PHONY: all clean

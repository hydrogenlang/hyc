/*
   hyc - Hydrogen Compiler written in C
   Copyright (C) 2021  Kacper Kocot <kocotian@kocotian.pl>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

*/

#ifndef _STR_H
#define _STR_H

#include <sys/types.h>

typedef struct String {
	char *data;
	size_t len;
} String;

int Strcmp(String s1, String s2);
int Strccmp(const String s, const char *cs);

#define Strevalf(S) (int)((S).len), (S).data

typedef struct Array {
	void *data;
	size_t len;
} Array;

#define Array(T) struct { T *data; size_t len; }

/* Vector - dynamic array */
#define newVector(ARR) ((ARR).data = malloc((ARR).len = 0))
#define pushVector(ARR, VAL) (((ARR).data = \
			realloc((ARR).data, \
				++((ARR).len) * (sizeof *((ARR).data)))), \
		(ARR).data[(ARR).len - 1] = (VAL))

#endif

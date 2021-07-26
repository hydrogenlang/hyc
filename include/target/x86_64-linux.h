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

#ifndef _TARGET_H
#define _TARGET_H

#include <ast.h>
#include <stdlib.h>

typedef struct {
	String text, data;
} Compiler;

static inline Compiler newCompiler(void)
{
	return (Compiler) {
		{ malloc(0), 0 },
		{ malloc(0), 0 },
	};
}

Compiler compileModule(ASTModule module);

#endif

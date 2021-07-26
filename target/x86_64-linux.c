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

#include <target/x86_64-linux.h>
#include <stdarg.h>

static Compiler newCompiler(void)
{
	return (Compiler) {
		{ malloc(0), 0 },
		{ malloc(0), 0 },
	};
}

static size_t
asmAppend(int data, Compiler *compiler, char *fmt, ...)
{
	char buf[8192];
	int wb;
	va_list ap;
	String *a = (data ? &compiler->data : &compiler->text);

	va_start(ap, fmt);
	wb = vsnprintf(buf, sizeof buf - 1, fmt, ap);
	va_end(ap);
	buf[wb++] = '\n'; buf[wb] = '\0';
	a->data = realloc(a->data, a->len + wb + 1);
	strcpy(a->data + a->len, buf);
	a->len += wb;
}

#define asmTextAppend(...) asmAppend(0, __VA_ARGS__)
#define asmDataAppend(...) asmAppend(1, __VA_ARGS__)

static void
compileStatement(Compiler *compiler, union ASTStatement *statement)
{
}

static void
compileGlobalFunction(Compiler *compiler, ASTGlobalFunction func)
{
	/*
	**	<function name>:
	**		<function body>
	**		ret
	*/
	asmTextAppend(compiler, "%s:", func.name.value);
	compileStatement(compiler, func.body);
	asmTextAppend(compiler, "\tret");
}

Compiler
compileModule(ASTModule module)
{
	Compiler compiler = newCompiler();
	size_t i;
	ASTGlobal *glob;

	for (i = 0; i < module.len; ++i) {
		if ((glob = &module.data[i])->type == ASTGlobalFunction_T)
			compileGlobalFunction(&compiler, module.data[i].Function);
	}

	return compiler;
}

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
#include <string.h>

static size_t asmAppend(int data, Compiler *compiler, char *fmt, ...);

#define asmTextAppend(...) asmAppend(0, __VA_ARGS__)
#define asmDataAppend(...) asmAppend(1, __VA_ARGS__)

static void compileExpressionLiteralIdentifier(Compiler *compiler, ASTExpressionLiteral expr);
static void compileExpressionLiteralInteger(Compiler *compiler, ASTExpressionLiteral expr);
static void compileExpressionLiteralString(Compiler *compiler, ASTExpressionLiteral expr);
static void compileExpressionUnaryNegation(Compiler *compiler, ASTExpressionUnary expr);
static void compileExpressionUnarySignChange(Compiler *compiler, ASTExpressionUnary expr);
static void compileExpressionUnaryAddressof(Compiler *compiler, ASTExpressionUnary expr);
static void compileExpressionUnaryValuefrom(Compiler *compiler, ASTExpressionUnary expr);
static void compileExpressionUnaryPreincrement(Compiler *compiler, ASTExpressionUnary expr);
static void compileExpressionUnaryPredecrement(Compiler *compiler, ASTExpressionUnary expr);
static void compileExpression(Compiler *compiler, union ASTExpression *expression);

static void compileStatementCompound(Compiler *compiler, ASTStatementCompound stat);
static void compileStatementConditional(Compiler *compiler, ASTStatementConditional stat);
static void compileStatementReturn(Compiler *compiler, ASTStatementReturn stat);
static void compileStatementExpression(Compiler *compiler, ASTStatementExpression stat);
static void compileStatement(Compiler *compiler, union ASTStatement *statement);

static void compileGlobalFunction(Compiler *compiler, ASTGlobalFunction func);

///////////////////////////////////////////////////////////////////////////////

static Array(ASTExpressionLiteral) literalStrings;

///////////////////////////////////////////////////////////////////////////////

static size_t
asmAppend(int data, Compiler *compiler, char *fmt, ...)
{
	char buf[8192];
	size_t wb;
	va_list ap;
	String *a = (data ? &compiler->data : &compiler->text);

	va_start(ap, fmt);
	wb = (unsigned)vsnprintf(buf, sizeof buf - 1, fmt, ap);
	va_end(ap);
	buf[wb++] = '\n'; buf[wb] = '\0';
	a->data = realloc(a->data, a->len + wb + 1);
	strcpy(a->data + a->len, buf);
	a->len += wb;
}

/* Expressions ***************************************************************/

static void
compileExpressionLiteralIdentifier(Compiler *compiler, ASTExpressionLiteral expr)
{
}

static void
compileExpressionLiteralInteger(Compiler *compiler, ASTExpressionLiteral expr)
{
	asmTextAppend(compiler, "\tmov r15, %s", expr.value);
}

static void
compileExpressionLiteralString(Compiler *compiler, ASTExpressionLiteral expr)
{
	pushVector(literalStrings, expr);
	asmDataAppend(compiler, "\t.STR%d: db \"%s\"",
			literalStrings.len - 1, expr.value);
	asmTextAppend(compiler, "\tmov r15, .STR%d", literalStrings.len - 1);
}

static void
compileExpressionUnaryNegation(Compiler *compiler, ASTExpressionUnary expr)
{
}

static void
compileExpressionUnarySignChange(Compiler *compiler, ASTExpressionUnary expr)
{
	compileExpression(compiler, expr.expr);
	asmTextAppend("\tneg r15");
}

static void
compileExpressionUnaryAddressof(Compiler *compiler, ASTExpressionUnary expr)
{
}

static void
compileExpressionUnaryValuefrom(Compiler *compiler, ASTExpressionUnary expr)
{
}

static void
compileExpressionUnaryPreincrement(Compiler *compiler, ASTExpressionUnary expr)
{
}

static void
compileExpressionUnaryPredecrement(Compiler *compiler, ASTExpressionUnary expr)
{
}

static void
compileExpression(Compiler *compiler, union ASTExpression *expression)
{
	switch (expression->type) {
	case ASTExpressionLiteralIdentifier_T:
		compileExpressionLiteralIdentifier(compiler, expression->Literal);
		break;
	case ASTExpressionLiteralInteger_T:
		compileExpressionLiteralInteger(compiler, expression->Literal);
		break;
	case ASTExpressionLiteralString_T:
		compileExpressionLiteralString(compiler, expression->Literal);
		break;
	case ASTExpressionUnaryNegation_T:
		compileExpressionUnaryNegation(compiler, expression->Unary);
		break;
	case ASTExpressionUnarySignChange_T:
		compileExpressionUnarySignChange(compiler, expression->Unary);
		break;
	case ASTExpressionUnaryAddressof_T:
		compileExpressionUnaryAddressof(compiler, expression->Unary);
		break;
	case ASTExpressionUnaryValuefrom_T:
		compileExpressionUnaryValuefrom(compiler, expression->Unary);
		break;
	case ASTExpressionUnaryPreincrement_T:
		compileExpressionUnaryPreincrement(compiler, expression->Unary);
		break;
	case ASTExpressionUnaryPredecrement_T:
		compileExpressionUnaryPredecrement(compiler, expression->Unary);
		break;
	default: break;
	}
}

/* Statements ****************************************************************/

static void
compileStatementCompound(Compiler *compiler, ASTStatementCompound stat)
{
	size_t i;
	for (i = 0; i < stat.len; ++i)
		compileStatement(compiler, stat.data + i);
}

static void
compileStatementConditional(Compiler *compiler, ASTStatementConditional stat)
{
	/* TODO! */
}

static void
compileStatementReturn(Compiler *compiler, ASTStatementReturn stat)
{
	compileExpression(compiler, stat.expr);
	asmTextAppend(compiler, "\tmov rax, rdx");
	asmTextAppend(compiler, "\tret");
}

static void
compileStatementExpression(Compiler *compiler, ASTStatementExpression stat)
{
	compileExpression(compiler, stat.expr);
}

static void
compileStatement(Compiler *compiler, union ASTStatement *statement)
{
	switch (statement->type) {
	case ASTStatementNoOp_T:
		break;
	case ASTStatementCompound_T:
		compileStatementCompound(compiler, statement->Compound);
		break;
	case ASTStatementConditional_T:
		compileStatementConditional(compiler, statement->Conditional);
		break;
	case ASTStatementReturn_T:
		compileStatementReturn(compiler, statement->Return);
		break;
	case ASTStatementExpression_T:
		compileStatementExpression(compiler, statement->Expression);
		break;
	default: break;
	}
}

/* Globals *******************************************************************/

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

/* Module ********************************************************************/

Compiler
compileModule(ASTModule module)
{
	Compiler compiler = newCompiler();
	size_t i;
	ASTGlobal *glob;

	/* initialization of global containers */
	newVector(literalStrings);

	for (i = 0; i < module.len; ++i) {
		if ((glob = &module.data[i])->type == ASTGlobalFunction_T)
			compileGlobalFunction(&compiler, module.data[i].Function);
	}

	return compiler;
}

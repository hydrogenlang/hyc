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
#include <stdio.h>
#include <string.h>

static size_t asmAppend(String *s, char *fmt, ...);

#define asmTextAppend(COMPILER, ...) asmAppend(&((COMPILER)->text), __VA_ARGS__)
#define asmDataAppend(COMPILER, ...) asmAppend(&((COMPILER)->data), __VA_ARGS__)

static void compileExpressionLiteralIdentifier(Compiler *compiler, ASTExpressionLiteral expr);
static void compileExpressionLiteralInteger(Compiler *compiler, ASTExpressionLiteral expr);
static void compileExpressionLiteralString(Compiler *compiler, ASTExpressionLiteral expr);
static void compileExpressionUnaryNegation(Compiler *compiler, ASTExpressionUnary expr);
static void compileExpressionUnarySignChange(Compiler *compiler, ASTExpressionUnary expr);
static void compileExpressionUnaryAddressof(Compiler *compiler, ASTExpressionUnary expr);
static void compileExpressionUnaryValuefrom(Compiler *compiler, ASTExpressionUnary expr);
static void compileExpressionUnaryPreincrement(Compiler *compiler, ASTExpressionUnary expr);
static void compileExpressionUnaryPredecrement(Compiler *compiler, ASTExpressionUnary expr);
static void compileExpressionFunctionCall(Compiler *compiler, ASTExpressionFunctionCall expr);
static void compileExpression(Compiler *compiler, union ASTExpression *expression);

static void compileStatementCompound(Compiler *compiler, ASTStatementCompound stat);
static void compileStatementConditional(Compiler *compiler, ASTStatementConditional stat);
static void compileStatementReturn(Compiler *compiler, ASTStatementReturn stat);
static void compileStatementExpression(Compiler *compiler, ASTStatementExpression stat);
static void compileStatementInlineAssembly(Compiler *compiler, ASTStatementInlineAssembly stat);
static void compileStatement(Compiler *compiler, union ASTStatement *statement);

static void compileGlobalFunction(Compiler *compiler, ASTGlobalFunction func);

///////////////////////////////////////////////////////////////////////////////

enum LiteralIdentifierType {
	LINULL,
	LIFunction, LIVariable,
	LICount
};

struct LiteralIdentifier {
	enum LiteralIdentifierType type;
	ASTExpressionLiteral *expr;
};

static struct LiteralIdentifier
newLiteralIdentifier_p(enum LiteralIdentifierType type, ASTExpressionLiteral *expr)
{
	return (struct LiteralIdentifier){ type, expr };
}

struct LiteralIdentifierTree {
	Array(struct LiteralIdentifier) identifiers;
};

static struct LiteralIdentifierTree
newLiteralIdentifierTree(void)
{
	struct LiteralIdentifierTree ret;
	newVector(ret.identifiers);
	return ret;
}

static Array(struct LiteralIdentifierTree) globalLiteralIdentifierTree;
static Array(ASTExpressionLiteral *) literalStrings;

///////////////////////////////////////////////////////////////////////////////

static size_t
asmAppend(String *s, char *fmt, ...)
{
	char buf[8192];
	size_t wb;
	va_list ap;

	va_start(ap, fmt);
	wb = (unsigned)vsnprintf(buf, sizeof buf - 1, fmt, ap);
	va_end(ap);
	buf[wb++] = '\n'; buf[wb] = '\0';
	s->data = realloc(s->data, s->len + wb + 1);
	strcpy(s->data + s->len, buf);
	s->len += wb;
	return wb;
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
	pushVector(literalStrings, &expr);
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
	asmTextAppend(compiler, "\tneg r15");
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
compileExpressionFunctionCall(Compiler *compiler, ASTExpressionFunctionCall expr)
{
	compileExpression(compiler, expr.callexpr);
	asmTextAppend(compiler, "\tcall r15\n\tmov r15, rax");
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
	case ASTExpressionFunctionCall_T:
		compileExpressionFunctionCall(compiler, expression->FunctionCall);
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
	asmTextAppend(compiler, "\tmov rax, r15");
	asmTextAppend(compiler, "\tret");
}

static void
compileStatementExpression(Compiler *compiler, ASTStatementExpression stat)
{
	compileExpression(compiler, stat.expr);
}

static void
compileStatementInlineAssembly(Compiler *compiler, ASTStatementInlineAssembly stat)
{
	asmTextAppend(compiler, "\t%s", stat.expr.value);
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
	case ASTStatementInlineAssembly_T:
		compileStatementInlineAssembly(compiler, statement->InlineAssembly);
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

	pushVector(lastArray(globalLiteralIdentifierTree).identifiers,
			newLiteralIdentifier_p(LIFunction, &(func.name)));
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
	newVector(globalLiteralIdentifierTree);
	newVector(literalStrings);

	pushVector(globalLiteralIdentifierTree, newLiteralIdentifierTree());

	for (i = 0; i < module.len; ++i) {
		if ((glob = &module.data[i])->type == ASTGlobalFunction_T)
			compileGlobalFunction(&compiler, module.data[i].Function);
	}

	return compiler;
}

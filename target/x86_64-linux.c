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
#include <util.h>

static struct LiteralIdentifier *identifierCheck(ASTExpressionLiteral expr);
static size_t asmAppend(String *s, char *fmt, ...);

#define asmTextAppend(COMPILER, ...) asmAppend(&((COMPILER)->text), __VA_ARGS__)
#define asmDataAppend(COMPILER, ...) asmAppend(&((COMPILER)->data), __VA_ARGS__)

static void compileExpressionLiteralIdentifier(Compiler *compiler, ASTExpressionLiteral expr, int lvalue);
static void compileExpressionLiteralInteger(Compiler *compiler, ASTExpressionLiteral expr, int lvalue);
static void compileExpressionLiteralString(Compiler *compiler, ASTExpressionLiteral expr, int lvalue);
static void compileExpressionUnarySignChange(Compiler *compiler, ASTExpressionUnary expr, int lvalue);
static void compileExpressionUnaryAddressof(Compiler *compiler, ASTExpressionUnary expr, int lvalue);
static void compileExpressionUnaryValuefrom(Compiler *compiler, ASTExpressionUnary expr, int lvalue);
static void compileExpressionUnaryPreincrement(Compiler *compiler, ASTExpressionUnary expr, int lvalue);
static void compileExpressionUnaryPredecrement(Compiler *compiler, ASTExpressionUnary expr, int lvalue);
static void compileExpressionUnaryLogicalNot(Compiler *compiler, ASTExpressionUnary expr, int lvalue);
static void compileExpressionFunctionCall(Compiler *compiler, ASTExpressionFunctionCall expr, int lvalue);
static void compileExpression(Compiler *compiler, union ASTExpression *expression, int lvalue);

static void compileStatementCompound(Compiler *compiler, ASTStatementCompound stat);
static void compileStatementConditional(Compiler *compiler, ASTStatementConditional stat);
static void compileStatementReturn(Compiler *compiler, ASTStatementReturn stat);
static void compileStatementExpression(Compiler *compiler, ASTStatementExpression stat);
static void compileStatementInlineAssembly(Compiler *compiler, ASTStatementInlineAssembly stat);
static void compileStatementVariableDeclaration(Compiler *compiler, ASTStatementVariableDeclaration stat);
static void compileStatement(Compiler *compiler, union ASTStatement *statement);

static void compileGlobalFunction(Compiler *compiler, ASTGlobalFunction func);

///////////////////////////////////////////////////////////////////////////////

static char *
operation_size(size_t size)
{
	return
		size == 1 ? "BYTE"  :
		size == 2 ? "WORD"  :
		size == 4 ? "DWORD" :
		size == 8 ? "QWORD" : "";
}

///////////////////////////////////////////////////////////////////////////////

enum LiteralIdentifierType {
	LINULL,
	LIFunction,
	LIVariableOnStack,
	LICount
};

static size_t *current_rbp_offset(void);

struct VariableOnStack {
	enum LiteralIdentifierType type;
	size_t rbp_offset;
	size_t size;
	ASTStatementVariableDeclaration decl;
};
static struct VariableOnStack
newVariableOnStack(size_t size, ASTStatementVariableDeclaration decl)
{
	struct VariableOnStack ret;
	ret.type = LIVariableOnStack;
	ret.rbp_offset = *(current_rbp_offset()) += size;
	ret.size = size;
	ret.decl = decl;
	return ret;
}

struct Function {
	enum LiteralIdentifierType type;
	ASTGlobalFunction decl;
};
static struct Function
newFunction(ASTGlobalFunction decl)
{
	struct Function ret;
	ret.type = LIFunction;
	ret.decl = decl;
	return ret;
}

union LiteralIdentifierData {
	enum LiteralIdentifierType type;
	struct VariableOnStack VariableOnStack;
	struct Function Function;
};

struct LiteralIdentifier {
	union LiteralIdentifierData data;
	ASTExpressionLiteral iden;
};
static struct LiteralIdentifier
newLiteralIdentifierFunction(ASTExpressionLiteral expr, ASTGlobalFunction decl)
{
	struct LiteralIdentifier ret;
	ret.data.type = LIFunction;
	ret.data.Function = newFunction(decl);
	ret.iden = expr;
	return ret;
}
static struct LiteralIdentifier
newLiteralIdentifierVariableOnStack(ASTExpressionLiteral expr, ASTStatementVariableDeclaration decl)
{
	struct LiteralIdentifier ret;
	ret.data.type = LIVariableOnStack;
	ret.data.VariableOnStack = newVariableOnStack(8, decl);
	ret.iden = expr;
	return ret;
}

struct LiteralIdentifierTree {
	size_t rbp_off;
	Array(struct LiteralIdentifier) identifiers;
	Array(struct VariableOnStack) stack;
};
static struct LiteralIdentifierTree
newLiteralIdentifierTree(void)
{
	struct LiteralIdentifierTree ret;
	ret.rbp_off = 0;
	newVector(ret.identifiers);
	newVector(ret.stack);
	return ret;
}

static Array(struct LiteralIdentifierTree) globalLiteralIdentifierTree;
static Array(ASTExpressionLiteral *) literalStrings;
static int ifcnt;

static struct LiteralIdentifier *
identifierCheck(ASTExpressionLiteral expr)
{
	ssize_t i, j;
	for (i = (signed)(globalLiteralIdentifierTree.len) - 1; i >= 0; ++i) {
		for (j = 0; j < (signed)(globalLiteralIdentifierTree.data[i].identifiers.len); ++j) {
			if (!strcmp(expr.value, globalLiteralIdentifierTree.data[i].identifiers.data[j].iden.value)) {
				return globalLiteralIdentifierTree.data[i].identifiers.data + j;
			}
		}
	}
	return NULL;
}

static size_t *
current_rbp_offset(void)
{
	return &(lastArray(globalLiteralIdentifierTree).rbp_off);
}

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
compileExpressionLiteralIdentifier(Compiler *compiler, ASTExpressionLiteral expr, int lvalue)
{
	struct LiteralIdentifier *iden;
	if ((iden = identifierCheck(expr)) == NULL)
		error(expr.any.any.inittoken, "'%s' was not declared", expr.value);
	if (iden->data.type == LIFunction) {
		asmTextAppend(compiler, "\t%s r15, %s", lvalue ? "lea" : "mov", expr.value);
	} else if (iden->data.type == LIVariableOnStack) {
		asmTextAppend(compiler, "\t%s %s r15, [rbp-%d]", lvalue ? "lea" : "mov",
				operation_size(iden->data.VariableOnStack.size),
				iden->data.VariableOnStack.rbp_offset);
	}
}

static void
compileExpressionLiteralInteger(Compiler *compiler, ASTExpressionLiteral expr, int lvalue)
{
	if (lvalue) error(expr.any.any.inittoken, "lvalue required (Integer literal is not lvalue)");
	asmTextAppend(compiler, "\tmov r15, %s", expr.value);
}

static void
compileExpressionLiteralString(Compiler *compiler, ASTExpressionLiteral expr, int lvalue)
{
	if (lvalue) error(expr.any.any.inittoken, "lvalue required (String literal is not lvalue)");
	pushVector(literalStrings, &expr);
	asmDataAppend(compiler, "\tSTR.%d: db \"%s\"",
			literalStrings.len - 1, expr.value);
	asmTextAppend(compiler, "\tmov r15, STR.%d", literalStrings.len - 1);
}

static void
compileExpressionUnarySignChange(Compiler *compiler, ASTExpressionUnary expr, int lvalue)
{
	if (lvalue) error(expr.any.any.inittoken, "lvalue required (Sign change expression is not lvalue)");
	compileExpression(compiler, expr.expr, 0);
	asmTextAppend(compiler, "\tneg r15");
}

static void
compileExpressionUnaryAddressof(Compiler *compiler, ASTExpressionUnary expr, int lvalue)
{
	if (lvalue) error(expr.any.any.inittoken, "lvalue required (Address getting expression is not lvalue)");
	switch (expr.expr->type) {
	case ASTExpressionLiteralIdentifier_T:
		compileExpressionLiteralIdentifier(compiler, expr.expr->Literal, 1);
		break;
	default:
		error(expr.any.any.inittoken, "cannot get address of expression other than Identifier literal"); /* TODO */
		break;
	}
}

static void
compileExpressionUnaryValuefrom(Compiler *compiler, ASTExpressionUnary expr, int lvalue)
{
	compileExpression(compiler, expr.expr, 0);
	asmTextAppend(compiler, "\t%s r15, [r15]", lvalue ? "lea" : "mov");
}

static void
compileExpressionUnaryPreincrement(Compiler *compiler, ASTExpressionUnary expr, int lvalue)
{
	if (lvalue) error(expr.any.any.inittoken, "lvalue required (Incrementation operation will not give lvalue)");
	compileExpression(compiler, expr.expr, 1);
	asmTextAppend(compiler, "\tadd QWORD [r15], 1");
}

static void
compileExpressionUnaryPredecrement(Compiler *compiler, ASTExpressionUnary expr, int lvalue)
{
	if (lvalue) error(expr.any.any.inittoken, "lvalue required (Decrementation operation will not give lvalue)");
	compileExpression(compiler, expr.expr, 1);
	asmTextAppend(compiler, "\tsub QWORD [r15], 1");
}

static void
compileExpressionUnaryLogicalNot(Compiler *compiler, ASTExpressionUnary expr, int lvalue)
{
	if (lvalue) error(expr.any.any.inittoken, "lvalue required (Logical negation will not give lvalue)");
	compileExpression(compiler, expr.expr, 0);
	asmTextAppend(compiler, "\tcmp r15, 0");
	asmTextAppend(compiler, "\tsete al");
	asmTextAppend(compiler, "\tmovzx r15, al");
}

static char *
argumentRegister(size_t argno, char *buf, size_t len)
{
	if (argno < 6) {
		buf[0] = 'r';
		buf[1] =
			argno == 0 ? 'd' :
			argno == 1 ? 's' :
			argno == 2 ? 'd' :
			argno == 3 ? 'c' :
			argno == 4 || argno == 5 ? '4' + argno :
			'1';
		buf[2] =
			argno == 0 || argno == 1 ? 'i' :
			argno == 2 || argno == 3 ? 'x' :
			argno == 4 || argno == 5 ? 'd' :
			'5';
		buf[3] = '\0';
	}
	return buf;
}

static void
compileExpressionFunctionArgumentList(Compiler *compiler, ASTExpressionFunctionArgumentList expr, int lvalue)
{
	size_t i;
	char data[32];
	if (lvalue) error(expr.any.any.inittoken, "lvalue required (Function Argument List is not lvalue)");
	for (i = 0; i < expr.len; ++i) {
		compileExpression(compiler, expr.data + i, 0);
		asmTextAppend(compiler, "\tmov %s, r15", argumentRegister(i, data, sizeof data));
	}
}

static void
compileExpressionFunctionCall(Compiler *compiler, ASTExpressionFunctionCall expr, int lvalue)
{
	if (lvalue) error(expr.any.any.inittoken, "lvalue required (Function Call is not lvalue)");
	compileExpression(compiler, expr.argv, 0);
	compileExpression(compiler, expr.callexpr, 0);
	asmTextAppend(compiler, "\tcall r15\n\tmov r15, rax");
}

static void
compileExpressionBinaryAssignment(Compiler *compiler, ASTExpressionBinary expr, int lvalue)
{
	compileExpression(compiler, expr.right, 0);
	asmTextAppend(compiler, "\tmov rax, r15");
	compileExpression(compiler, expr.left, 1);
	asmTextAppend(compiler, "\tmov [r15], rax");
	if (!lvalue)
		asmTextAppend(compiler, "\tmov r15, [r15]");
}

static void
compileExpression(Compiler *compiler, union ASTExpression *expression, int lvalue)
{
	switch (expression->type) {
	case ASTExpressionLiteralIdentifier_T:
		compileExpressionLiteralIdentifier(compiler, expression->Literal, lvalue);
		break;
	case ASTExpressionLiteralInteger_T:
		compileExpressionLiteralInteger(compiler, expression->Literal, lvalue);
		break;
	case ASTExpressionLiteralString_T:
		compileExpressionLiteralString(compiler, expression->Literal, lvalue);
		break;
	case ASTExpressionUnarySignChange_T:
		compileExpressionUnarySignChange(compiler, expression->Unary, lvalue);
		break;
	case ASTExpressionUnaryAddressof_T:
		compileExpressionUnaryAddressof(compiler, expression->Unary, lvalue);
		break;
	case ASTExpressionUnaryValuefrom_T:
		compileExpressionUnaryValuefrom(compiler, expression->Unary, lvalue);
		break;
	case ASTExpressionUnaryPreincrement_T:
		compileExpressionUnaryPreincrement(compiler, expression->Unary, lvalue);
		break;
	case ASTExpressionUnaryPredecrement_T:
		compileExpressionUnaryPredecrement(compiler, expression->Unary, lvalue);
		break;
	case ASTExpressionUnaryLogicalNot_T:
		compileExpressionUnaryLogicalNot(compiler, expression->Unary, lvalue);
		break;
	case ASTExpressionFunctionArgumentList_T:
		compileExpressionFunctionArgumentList(compiler, expression->FunctionArgumentList, lvalue);
		break;
	case ASTExpressionFunctionCall_T:
		compileExpressionFunctionCall(compiler, expression->FunctionCall, lvalue);
		break;
	case ASTExpressionBinaryAssignment_T:
		compileExpressionBinaryAssignment(compiler, expression->Binary, lvalue);
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
	int iifcnt = ifcnt;
	compileExpression(compiler, stat.condition, 0);
	asmTextAppend(compiler, "\tcmp r15, 0");
	if (stat.elsebody == NULL) {
		++ifcnt;
		asmTextAppend(compiler, "\tje .IF.%d", iifcnt);
		compileStatement(compiler, stat.body);
		asmTextAppend(compiler, ".IF.%d:", iifcnt);
	} else {
		ifcnt += 2;
		asmTextAppend(compiler, "\tje .IF.%d", iifcnt);
		compileStatement(compiler, stat.body);
		asmTextAppend(compiler, "\tjmp .IF.%d", iifcnt + 1);
		asmTextAppend(compiler, ".IF.%d:", iifcnt);
		compileStatement(compiler, stat.elsebody);
		asmTextAppend(compiler, ".IF.%d:", iifcnt + 1);
	}
}

static void
compileStatementReturn(Compiler *compiler, ASTStatementReturn stat)
{
	compileExpression(compiler, stat.expr, 0);
	asmTextAppend(compiler, "\tmov rax, r15");
	asmTextAppend(compiler, "\tret");
}

static void
compileStatementExpression(Compiler *compiler, ASTStatementExpression stat)
{
	compileExpression(compiler, stat.expr, 0);
}

static void
compileStatementInlineAssembly(Compiler *compiler, ASTStatementInlineAssembly stat)
{
	asmTextAppend(compiler, "\t%s", stat.expr.value);
}

static void
compileStatementVariableDeclaration(Compiler *compiler, ASTStatementVariableDeclaration stat)
{
	asmTextAppend(compiler, "\tsub rsp, 8");
	pushVector(lastArray(globalLiteralIdentifierTree).identifiers,
			newLiteralIdentifierVariableOnStack(stat.name, stat));
	(void)(stat.name.value);
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
	case ASTStatementVariableDeclaration_T:
		compileStatementVariableDeclaration(compiler, statement->VariableDeclaration);
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
			newLiteralIdentifierFunction(func.name, func));
	/* TODO: parameters */
	asmTextAppend(compiler, "%s:", func.name.value);
	asmTextAppend(compiler, "\tpush rbp");
	asmTextAppend(compiler, "\tmov rbp, rsp");
	compileStatement(compiler, func.body);
	asmTextAppend(compiler, "\tleave");
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
	ifcnt = 0;

	pushVector(globalLiteralIdentifierTree, newLiteralIdentifierTree());

	for (i = 0; i < module.len; ++i) {
		if ((glob = &module.data[i])->type == ASTGlobalFunction_T)
			compileGlobalFunction(&compiler, module.data[i].Function);
	}

	return compiler;
}

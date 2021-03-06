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

#include <ast.h>
#include <err.h>
#include <stdlib.h>
#include <util.h>

typedef struct {
	Token *data;
	size_t len;
	ssize_t pos;
} Tokenizer;

#define new(PTR) (*((PTR) = malloc(sizeof *(PTR))))

static Token *prevToken(Tokenizer *t);

static Token *nextToken(Tokenizer *t);
static Token *nextTokenType(Tokenizer *t, TokenType type);
static Token *enextToken(Tokenizer *t);
static Token *enextTokenType(Tokenizer *t, TokenType type);

static ASTExpression tokenstoASTExpressionLiteral(Tokenizer *t);
static ASTExpression tokenstoASTExpressionFunctionArgumentList(Tokenizer *t);
static ASTExpression tokenstoASTExpression(Tokenizer *t);

static ASTStatement tokenstoASTStatementCompound(Tokenizer *t);
static ASTStatement tokenstoASTStatementConditional(Tokenizer *t);
static ASTStatement tokenstoASTStatementReturn(Tokenizer *t);
static ASTStatement tokenstoASTStatementExpression(Tokenizer *t);
static ASTStatement tokenstoASTStatementInlineAssembly(Tokenizer *t);
static ASTStatement tokenstoASTStatementVariableDeclaration(Tokenizer *t, int seek);
static ASTStatement tokenstoASTStatement(Tokenizer *t);

static ASTGlobal tokenstoASTGlobalFunction(Tokenizer *t);
static ASTGlobal tokenstoASTGlobalExport(Tokenizer *t);
static ASTGlobal tokenstoASTGlobal(Tokenizer *t);

static Token *
prevToken(Tokenizer *t)
{
	return (--t->pos >= 0) ? &(t->data[t->pos]) : NULL;
}

static Token *
nextToken(Tokenizer *t)
{
	return ((unsigned)(++t->pos) < t->len) ? &(t->data[t->pos]) : NULL;
}

static Token *
nextTokenType(Tokenizer *t, TokenType type)
{
	Token *tok;
	if ((tok = nextToken(t)) != NULL) {
		if (tok->type != type)
			error(tok, "unexpected '%s' (expected '%s')",
					strTokenType(tok->type), strTokenType(type));
	}
	return tok;
}

static Token *
enextToken(Tokenizer *t)
{
	Token *tok;
	if ((tok = nextToken(t)) == NULL)
		error(tok, "unexpected end of input");
	return tok;
}

static Token *
enextTokenType(Tokenizer *t, TokenType type)
{
	Token *tok;
	tok = enextToken(t);
	if (tok->type != type)
		error(tok, "unexpected '%s' (expected '%s')",
				strTokenType(tok->type), strTokenType(type));
	return tok;
}

/*****************************************************************************/

/* Expressions */

static ASTExpression
tokenstoASTExpressionLiteral(Tokenizer *t)
{
	ASTExpression expr;
	Token *tok;
	tok = enextToken(t);

	expr.Any.any.inittoken = tok;

	if (tok->type == TokenIdentifier) {
		expr.type = ASTExpressionLiteralIdentifier_T;
		expr.Literal.value = Strdup(tok->str).data;
	} else if (tok->type == TokenInteger) {
		expr.type = ASTExpressionLiteralInteger_T;
		expr.Literal.value = Strdup(tok->str).data;
	} else if (tok->type == TokenString) {
		expr.type = ASTExpressionLiteralString_T;
		expr.Literal.value = Strdup((String){tok->str.data + 1, tok->str.len - 2}).data;
	}

	return expr;
}

static ASTExpression
tokenstoASTExpressionFunctionArgumentList(Tokenizer *t)
{
	ASTExpression expr;
	Token *tok;

	expr.type = ASTExpressionFunctionArgumentList_T;
	newVector(expr.FunctionArgumentList);
	tok = enextTokenType(t, TokenOpeningParenthesis);

	expr.Any.any.inittoken = tok;

	if ((tok = enextToken(t))->type != TokenClosingParenthesis) {
		prevToken(t);
		do {
			pushVector(expr.FunctionArgumentList, tokenstoASTExpression(t));
		} while ((tok = enextToken(t))->type == TokenComma);
		prevToken(t);
		enextTokenType(t, TokenClosingParenthesis);
	}

	return expr;
}

static ASTExpression
tokenstoASTExpression(Tokenizer *t)
{
	ASTExpression expr;
	Token *tok;
	tok = enextToken(t);

	expr.Any.any.inittoken = tok;

	if (0) {
	/* Literals: */
	} else if (tok->type == TokenIdentifier) {
		prevToken(t);
		expr = tokenstoASTExpressionLiteral(t);
	} else if (tok->type == TokenInteger) {
		prevToken(t);
		expr = tokenstoASTExpressionLiteral(t);
	} else if (tok->type == TokenString) {
		prevToken(t);
		expr = tokenstoASTExpressionLiteral(t);
	} else if (tok->type == TokenOpeningParenthesis) {
		expr = tokenstoASTExpression(t);
		enextTokenType(t, TokenClosingParenthesis);
	} else if (tok->type == TokenExclamationMark) {
		expr.type = ASTExpressionUnaryLogicalNot_T;
		new(expr.Unary.expr) = tokenstoASTExpression(t);
	} else if (tok->type == TokenMinus) {
		expr.type = ASTExpressionUnarySignChange_T;
		new(expr.Unary.expr) = tokenstoASTExpression(t);
	} else if (tok->type == TokenAmperstand) {
		expr.type = ASTExpressionUnaryAddressof_T;
		new(expr.Unary.expr) = tokenstoASTExpression(t);
	} else if (tok->type == TokenAsterisk) {
		expr.type = ASTExpressionUnaryValuefrom_T;
		new(expr.Unary.expr) = tokenstoASTExpression(t);
	} else if (tok->type == TokenPlusPlus) {
		expr.type = ASTExpressionUnaryPreincrement_T;
		new(expr.Unary.expr) = tokenstoASTExpression(t);
	} else if (tok->type == TokenMinusMinus) {
		expr.type = ASTExpressionUnaryPredecrement_T;
		new(expr.Unary.expr) = tokenstoASTExpression(t);
	} else {
		error(tok, "unexpected token: '%s'", strTokenType(tok->type));
	}

	tok = enextToken(t);
	if (tok->type == TokenOpeningParenthesis) { /* function call */
		ASTExpression callexpr = expr;

		prevToken(t);
		expr.type = ASTExpressionFunctionCall_T;
		new(expr.FunctionCall.callexpr) = callexpr;
		new(expr.FunctionCall.argv) =
			tokenstoASTExpressionFunctionArgumentList(t);
	} else if (tok->type == TokenAssignment) { /* assignment */
		ASTExpression left = expr;
		ASTExpression right = tokenstoASTExpression(t);
		expr.type = ASTExpressionBinaryAssignment_T;
		new(expr.BinaryAssignment.left) = left;
		new(expr.BinaryAssignment.right) = right;
	} else {
		prevToken(t);
	}

	return expr;
}

/* Statements */

static ASTStatement
tokenstoASTStatementCompound(Tokenizer *t)
{
	ASTStatement stat;
	Token *tok;

	stat.type = ASTStatementCompound_T;
	tok = enextTokenType(t, TokenOpeningBrace);
	stat.Any.any.inittoken = tok;
	newVector(stat.Compound);

	while ((tok = enextToken(t))->type != TokenClosingBrace) {
		prevToken(t);
		pushVector(stat.Compound, tokenstoASTStatement(t));
	}

	return stat;
}

static ASTStatement
tokenstoASTStatementConditional(Tokenizer *t)
{
	/* if <expr> <statement> [else <statement>]; */
	ASTStatement stat;
	Token *tok;

	stat.type = ASTStatementConditional_T;
	tok = enextTokenType(t, TokenIdentifier);
	stat.Any.any.inittoken = tok;
	if (Strccmp(tok->str, "if"))
		error(tok, "expected 'if' keyword");

	new(stat.Conditional.condition) = tokenstoASTExpression(t);
	new(stat.Conditional.body) = tokenstoASTStatement(t);

	tok = enextToken(t);
	if (tok->type == TokenIdentifier && !Strccmp(tok->str, "else"))
		new(stat.Conditional.elsebody) = tokenstoASTStatement(t);
	else {
		prevToken(t); stat.Conditional.elsebody = NULL;
	}

	return stat;
}

static ASTStatement
tokenstoASTStatementReturn(Tokenizer *t)
{
	/* return <expr>; */
	ASTStatement stat;
	Token *tok;

	stat.type = ASTStatementReturn_T;
	tok = enextTokenType(t, TokenIdentifier);
	stat.Any.any.inittoken = tok;
	if (Strccmp(tok->str, "return"))
		error(tok, "expected 'return' keyword");

	*(stat.Return.expr = malloc(sizeof *stat.Return.expr)) = tokenstoASTExpression(t);

	return stat;
}

static ASTStatement
tokenstoASTStatementExpression(Tokenizer *t)
{
	/* <expr>; */
	ASTStatement stat;

	stat.type = ASTStatementExpression_T;
	*(stat.Expression.expr = malloc(sizeof *stat.Expression.expr))
		= tokenstoASTExpression(t);
	stat.Any.any.inittoken = stat.Expression.expr->Any.any.inittoken;

	return stat;
}

static ASTStatement
tokenstoASTStatementInlineAssembly(Tokenizer *t)
{
	/* asm <string literal>; */
	ASTStatement stat;
	Token *tok;

	stat.type = ASTStatementInlineAssembly_T;
	tok = enextTokenType(t, TokenIdentifier);
	stat.Any.any.inittoken = tok;
	if (Strccmp(tok->str, "asm"))
		error(tok, "expected 'asm' keyword");

	stat.InlineAssembly.expr = tokenstoASTExpressionLiteral(t).Literal;

	return stat;
}

static ASTStatement
tokenstoASTStatementVariableDeclaration(Tokenizer *t, int seek)
{
	/* var <identifier literal>; */
	ASTStatement stat;
	Token *tok;

	stat.type = ASTStatementVariableDeclaration_T;
	if (!seek) {
		tok = enextTokenType(t, TokenIdentifier);
		stat.Any.any.inittoken = tok;
		if (Strccmp(tok->str, "var"))
			error(tok, "expected 'var' keyword");
	} else {
		tok = enextToken(t);
		stat.Any.any.inittoken = tok;
		prevToken(t);
	}

	stat.VariableDeclaration.name = tokenstoASTExpressionLiteral(t).Literal;

	return stat;
}

static ASTStatement
tokenstoASTStatement(Tokenizer *t)
{
	ASTStatement stat;
	Token *tok;

	tok = enextToken(t);
	stat.Any.any.inittoken = tok;

	if (0) {
	} else if (tok->type == TokenSemicolon) {
		stat.type = ASTStatementNoOp_T;
	} else if (tok->type == TokenOpeningBrace) {
		prevToken(t);
		stat = tokenstoASTStatementCompound(t);
	} else if (tok->type == TokenIdentifier && !Strccmp(tok->str, "if")) {
		prevToken(t);
		stat = tokenstoASTStatementConditional(t);
	} else if (tok->type == TokenIdentifier && !Strccmp(tok->str, "return")) {
		prevToken(t);
		stat = tokenstoASTStatementReturn(t);
		tok = enextTokenType(t, TokenSemicolon);
	} else if (tok->type == TokenIdentifier && !Strccmp(tok->str, "asm")) {
		prevToken(t);
		stat = tokenstoASTStatementInlineAssembly(t);
		tok = enextTokenType(t, TokenSemicolon);
	} else if (tok->type == TokenIdentifier && !Strccmp(tok->str, "var")) {
		stat = tokenstoASTStatementVariableDeclaration(t, 1);
		tok = enextTokenType(t, TokenSemicolon);
	} else {
		prevToken(t);
		stat = tokenstoASTStatementExpression(t);
		tok = enextTokenType(t, TokenSemicolon);
	}

	return stat;
}

/* Globals */

static ASTGlobal
tokenstoASTGlobalFunction(Tokenizer *t)
{
	ASTGlobal global;
	Token *tok;

	global.type = ASTGlobalFunction_T;
	newVector(global.Function.parameters);
	tok = enextTokenType(t, TokenIdentifier);
	global.Any.any.inittoken = tok;
	if (Strccmp(tok->str, "function"))
		error(tok, "expected 'function' keyword");

	global.Function.name = tokenstoASTExpressionLiteral(t).Literal;

	tok = enextTokenType(t, TokenOpeningParenthesis);
	if ((tok = enextToken(t))->type != TokenClosingParenthesis) {
		prevToken(t);
		do {
			pushVector(global.Function.parameters,
					tokenstoASTStatementVariableDeclaration(t, 1).VariableDeclaration);
		} while ((tok = enextToken(t))->type == TokenComma);
		prevToken(t);
		enextTokenType(t, TokenClosingParenthesis);
	}

	new(global.Function.body) = tokenstoASTStatement(t);

	return global;
}

static ASTGlobal
tokenstoASTGlobalExport(Tokenizer *t)
{
	ASTGlobal global;
	Token *tok;

	global.type = ASTGlobalExport_T;
	tok = enextTokenType(t, TokenIdentifier);
	global.Any.any.inittoken = tok;
	if (Strccmp(tok->str, "export"))
		error(tok, "expected 'export' keyword");

	global.Export.name = tokenstoASTExpressionLiteral(t).Literal;

	return global;
}

static ASTGlobal
tokenstoASTGlobal(Tokenizer *t)
{
	ASTGlobal global;
	Token *tok;

	tok = enextToken(t);
	global.Any.any.inittoken = tok;

	if (0) {
	} else if (tok->type == TokenIdentifier && !Strccmp(tok->str, "function")) {
		prevToken(t);
		global = tokenstoASTGlobalFunction(t);
	} else if (tok->type == TokenIdentifier && !Strccmp(tok->str, "export")) {
		prevToken(t);
		global = tokenstoASTGlobalExport(t);
	} else {
		error(tok, "unexpected token");
	}

	return global;
}

ASTModule
tokenstoASTModule(Token *tdata, size_t tlen)
{
	ASTModule module;
	Token *tok;
	Tokenizer t = {tdata, tlen, -1};

	newVector(module);

	while ((tok = nextToken(&t)) != NULL) {
		prevToken(&t);
		pushVector(module, tokenstoASTGlobal(&t));
	}

	return module;
}

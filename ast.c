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

static Token *prevToken(Tokenizer *t);

static Token *nextToken(Tokenizer *t);
static Token *nextTokenType(Tokenizer *t, TokenType type);
static Token *enextToken(Tokenizer *t);
static Token *enextTokenType(Tokenizer *t, TokenType type);

static ASTStatement tokenstoASTStatement(Tokenizer *t);

static ASTGlobal tokenstoASTGlobalFunction(Tokenizer *t);

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
			error(tok, "token type mismatch (expected %s, got %s)",
					strTokenType(type), strTokenType(tok->type));
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
		error(tok, "token type mismatch (expected %s, got %s)",
				strTokenType(type), strTokenType(tok->type));
	return tok;
}

/*****************************************************************************/

/* Expressions */

static ASTExpression
tokenstoASTExpression(Tokenizer *t)
{
	ASTExpression expr;
	Token *tok;
	tok = enextToken(t);

	if (0) {
	/* Literals: */
	} else if (tok->type == TokenIdentifier) {
		expr.type = ASTExpressionLiteralIdentifier_T;
		expr.Literal.value = Strdup(tok->str).data;
	} else if (tok->type == TokenInteger) {
		expr.type = ASTExpressionLiteralInteger_T;
		expr.Literal.value = Strdup(tok->str).data;
	} else if (tok->type == TokenString) {
		expr.type = ASTExpressionLiteralString_T;
		expr.Literal.value = Strdup(tok->str).data;
	}

	return expr;
}

/* Statements */

static ASTStatement
tokenstoASTStatementCompound(Tokenizer *t)
{
	ASTStatement stat;
	Token *tok;

	tok = enextTokenType(t, TokenOpeningBrace);
	stat.type = ASTStatementCompound_T;
	newVector(stat.Compound);

	while ((tok = enextToken(t))->type != TokenClosingBrace) {
		prevToken(t);
		pushVector(stat.Compound, tokenstoASTStatement(t));
	}

	return stat;
}

static ASTStatement
tokenstoASTStatementReturn(Tokenizer *t)
{
	/* return <expr>; */
	ASTStatement stat;
	Token *tok;

	tok = enextTokenType(t, TokenIdentifier);
	if (Strccmp(tok->str, "return"))
		error(tok, "expected 'return' keyword");

	*(stat.Return.expr = malloc(sizeof *stat.Return.expr)) = tokenstoASTExpression(t);

	tok = enextTokenType(t, TokenSemicolon);

	return stat;
}

static ASTStatement
tokenstoASTStatement(Tokenizer *t)
{
	ASTStatement stat;
	Token *tok;

	tok = enextToken(t);

	if (0) {
	} else if (tok->type == TokenSemicolon) {
		stat.type = ASTStatementNoOp_T;
	} else if (tok->type == TokenOpeningBrace) {
		prevToken(t);
		stat = tokenstoASTStatementCompound(t);
	} else if (tok->type == TokenIdentifier && !Strccmp(tok->str, "return")) {
		prevToken(t);
		stat = tokenstoASTStatementReturn(t);
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
	global.Function.name = tokenstoASTExpression(t).Literal;

	tok = enextTokenType(t, TokenOpeningParenthesis);
	while ((tok = enextToken(t)) != NULL) {
		/* TODO: parameters */
		if (tok->type == TokenClosingParenthesis)
			break;
	}

	global.Function.body = tokenstoASTStatement(t);

	return global;
}

ASTModule
tokenstoASTModule(Token *tdata, size_t tlen)
{
	ASTModule module;
	Token *tok;
	Tokenizer t = {tdata, tlen, -1};

	newVector(module);

	while ((tok = nextTokenType(&t, TokenIdentifier)) != NULL) {
		if (!Strccmp(tok->str, "function")) {
			pushVector(module, tokenstoASTGlobalFunction(&t));
		}
	}

	return module;
}

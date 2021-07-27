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

#ifndef _AST_H
#define _AST_H

#include <str.h>
#include <tokenize.h>

/* Expressions */
struct ASTExpressionAny;
struct ASTExpressionLiteral;
union ASTExpression;

/* Statements */
struct ASTStatementAny;
struct ASTStatementReturn;
struct ASTStatementCompound;
union ASTStatement;

/* Global parts */
struct ASTAny;
struct ASTGlobalFunction;
union ASTGlobal;

/**/

typedef enum ASTExpressionType {
	ASTExpressionNULL_T = 0,
	ASTExpressionLiteralIdentifier_T,
	ASTExpressionLiteralInteger_T,
	ASTExpressionLiteralString_T,
	ASTExpressionUnaryNegation_T,
	ASTExpressionUnarySignChange_T,
	ASTExpressionUnaryAddressof_T,
	ASTExpressionUnaryValuefrom_T,
	ASTExpressionUnaryPreincrement_T,
	ASTExpressionUnaryPredecrement_T,
	ASTExpressionFunctionCall_T,
	ASTExpression_Count,
} ASTExpressionType;

typedef struct ASTExpressionAny {
	enum ASTExpressionType type;
} ASTExpressionAny;

typedef struct ASTExpressionLiteral {
	enum ASTExpressionType type;
	char *value;
} ASTExpressionLiteral;

typedef struct ASTExpressionUnary {
	enum ASTExpressionType type;
	union ASTExpression *expr;
} ASTExpressionUnary;

typedef struct ASTExpressionFunctionCall {
	enum ASTExpressionType type;
	union ASTExpression *callexpr;
} ASTExpressionFunctionCall;

typedef union ASTExpression {
	enum ASTExpressionType type;
	struct ASTExpressionAny Any;
	struct ASTExpressionLiteral Literal;
	struct ASTExpressionUnary Unary;
	struct ASTExpressionUnary UnaryNegation;
	struct ASTExpressionUnary UnarySignChange;
	struct ASTExpressionUnary UnaryAddressof;
	struct ASTExpressionUnary UnaryValuefrom;
	struct ASTExpressionUnary UnaryPreincrement;
	struct ASTExpressionUnary UnaryPredecrement;
	struct ASTExpressionFunctionCall FunctionCall;
} ASTExpression;

/**/

typedef enum ASTStatementType {
	ASTStatementNoOp_T = 0,
	ASTStatementCompound_T,
	ASTStatementConditional_T,
	ASTStatementReturn_T,
	ASTStatementExpression_T,
	ASTStatementInlineAssembly_T,
	ASTStatementVariableDeclaration_T,
	ASTStatement_Count,
} ASTStatementType;

typedef struct ASTStatementAny {
	enum ASTStatementType type;
} ASTStatementAny;

typedef struct ASTStatementCompound {
	enum ASTStatementType type;
	union ASTStatement *data;
	size_t len;
} ASTStatementCompound;

typedef struct ASTStatementConditional {
	enum ASTStatementType type;
	union ASTExpression *condition;
	union ASTStatement *body;
} ASTStatementConditional;

typedef struct ASTStatementReturn {
	enum ASTStatementType type;
	union ASTExpression *expr;
} ASTStatementReturn;

typedef struct ASTStatementExpression {
	enum ASTStatementType type;
	union ASTExpression *expr;
} ASTStatementExpression;

typedef struct ASTStatementInlineAssembly {
	enum ASTStatementType type;
	struct ASTExpressionLiteral expr;
} ASTStatementInlineAssembly;

typedef struct ASTStatementVariableDeclaration {
	enum ASTStatementType type;
	struct ASTExpressionLiteral name;
} ASTStatementVariableDeclaration;

typedef union ASTStatement {
	enum ASTStatementType type;
	struct ASTStatementAny Any;
	struct ASTStatementCompound Compound;
	struct ASTStatementConditional Conditional;
	struct ASTStatementReturn Return;
	struct ASTStatementExpression Expression;
	struct ASTStatementInlineAssembly InlineAssembly;
	struct ASTStatementVariableDeclaration VariableDeclaration;
} ASTStatement;

/**/

typedef enum ASTGlobalType {
	ASTGlobalNothing_T = 0,
	ASTGlobalFunction_T,
	ASTGlobal_Count,
} ASTGlobalType;

typedef struct ASTGlobalAny {
	enum ASTGlobalType type;
} ASTGlobalAny;

typedef struct ASTGlobalFunction {
	enum ASTGlobalType type;
	struct ASTExpressionLiteral name;
	union ASTStatement *body;
} ASTGlobalFunction;

typedef union ASTGlobal {
	enum ASTGlobalType type;
	struct ASTGlobalAny Any;
	struct ASTGlobalFunction Function;
} ASTGlobal;

/**/

typedef struct ASTModule {
	union ASTGlobal *data;
	size_t len;
} ASTModule;

/**/

ASTModule tokenstoASTModule(Token *tdata, size_t tlen);

#endif

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

typedef struct ASTStructAny {
	Token *inittoken;
} ASTStructAny;

typedef enum ASTExpressionType {
	ASTExpressionNULL_T = 0,
	ASTExpressionLiteralIdentifier_T,
	ASTExpressionLiteralInteger_T,
	ASTExpressionLiteralString_T,
	ASTExpressionUnarySignChange_T,
	ASTExpressionUnaryAddressof_T,
	ASTExpressionUnaryValuefrom_T,
	ASTExpressionUnaryPreincrement_T,
	ASTExpressionUnaryPredecrement_T,
	ASTExpressionUnaryLogicalNot_T,
	ASTExpressionFunctionArgumentList_T,
	ASTExpressionFunctionCall_T,
	ASTExpressionBinaryAssignment_T,
	ASTExpression_Count,
} ASTExpressionType;

typedef struct ASTExpressionAny {
	enum ASTExpressionType type;
	struct ASTStructAny any;
} ASTExpressionAny;

typedef struct ASTExpressionLiteral {
	struct ASTExpressionAny any;
	char *value;
} ASTExpressionLiteral;

typedef struct ASTExpressionUnary {
	struct ASTExpressionAny any;
	union ASTExpression *expr;
} ASTExpressionUnary;

typedef struct ASTExpressionBinary {
	struct ASTExpressionAny any;
	union ASTExpression *left;
	union ASTExpression *right;
} ASTExpressionBinary;

typedef struct ASTExpressionFunctionArgumentList {
	struct ASTExpressionAny any;
	union ASTExpression *data;
	size_t len;
} ASTExpressionFunctionArgumentList;

typedef struct ASTExpressionFunctionCall {
	struct ASTExpressionAny any;
	union ASTExpression *callexpr;
	union ASTExpression *argv;
} ASTExpressionFunctionCall;

typedef union ASTExpression {
	enum ASTExpressionType type;
	struct ASTExpressionAny Any;
	struct ASTExpressionLiteral Literal;
	struct ASTExpressionUnary Unary;
	struct ASTExpressionUnary UnarySignChange;
	struct ASTExpressionUnary UnaryAddressof;
	struct ASTExpressionUnary UnaryValuefrom;
	struct ASTExpressionUnary UnaryPreincrement;
	struct ASTExpressionUnary UnaryPredecrement;
	struct ASTExpressionUnary UnaryLogicalNot;
	struct ASTExpressionFunctionArgumentList FunctionArgumentList;
	struct ASTExpressionFunctionCall FunctionCall;
	struct ASTExpressionBinary Binary;
	struct ASTExpressionBinary BinaryAssignment;
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
	struct ASTStructAny any;
} ASTStatementAny;

typedef struct ASTStatementCompound {
	struct ASTStatementAny any;
	union ASTStatement *data;
	size_t len;
} ASTStatementCompound;

typedef struct ASTStatementConditional {
	struct ASTStatementAny any;
	union ASTExpression *condition;
	union ASTStatement *body, *elsebody;
} ASTStatementConditional;

typedef struct ASTStatementReturn {
	struct ASTStatementAny any;
	union ASTExpression *expr;
} ASTStatementReturn;

typedef struct ASTStatementExpression {
	struct ASTStatementAny any;
	union ASTExpression *expr;
} ASTStatementExpression;

typedef struct ASTStatementInlineAssembly {
	struct ASTStatementAny any;
	struct ASTExpressionLiteral expr;
} ASTStatementInlineAssembly;

typedef struct ASTStatementVariableDeclaration {
	struct ASTStatementAny any;
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
	ASTGlobalExport_T,
	ASTGlobal_Count,
} ASTGlobalType;

typedef struct ASTGlobalAny {
	enum ASTGlobalType type;
	struct ASTStructAny any;
} ASTGlobalAny;

typedef struct ASTGlobalFunction {
	struct ASTGlobalAny any;
	struct ASTExpressionLiteral name;
	struct {
		struct ASTStatementVariableDeclaration *data;
		size_t len;
	} parameters;
	union ASTStatement *body;
} ASTGlobalFunction;

typedef struct ASTGlobalExport {
	struct ASTGlobalAny any;
	struct ASTExpressionLiteral name;
} ASTGlobalExport;

typedef union ASTGlobal {
	enum ASTGlobalType type;
	struct ASTGlobalAny Any;
	struct ASTGlobalFunction Function;
	struct ASTGlobalExport Export;
} ASTGlobal;

/**/

typedef struct ASTModule {
	union ASTGlobal *data;
	size_t len;
} ASTModule;

/**/

ASTModule tokenstoASTModule(Token *tdata, size_t tlen);

#endif

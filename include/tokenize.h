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

#ifndef _TOKENIZE_H
#define _TOKENIZE_H

#include <str.h>

typedef enum {
	TokenNULL,
	TokenIdentifier, TokenInteger, TokenString,
	TokenOpeningParenthesis, TokenClosingParenthesis,
	TokenOpeningBracket, TokenClosingBracket,
	TokenOpeningBrace, TokenClosingBrace,
	TokenAsterisk, TokenAmperstand,
	TokenSemicolon, TokenComma, TokenDot,
	TokenColon, TokenDoubleColon,
} TokenType;

typedef struct Token {
	TokenType type;
	String str;
	size_t line, col;
} Token;

Token newToken(void);

ssize_t tokenize(String input, Token **output);
char *strTokenType(TokenType type);

#endif

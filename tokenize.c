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

#include <tokenize.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

Token
newToken(void)
{
	Token t;
	return *(Token *)(memset(&t, 0, sizeof t));
}

ssize_t
tokenize(String input, Token **output)
#define NOT_OVERFLOW (i < input.len)
#define CURCHAR (input.data[i])
#define NEXTCHAR (i + 1 < input.len ? input.data[i + 1] : 0)
#define CHARPLUS(N) (i + (N) < input.len ? input.data[i + (N)] : 0)
#define CURTOK ((*output)[tokcount - 1])
#define TYPE(T) (CURTOK.type = Token##T)
#define SEEKCHAR (++i, ++col)
{
	size_t tokcount;
	size_t i, initial_i;
	size_t ln, col;

	*output = malloc(tokcount = 0);
	i = ln = col = 0;

	while (NOT_OVERFLOW) {
		*output = realloc(*output, (++tokcount) * sizeof (**output));
		CURTOK = newToken();

		while (isspace(CURCHAR) && NOT_OVERFLOW) {
			if (CURCHAR == '\n') {
				++ln; col = 0;
				++i;
			} else SEEKCHAR;
		}

		CURTOK.str.data = input.data + i;
		CURTOK.line = ln;
		CURTOK.col = col;

		initial_i = i;

		if (!NOT_OVERFLOW) {
			break;
		/* Identifier */
		} else if ((CURCHAR >= 'a' && CURCHAR <= 'z')
		     ||  (CURCHAR >= 'A' && CURCHAR <= 'Z')
			 ||  (CURCHAR == '_')) {
			TYPE(Identifier);
			SEEKCHAR;
			while (((CURCHAR >= 'a' && CURCHAR <= 'z')
			   ||   (CURCHAR >= 'A' && CURCHAR <= 'Z')
			   ||   (CURCHAR >= '0' && CURCHAR <= '9')
			   ||   (CURCHAR == '_'))
			   &&  NOT_OVERFLOW) SEEKCHAR;
		/* Integer */
		} else if (CURCHAR >= '0' && CURCHAR <= '9') {
			TYPE(Integer);
			SEEKCHAR;
			while ((CURCHAR >= '0' && CURCHAR <= '9')
			   &&  NOT_OVERFLOW) SEEKCHAR;
		/* String */
		} else if (CURCHAR == '"') {
			TYPE(String);
			SEEKCHAR;
			while (CURCHAR != '"' && NOT_OVERFLOW) SEEKCHAR;
			SEEKCHAR;
		/* Unary and conditional operators */
		} else if (CURCHAR == '!') {
			TYPE(ExclamationMark);
			if (NEXTCHAR == '=') {
				TYPE(NotEqual); SEEKCHAR;
			}
			SEEKCHAR;
		} else if (CURCHAR == '=') {
			TYPE(Assignment);
			if (NEXTCHAR == '=') {
				TYPE(Equal); SEEKCHAR;
			}
			SEEKCHAR;
		} else if (CURCHAR == '-') {
			TYPE(Minus);
			if (NEXTCHAR == '-') {
				TYPE(MinusMinus); SEEKCHAR;
			} else if (NEXTCHAR == '=') {
				TYPE(MinusEqual); SEEKCHAR;
			}
			SEEKCHAR;
		} else if (CURCHAR == '+') {
			TYPE(Plus);
			if (NEXTCHAR == '+') {
				TYPE(PlusPlus); SEEKCHAR;
			} else if (NEXTCHAR == '=') {
				TYPE(PlusEqual); SEEKCHAR;
			}
			SEEKCHAR;
		/* Brackets */
		} else if (CURCHAR == '(') { TYPE(OpeningParenthesis); SEEKCHAR;
		} else if (CURCHAR == ')') { TYPE(ClosingParenthesis); SEEKCHAR;
		} else if (CURCHAR == '[') { TYPE(OpeningBracket); SEEKCHAR;
		} else if (CURCHAR == ']') { TYPE(ClosingBracket); SEEKCHAR;
		} else if (CURCHAR == '{') { TYPE(OpeningBrace); SEEKCHAR;
		} else if (CURCHAR == '}') { TYPE(ClosingBrace); SEEKCHAR;
		/* Other single-char operators */
		} else if (CURCHAR == '*') { TYPE(Asterisk); SEEKCHAR;
		} else if (CURCHAR == '&') { TYPE(Amperstand); SEEKCHAR;
		} else if (CURCHAR == ';') { TYPE(Semicolon); SEEKCHAR;
		} else if (CURCHAR == ',') { TYPE(Comma); SEEKCHAR;
		} else if (CURCHAR == '.') { TYPE(Dot); SEEKCHAR;
		} else if (CURCHAR == ':') {
			TYPE(Colon);
			if (NEXTCHAR == ':') {
				TYPE(DoubleColon); SEEKCHAR;
			}
			SEEKCHAR;
		} else {
			SEEKCHAR;
		}

		CURTOK.str.len = i - initial_i;
	}

	return (signed)--tokcount;
}
#undef NOT_OVERFLOW
#undef CURCHAR
#undef NEXTCHAR
#undef CHARPLUS
#undef CURTOK
#undef TYPE
#undef SEEKCHAR

char *
strTokenType(TokenType type)
{
	switch (type) {
	case TokenNULL: return "<null>"; break;
	case TokenIdentifier: return "identifier"; break;
	case TokenInteger: return "integer"; break;
	case TokenString: return "string"; break;
	case TokenOpeningParenthesis: return "opening parenthesis"; break;
	case TokenClosingParenthesis: return "closing parenthesis"; break;
	case TokenOpeningBracket: return "opening bracket"; break;
	case TokenClosingBracket: return "closing bracket"; break;
	case TokenOpeningBrace: return "opening brace"; break;
	case TokenClosingBrace: return "closing brace"; break;
	case TokenAsterisk: return "asterisk"; break;
	case TokenAmperstand: return "amperstand"; break;
	case TokenSemicolon: return "semicolon"; break;
	case TokenComma: return "comma"; break;
	case TokenDot: return "dot"; break;
	case TokenColon: return "colon"; break;
	case TokenDoubleColon: return "double colon"; break;
	}
	return "unknown";
}

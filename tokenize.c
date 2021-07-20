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
{
	size_t tokcount;
	size_t i, initial_i;

	*output = malloc(tokcount = 0);
	i = 0;

	while (NOT_OVERFLOW) {
		*output = realloc(*output, (++tokcount) * sizeof (**output));
		CURTOK = newToken();

		while (isspace(CURCHAR) && NOT_OVERFLOW)
			++i;

		CURTOK.str.data = input.data + i;
		initial_i = i;

		if (!NOT_OVERFLOW) {
			break;
		/* Identifier */
		} else if ((CURCHAR >= 'a' && CURCHAR <= 'z')
		     ||  (CURCHAR >= 'A' && CURCHAR <= 'Z')
			 ||  (CURCHAR == '_')) {
			TYPE(Identifier);
			++i;
			while (((CURCHAR >= 'a' && CURCHAR <= 'z')
			   ||   (CURCHAR >= 'A' && CURCHAR <= 'Z')
			   ||   (CURCHAR >= '0' && CURCHAR <= '9')
			   ||   (CURCHAR == '_'))
			   &&  NOT_OVERFLOW) ++i;
		/* Integer */
		} else if (CURCHAR >= '0' && CURCHAR <= '9') {
			TYPE(Integer);
			++i;
			while ((CURCHAR >= '0' && CURCHAR <= '9')
			   &&  NOT_OVERFLOW) ++i;
		/* String */
		} else if (CURCHAR == '"') {
			TYPE(String);
			++i;
			while (CURCHAR != '"' && NOT_OVERFLOW) ++i;
			++i;
		/* Brackets */
		} else if (CURCHAR == '(') { TYPE(OpeningParenthesis); ++i;
		} else if (CURCHAR == ')') { TYPE(ClosingParenthesis); ++i;
		} else if (CURCHAR == '[') { TYPE(OpeningBracket); ++i;
		} else if (CURCHAR == ']') { TYPE(ClosingBracket); ++i;
		} else if (CURCHAR == '{') { TYPE(OpeningBrace); ++i;
		} else if (CURCHAR == '}') { TYPE(ClosingBrace); ++i;
		/* Other single-char operators */
		} else if (CURCHAR == '*') { TYPE(Asterisk); ++i;
		} else if (CURCHAR == '&') { TYPE(Amperstand); ++i;
		} else if (CURCHAR == ';') { TYPE(Semicolon); ++i;
		} else if (CURCHAR == ',') { TYPE(Comma); ++i;
		} else if (CURCHAR == '.') { TYPE(Dot); ++i;
		} else if (CURCHAR == ':') {
			TYPE(Colon);
			if (NEXTCHAR == ':') {
				TYPE(DoubleColon); ++i;
			}
			++i;
		} else {
			++i;
		}

		CURTOK.str.len = i - initial_i;
	}

	return (signed)tokcount;
}
#undef NOT_OVERFLOW
#undef CURCHAR
#undef NEXTCHAR
#undef CHARPLUS
#undef CURTOK
#undef TYPE

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

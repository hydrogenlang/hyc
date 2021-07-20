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

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include <arg.h>
#include <str.h>
#include <tokenize.h>
#include <util.h>

/* function prototypes */
static ssize_t mapfile(const char *filename, char **ptr);
static ssize_t readfile(const char *filename, char *data, size_t len);
static void usage(void);

/* global variables */
char *argv0;

/* function declarations */
static ssize_t
mapfile(const char *filename, char **ptr)
{
	struct stat sb;
	int fd;
	ssize_t siz;

	if ((fd = open(filename, O_RDONLY)) < 0)
		return -1;
	if (fstat(fd, &sb) < 0)
		return -1;
	siz = (ssize_t)sb.st_size;
	if ((*ptr = mmap(NULL, (unsigned)siz, PROT_READ, MAP_PRIVATE, fd, 0)) == NULL)
		return -1;
	if (close(fd) < 0)
		return -1;
	return siz;
}

static ssize_t
readfile(const char *filename, char *data, size_t len)
{
	int fd;
	ssize_t rb;

	if ((fd = open(filename, O_RDONLY)) < 0)
		return (ssize_t)fd;
	if ((rb = read(fd, data, len)) < 0)
		return rb;
	if ((fd = close(fd)) < 0)
		return fd;
	return rb;
}

static void
usage(void)
{
	die("usage: %s [-h]", argv0);
}

/* main */
int
main(int argc, char *argv[])
{
	int i;
	String s;

	(void)readfile;

	ARGBEGIN {
	default:
		usage(); break;
	} ARGEND

	for (i = 0; i < argc; ++i) {
		Array(Token) tokens;

		if ((signed)(s.len = (unsigned)mapfile(argv[i], &(s.data))) < 0)
			die("mapfile(%s):", argv[i]);

		if ((signed)(tokens.len = (unsigned)tokenize(s, &tokens.data)) < 0)
			die("tokenize(%s):", argv[i]);

		/* write(STDOUT_FILENO, s.data, s.len); */
		{
			size_t j;
			for (j = 0; j < tokens.len; ++j) {
				printf("[%4ld]: %s | \"%.*s\"\n",
						j, strTokenType(tokens.data[j].type),
						(int)tokens.data[j].str.len, tokens.data[j].str.data);
			}
		}
		free(tokens.data);
		munmap(s.data, s.len);
	}

	return 0;
}

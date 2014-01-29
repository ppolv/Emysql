#include <stdio.h>
#include <mysql.h>
#include "erl_nif.h"


#define MYSQL_LIMIT_BYTE 250
#define MYSQL_NULL_RESULT 251
#define MYSQL_RESP_EOF 254
#define MYSQL_TYPE_FIELD_EXTRA 225 // Column type doesnt exist, created for parsing extra params in field list

typedef struct {
    ERL_NIF_TERM    atom_undefined;
    ERL_NIF_TERM    atom_date;
    ERL_NIF_TERM    atom_time;
    ERL_NIF_TERM    atom_datetime;
    ERL_NIF_TERM    atom_eof;
    ERL_NIF_TERM    atom_incomplete;
    ERL_NIF_TERM    atom_empty;
} parser_st;

typedef struct {
	ErlNifEnv*      env;
	parser_st*		atoms;
	ERL_NIF_TERM 	raw;
	unsigned char*	buffer;
	int* 			columns;
	unsigned 		table_width;
	unsigned long  	frame_start;
	unsigned long  	frame_size;
	unsigned long  	buffer_size;
	long  			remaining_length;
	unsigned char*	pointer;
} Parser;


// Utils

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);

// Funcs

ERL_NIF_TERM parse_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// Byte Decoders

#define D1(s) 	((unsigned long long)((s)[0]&0xff))
#define D2(s) 	((D1(s) << 8 | D1(s+1)))
#define D2I(s) 	((D1(s+1) << 8 | D1(s))) // Invertido
#define D3(s) 	((D1(s) << 16 | D1(s+1) << 8 | D1(s+2)))
#define D3I(s) 	((D1(s+2) << 16 | D1(s+1) << 8 | D1(s)))
#define D4(s) 	((D1(s) << 24 | D1(s+1) << 16 | D1(s+2) << 8 | D1(s+3)))
#define D4I(s) 	((D1(s+3) << 24 | D1(s+2) << 16 | D1(s+1) << 8 | D1(s)))
#define D8(s) 	((D1(s) << 56 | D1(s+1) << 48 | D1(s+2) << 40 | D1(s+3) << 32 | D1(s+4) << 24 | D1(s+5) << 16 | D1(s+6) << 8 | D1(s+7)))
#define D8I(s) 	((D1(s+7) << 56 | D1(s+6) << 48 | D1(s+5) << 40 | D1(s+4) << 32 | D1(s+3) << 24 | D1(s+2) << 16 | D1(s+1) << 8 | D1(s)))
#include "nif.h"

ERL_NIF_TERM atom_undefined, atom_date, atom_time, atom_datetime;

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {
	parser_st* st = enif_alloc(sizeof(parser_st));

	st->atom_undefined 	= make_atom(env, "undefined");
    st->atom_date 		= make_atom(env, "date");
    st->atom_time 		= make_atom(env, "time");
    st->atom_datetime 	= make_atom(env, "datetime");
    st->atom_eof 		= make_atom(env, "eof");
    st->atom_incomplete = make_atom(env, "incomplete");
    st->atom_empty 		= make_atom(env, "empty");

    *priv = (void*) st;
	return 0;
}

static void unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
    return;
}

static ErlNifFunc sql_funcs[] =
{
	{"parse_buffer", 3, parse_buffer}
};

ERL_NIF_INIT(emysql_parse, sql_funcs, &load, NULL, NULL, &unload)
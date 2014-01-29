#include "nif.h"


ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name) {
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}
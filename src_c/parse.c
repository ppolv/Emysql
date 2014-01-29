#include "nif.h"


int init_parser(Parser* parser, ErlNifEnv* env, ERL_NIF_TERM arg, ErlNifBinary* buffer, ERL_NIF_TERM columns) {
	parser->env = env;
	parser->atoms = enif_priv_data(env);
	parser->raw = arg;
	parser->buffer = buffer->data;

	if(!enif_get_list_length(env, columns, &parser->table_width)) return 0; 

	ERL_NIF_TERM list, head, tail;
	int val, index = 0;

	parser->columns = (int *) enif_alloc(parser->table_width * sizeof(int));
	list = columns;

	while(enif_get_list_cell(env, list, &head, &tail)) {
		if (!enif_get_int(env, head, &val)) return 0;
		parser->columns[index] = val;

		index++;
		list = tail;
	}

	parser->frame_start = 0;
	parser->frame_size = 0;
	parser->buffer_size = buffer->size;

	return 1;
}

void destroy_parser(Parser* parser) {
    if(parser->columns != NULL) {
        enif_free(parser->columns);
    }
}

ERL_NIF_TERM parse_line(Parser* parser) {
	unsigned long idx = parser->frame_start;
	unsigned col = 0;
	
	ERL_NIF_TERM reply[parser->table_width];

	while(col < parser->table_width) {

		unsigned char size_length = (parser->buffer[idx] <= MYSQL_LIMIT_BYTE) ? (1) : (parser->buffer[idx] - MYSQL_LIMIT_BYTE);
		size_length *= (size_length == 4) ? (2) : (1);

		unsigned long long content_length = 0; 
		if (parser->buffer[idx] != MYSQL_NULL_RESULT) {
			switch (size_length) {
				case 1:
					content_length = D1(parser->buffer + idx);
					break;
				case 2:
					content_length = D2I(parser->buffer + idx + 1);
					size_length++;
					break;
				case 3:
					content_length = D3I(parser->buffer + idx + 1);
					size_length++;
					break;
				case 8:
					content_length =  D8I(parser->buffer + idx + 1);
					size_length++;
					break;
			}
		} else content_length = 0;

		unsigned char *new_val = parser->buffer + idx + size_length;
		MYSQL_TIME time_;

		if (content_length == 0) reply[col] = parser->atoms->atom_undefined;
		else if (parser->columns[col] == MYSQL_TYPE_BIT
			|| parser->columns[col] == MYSQL_TYPE_TINY_BLOB
			|| parser->columns[col] == MYSQL_TYPE_MEDIUM_BLOB
			|| parser->columns[col] == MYSQL_TYPE_LONG_BLOB
			|| parser->columns[col] == MYSQL_TYPE_BLOB
			|| parser->columns[col] == MYSQL_TYPE_VAR_STRING
			|| parser->columns[col] == MYSQL_TYPE_STRING) reply[col] = enif_make_sub_binary(parser->env, parser->raw, idx + size_length, size_length + content_length - 1);
		else if (parser->columns[col] == MYSQL_TYPE_TINY
			|| parser->columns[col] == MYSQL_TYPE_SHORT
			|| parser->columns[col] == MYSQL_TYPE_LONG
			|| parser->columns[col] == MYSQL_TYPE_LONGLONG
			|| parser->columns[col] == MYSQL_TYPE_INT24
			|| parser->columns[col] == MYSQL_TYPE_YEAR) reply[col] = enif_make_int(parser->env, atoi((char *) new_val));
		else if (parser->columns[col] == MYSQL_TYPE_DECIMAL
			|| parser->columns[col] == MYSQL_TYPE_NEWDECIMAL
			|| parser->columns[col] == MYSQL_TYPE_FLOAT
			|| parser->columns[col] == MYSQL_TYPE_DOUBLE) reply[col] = enif_make_double(parser->env, strtod((const char *) new_val, NULL));
		else if (parser->columns[col] == MYSQL_TYPE_DATE) {
			sscanf((const char*) new_val, "%d-%d-%d", &time_.year, &time_.month, &time_.day);

			reply[col] = enif_make_tuple2(parser->env,
        		parser->atoms->atom_date,
				enif_make_tuple3(parser->env, 
		            enif_make_int(parser->env, time_.year),
		            enif_make_int(parser->env, time_.month),
		            enif_make_int(parser->env, time_.day))
				);
		}
		else if (parser->columns[col] == MYSQL_TYPE_TIME) {
			sscanf((const char*) new_val, "%d:%d:%d", &time_.hour, &time_.minute, &time_.second);

            reply[col] = enif_make_tuple2(parser->env,
        		parser->atoms->atom_time,
        		enif_make_tuple3(parser->env, 
		            enif_make_int(parser->env, time_.hour),
		            enif_make_int(parser->env, time_.minute),
		            enif_make_int(parser->env, time_.second))
        		);
		}
		else if (parser->columns[col] == MYSQL_TYPE_TIMESTAMP
			|| parser->columns[col] == MYSQL_TYPE_DATETIME) {
			sscanf((const char*) new_val, "%d-%d-%d %d:%d:%d", &time_.year, &time_.month, &time_.day, &time_.hour, &time_.minute, &time_.second);

            reply[col] = enif_make_tuple2(parser->env,
        		parser->atoms->atom_datetime,
            	enif_make_tuple2(parser->env,
					enif_make_tuple3(parser->env, 
						enif_make_int(parser->env, time_.year),
						enif_make_int(parser->env, time_.month),
						enif_make_int(parser->env, time_.day)
					),
					enif_make_tuple3(parser->env,
						enif_make_int(parser->env, time_.hour),
						enif_make_int(parser->env, time_.minute),
						enif_make_int(parser->env, time_.second)
					)
				)
			);

		}
		else if (parser->columns[col] == MYSQL_TYPE_FIELD_EXTRA) {
			ERL_NIF_TERM default_content;
			if (content_length > 12) default_content = enif_make_sub_binary(parser->env, parser->raw, idx + size_length + 12, size_length + content_length - 13);
			else default_content = parser->atoms->atom_undefined;

			reply[col] = enif_make_tuple6(parser->env,
				enif_make_int(parser->env, D2I(new_val)),
				enif_make_int(parser->env, D4I(new_val + 2)),
				enif_make_int(parser->env, D1(new_val + 6)),
				enif_make_int(parser->env, D2I(new_val + 7)),
				enif_make_int(parser->env, D1(new_val + 11)),
				default_content
			);
		}

		col++;
		idx += content_length + size_length;

	}

	return enif_make_list_from_array(parser->env, reply, parser->table_width);

}

ERL_NIF_TERM parse_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	Parser par;
	Parser* parser = &par;

	ErlNifBinary buffer;

	if(!enif_inspect_binary(env, argv[0], &buffer)) return enif_make_badarg(env);
	if(!init_parser(parser, env, argv[0], &buffer, argv[1])) return enif_make_badarg(env);

	ERL_NIF_TERM reply = argv[2];
	ERL_NIF_TERM return_value;

	parser->remaining_length = buffer.size;
	parser->pointer = buffer.data + parser->frame_start;

	while (parser->frame_start < parser->buffer_size) {
		
		parser->frame_size = D3I(parser->pointer);

		parser->pointer += 3;
		unsigned long seg_num = D1(parser->pointer);
		parser->pointer += 1;

		parser->remaining_length -= (parser->frame_size + 4);

		if (parser->remaining_length < 0) {
			return_value = enif_make_tuple3(env, 
				parser->atoms->atom_incomplete,
				reply, 
				enif_make_sub_binary(env, parser->raw, parser->frame_start, parser->buffer_size - parser->frame_start));

			break;
		}

		if (*parser->pointer == MYSQL_RESP_EOF) {
			ERL_NIF_TERM remaining_buffer = enif_make_sub_binary(env, parser->raw, parser->frame_start + parser->frame_size + 4, parser->remaining_length);

			if (parser->frame_size == 5) {
				unsigned long server_status = D2I(parser->pointer + 3);

				return_value = enif_make_tuple5(env, 
					parser->atoms->atom_eof,
					enif_make_uint(env, server_status), 
					enif_make_uint(env, seg_num), 
					reply,
					remaining_buffer);
			} 

			else return_value = enif_make_tuple4(env, 
					parser->atoms->atom_eof,
					enif_make_uint(env, seg_num),
					reply,
					remaining_buffer);

			break;

		}

		parser->frame_start += 4;

		ERL_NIF_TERM row = parse_line(parser);
		reply = enif_make_list_cell(env, row, reply);

		parser->frame_start += parser->frame_size;
		parser->pointer += parser->frame_size;
	}

	if (parser->frame_start >= parser->buffer_size) return_value = enif_make_tuple3(env, 
												parser->atoms->atom_incomplete,
												reply, 
												parser->atoms->atom_empty);

	destroy_parser(parser);
	return return_value;
}
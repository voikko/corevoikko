/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* Operations for files and file names. */

namespace libvoikko { namespace morphology { namespace malaga {

/* File operations. =========================================================*/

extern FILE *open_stream( string_t file_name, string_t stream_mode );
/* Open file FILE_NAME and create a stream from/to it in mode STREAM_MODE.
 * Works like "fopen", but calls "error" if it doesn't work. */

extern void close_stream( FILE **stream_p, string_t file_name );
/* Close the stream *STREAM_P which is connected to the file FILE_NAME
 * and set *STREAM_P to NULL. Don't do anything if *STREAM_P == NULL.
 * Works like "fclose", but calls "error" if FILE_NAME != NULL and an error
 * occurs during closing. */

extern void read_vector( void *address, 
                         int_t item_size, 
                         int_t item_count, 
                         FILE *stream, 
                         string_t file_name );
/* Read ITEM_COUNT items, of size ITEM_SIZE each, from STREAM,
 * which is connected to file FILE_NAME, and store them at *ADDRESS.
 * Works like "fread", but calls "error" if it doesn't work. */

extern void *read_new_vector( int_t item_size, 
                              int_t item_count, 
                              FILE *stream, 
                              string_t file_name );
/* Read ITEM_COUNT items, of size ITEM_SIZE each, from STREAM,
 * which is connected to file FILE_NAME, into allocated memory block,
 * and return a pointer to that block. */

extern void map_file( string_t file_name, void **address, int_t *length );
/* Map file "file_name" into the memory. It will be available in the 
 * memory region starting at *ADDRESS and will occupy LENGTH bytes.
 * After usage, return the memory region via "unmap_file". */

extern void unmap_file( void **address, int_t length );
/* Return the memory region that has been allocated by "map_file".
 * The region starts at *ADDRESS and occupies LENGTH bytes. */

/* File name operations. ====================================================*/

extern char_t *absolute_path( string_t src_path, string_t relative_to );
/* Return the absolute path name which is equivalent to SRC_PATH.
 * If SRC_PATH starts with "~", it's replaced by the home directory of the
 * user whose login name is following (current user if no login name).
 * If RELATIVE_TO is not NULL, SRC_NAME is relative to that path name.
 * RELATIVE_TO must be an absolute path name (a directory or a file).
 * The returned path must be freed after use. */

}}}

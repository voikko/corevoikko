/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* Operations for files and file names. */

/* Includes. ================================================================*/

// XXX: what do we actually want here?
#include "voikko_defs.h"
#ifdef HAVE_GETPWUID_R
#define POSIX
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <setjmp.h>
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/files.hpp"
#ifdef POSIX
#include <unistd.h>
#include <pwd.h>
#include <sys/mman.h>
#include <fcntl.h>
#endif
#ifdef WIN32
#include <windows.h>
#endif

namespace libvoikko { namespace morphology { namespace malaga {

/* Constants. ===============================================================*/

enum {MAX_PATH_SIZE = 200}; /* Maximum path size in characters. */

/* Macros. ==================================================================*/

#define IS_LETTER(c) (((c) >= 'A' && (c) <= 'Z') || ((c) >= 'a' && (c) <= 'z'))

/* File operations. =========================================================*/

bool 
file_exists( string_t file_name )
/* Return true iff file FILE_NAME exists and can be read. */
{ 
  FILE *stream;

  stream = fopen( file_name, "r" );
  if (stream == NULL) 
    return false;
  fclose( stream );
  return true;
}

/*---------------------------------------------------------------------------*/

FILE *
open_stream( string_t file_name, string_t stream_mode )
/* Open file FILE_NAME and create a stream from/to it in mode STREAM_MODE.
 * Works like "fopen", but calls "error" if it doesn't work. */
{ 
  return fopen( file_name, stream_mode );
} 

/*---------------------------------------------------------------------------*/

void 
close_stream( FILE **stream_p, string_t file_name )
/* Close the stream *STREAM_P which is connected to the file FILE_NAME
 * and set *STREAM_P to NULL. Don't do anything if *STREAM_P == NULL.
 * Works like "fclose", but calls "error" if FILE_NAME != NULL and an error
 * occurs during closing. */
{ 
  FILE *stream = *stream_p;

  *stream_p = NULL;
  if (stream != NULL) {
      fclose(stream);
  }
}

/*---------------------------------------------------------------------------*/

void 
read_vector( void *address, int_t item_size, int_t item_count, 
             FILE *stream, string_t file_name )
/* Read ITEM_COUNT items, of size ITEM_SIZE each, from STREAM,
 * which is connected to file FILE_NAME, and store them at *ADDRESS.
 * Works like "fread", but calls "error" if it doesn't work. */
{ 
  if (fread( address, (size_t) item_size, (size_t) item_count, stream ) 
      < (size_t) item_count) 
  {
    return;
  }
}

/*---------------------------------------------------------------------------*/

void *
read_new_vector( int_t item_size, int_t item_count, 
                 FILE *stream, string_t file_name )
/* Read ITEM_COUNT items, of size ITEM_SIZE each, from STREAM,
 * which is connected to file FILE_NAME, into allocated memory block,
 * and return a pointer to that block.
 * The block must be freed after use. */
{ 
  void *block;

  block = new_vector( item_size, item_count );
  read_vector( block, item_size, item_count, stream, file_name );
  return block;
}

/*---------------------------------------------------------------------------*/

void 
map_file( string_t file_name, void **address, int_t *length )
/* Map file "file_name" into the memory. It will be available in the 
 * memory region starting at *ADDRESS and will occupy LENGTH bytes.
 * After usage, return the memory region via "unmap_file". */
{ 
#ifdef POSIX
  int file_descriptor;

  /* Get a file descriptor. */
  file_descriptor = open( file_name, O_RDONLY );

  /* Get file length. */
  *length = lseek( file_descriptor, 0, SEEK_END );

  *address = mmap( NULL, *length, PROT_READ, MAP_SHARED, file_descriptor, 0 );

  /* The file descriptor is no longer needed. */
  close( file_descriptor );
#endif

#ifdef WIN32
  HANDLE file_handle, map_handle;

  file_handle = CreateFile( file_name, GENERIC_READ, FILE_SHARE_READ, NULL, 
			    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );

  map_handle = CreateFileMapping( file_handle, NULL, PAGE_READONLY, 
				  0, 0, NULL );

  *address = MapViewOfFile( map_handle, FILE_MAP_READ, 0, 0, 0 );
  *length = GetFileSize( file_handle, NULL );

  CloseHandle( map_handle );
  CloseHandle( file_handle );
#endif
}

/*---------------------------------------------------------------------------*/

void 
unmap_file( void **address, int_t length )
/* Return the memory region that has been allocated by "map_file".
 * The region starts at *ADDRESS and occupies LENGTH bytes. */
{ 
#ifdef POSIX
  munmap( *address, length );
#endif
#ifdef WIN32
  UnmapViewOfFile( *address );
#endif
  *address = NULL;
}

/* File name operations. ====================================================*/

string_t 
name_in_path( string_t path_name )
/* Return the file name in PATH_NAME, 
 * i.e. the name after the last separator. */
{ 
  string_t name;

  for (name = path_name + strlen( path_name ); name > path_name; name--)
  {
    if (name[-1] == '/') 
      break;
#ifdef WIN32
    if (name[-1] == '\\'
        || (name == path_name + 2 && IS_LETTER( name[-2] ) && name[-1] == ':'))
    {
      break;
    }
#endif
  }
  return name;
}

/*---------------------------------------------------------------------------*/

static string_t 
get_env( string_t name )
/* Get the content of the environment variable NAME.
 * Emit an error if it is not defined. */
{ 
  return getenv( name );
}

/*---------------------------------------------------------------------------*/

static void 
tidy_path( char_t *path )
/* Remove all superfluous "..", "." and "/" in PATH.
 * PATH must be absolute. */
{ 
  char_t *src_p;
  char_t *dest_p;

#ifdef POSIX
  dest_p = src_p = path;
  while (*src_p != EOS) 
  { 
    while (*src_p == '/') 
      src_p++;
    *dest_p++ = '/';
    if (src_p[0] == '.' && src_p[1] == '.' 
	&& (src_p[2] == '/' || src_p[2] == EOS)) 
    { 
      /* Walk up only if we are not on root level. */
      src_p += 2;
      if (dest_p > path + 1) 
	dest_p -= 2;
      while (*dest_p != '/') 
	dest_p--;
    } 
    else if (src_p[0] == '.' && (src_p[1] == '/' || src_p[1] == EOS)) 
    { 
      src_p++;
      dest_p--;
    } 
    else 
    { 
      while (*src_p != '/' && *src_p != EOS) 
	*dest_p++ = *src_p++;
    }
  }
  if (dest_p > path + 1 && dest_p[-1] == '/') 
    dest_p--;
  *dest_p = EOS;
#endif

#ifdef WIN32
  /* The first two chars is the drive specification. */
  dest_p = src_p = path + 2;

  while (*src_p != EOS) 
  { 
    while (*src_p == '\\' || *src_p == '/') 
      src_p++;
    *dest_p++ = '\\';
    if (src_p[0] == '.' && src_p[1] == '.' 
        && (src_p[2] == '\\' || src_p[2] == '/' || src_p[2] == EOS)) 
    { 
      /* Walk up only if we are not on root level. */
      src_p += 2;
      if (dest_p > path + 3) 
	dest_p -= 2;
      while (*dest_p != '\\') 
	dest_p--;
    } 
    else if (src_p[0] == '.' 
	     && (src_p[1] == '\\' || src_p[1] == '/' || src_p[1] == EOS)) 
    { 
      src_p++;
      dest_p--;
    } 
    else 
    { 
      while (*src_p != '\\' && *src_p != '/' && *src_p != EOS) 
	*dest_p++ = *src_p++;
    }
  }
  if (dest_p > path + 3 && dest_p[-1] == '\\') 
    dest_p--;
  *dest_p = EOS;
#endif
}

/*---------------------------------------------------------------------------*/

char_t * 
absolute_path( string_t src_path, string_t relative_to )
/* Return the absolute path name which is equivalent to SRC_PATH.
 * If SRC_PATH starts with "~", it's replaced by the home directory of the
 * user whose login name is following (current user if no login name).
 * If RELATIVE_TO is not NULL, SRC_NAME is relative to that path name.
 * RELATIVE_TO must be an absolute path name (a directory or a file).
 * The returned path must be freed after use. */
{ 
#ifdef POSIX
  text_t *path;
  string_t src_path_p, login;
  char_t *dest_path;
  struct passwd *password;
  string_t relative_dir;
  char_t current_dir[ MAX_PATH_SIZE ];

  path = new_text();

  /* Put a home directory in front. */
  src_path_p = src_path;
  if (*src_path_p == '~') 
  { 
    /* Put a users home directory in front. */
    src_path_p++;
    string_t login_p = src_path_p;
    while (*src_path_p != '/' && *src_path_p != EOS) 
      src_path_p++;
    if (src_path_p == login_p) 
      add_to_text( path, get_env( "HOME" ) );
    else 
    { 
      /* Put home directory of user LOGIN in front. */
      login = new_string( login_p, src_path_p );
      password = getpwnam( login );
      add_to_text( path, password->pw_dir );
      free_mem( &login );
    }
  } 
  else if (*src_path_p != '/') 
  { 
    if (relative_to != NULL) 
    { 
      /* Put RELATIVE_TO ahead (strip last name). */
      string_t relative_end = relative_to + strlen( relative_to );
      while (relative_end[-1] != '/') 
	relative_end--;
      relative_dir = new_string( relative_to, relative_end );
      add_to_text( path, relative_dir );
      free_mem( &relative_dir );
    } 
    else if (getcwd( current_dir, MAX_PATH_SIZE ) != NULL)
    { 
      /* Put current directory in front. */
      add_to_text( path, current_dir );
    }
  }
  
  /* Copy rest of DEST_PATH, clean it up and return it. */
  add_char_to_text( path, '/' );
  add_to_text( path, src_path_p );

  dest_path = text_to_string( &path );
  tidy_path( dest_path );
  return dest_path;
#endif

#ifdef WIN32
  text_t *path;
  string_t src_path_p;		
  string_t relative_dir;
  char_t current_dir[ MAX_PATH_SIZE ];

  path = new_text();
  src_path_p = src_path;
  if (src_path_p[0] == '~' && (src_path_p[1] == '\\' || src_path_p[1] == '/'))
  {
    /* Put the users home directory in front. */
    src_path_p += 2;
    relative_to = getenv( "USERPROFILE" );
    if (relative_to == NULL) 
      relative_to = get_env( "SYSTEMDRIVE" );
    add_to_text( path, relative_to );
    add_char_to_text( path, '\\' );
  }
  else if (IS_LETTER( src_path_p[0] ) && src_path_p[1] == ':')
  {
    /* The path is already complete. */
  }
  else if (src_path_p[0] == '\\' || src_path_p[0] == '/')
  {
    /* Put the current drive in front. */
    src_path_p++;
    if (relative_to != NULL)
    {
      add_char_to_text( path, relative_to[0] );
      add_char_to_text( path, relative_to[1] );
    }
    else
    {
      GetCurrentDirectory( MAX_PATH_SIZE, current_dir );
      add_char_to_text( path, current_dir[0] );
      add_char_to_text( path, current_dir[1] );
    }  
    add_char_to_text( path, '\\' );
  }
  else if (relative_to != NULL) 
  { 
    /* Put RELATIVE_TO ahead (strip last name). */
    string_t relative_end = relative_to + strlen( relative_to );
    while (relative_end[-1] != '\\') 
      relative_end--;
    relative_dir = new_string( relative_to, relative_end );
    add_to_text( path, relative_dir );
    add_char_to_text( path, '\\' );
    free_mem( &relative_dir );
  }
  else 
  { 
    /* Put current directory in front. */
    GetCurrentDirectory( MAX_PATH_SIZE, current_dir );
    add_to_text( path, current_dir );
    add_char_to_text( path, '\\' );
  }
  
  add_to_text( path, src_path_p );
  char * dest_path = text_to_string( &path );
  tidy_path( dest_path );
  return dest_path;
#endif
}

/*---------------------------------------------------------------------------*/

static string_t extension_start( string_t name )
/* Return a pointer to the start (the dot) of the extension in NAME,
 * or to the end of the string if there is no extension. */
{
  string_t s, t;
  
  s = NULL;
  for (t = name; *t != EOS; t++)
  {
    if (*t == '/') 
      s = NULL;
#ifdef WIN32
    else if (*t == '\\') 
      s = NULL;
#endif
    else if (*t == '.') 
      s = t;
  }
  return (s != NULL ? s : t);
}

/*---------------------------------------------------------------------------*/

bool 
has_extension( string_t file_name, string_t extension )
/* Test if FILE_NAME has extension EXTENSION. */
{
  string_t ext; /* The real extension of FILE_NAME (including "."). */

  ext = extension_start( file_name );
  return (*ext != EOS && strcmp( ext + 1, extension ) == 0);
}

/*---------------------------------------------------------------------------*/

char_t *
replace_extension( string_t file_name, string_t extension )
/* Return a new string that contains FILE_NAME with new EXTENSION. 
 * The string must be freed after use. */
{
  string_t base;
  char_t *s;

  base = new_string( file_name, extension_start( file_name ) );
  s = concat_strings( base, ".", extension, NULL );
  free_mem( &base );
  return s;
}

/*---------------------------------------------------------------------------*/

void
set_file_name( string_t *file_name_p, string_t file_name )
/* Set *FILE_NAME_P to absolute path FILE_NAME, relative to current dir.
 * Print an error if *FILE_NAME_P is already set.
 * The created file name must be freed after use. */
{ 
  *file_name_p = absolute_path( file_name, NULL );
}

/*---------------------------------------------------------------------------*/

void
set_binary_file_name( string_t *file_name_p, string_t file_name )
/* Set *FILE_NAME_P to
 * FILE_NAME plus "_l" for little endian, "_b" for big endian, "_c" else,
 * converted to absolute path. 
 * Print an error if *FILE_NAME_P is already set.
 * The created file name must be freed after use. */
{
  union { char_t chars[4]; int_t integer; } format;
  string_t suffix, binary_file_name;

  format.integer = 0x12345678;
  if (sizeof( int_t ) != 4) 
    suffix = "_c";
  else if (format.chars[0] == 0x12 && format.chars[1] == 0x34
	   && format.chars[2] == 0x56 && format.chars[3] == 0x78)
  {
    suffix = "_b";
  }
  else if (format.chars[0] == 0x78 && format.chars[1] == 0x56
	   && format.chars[2] == 0x34 && format.chars[3] == 0x12)
  {
     suffix = "_l";
  }
  else 
    suffix = "_c";

  binary_file_name = concat_strings( file_name, suffix, NULL );
  *file_name_p = absolute_path( binary_file_name, NULL );
  free_mem( &binary_file_name );
}

}}}

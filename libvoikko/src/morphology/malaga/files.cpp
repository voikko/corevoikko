/* Copyright (C) 1995 Bjoern Beutel.
 *               2009 Harri Pitkänen <hatapitk@iki.fi>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *********************************************************************************/

/* Description. =============================================================*/

/* Operations for files and file names. */

/* Includes. ================================================================*/

// XXX: what do we actually want here?
#include "porting.h"
#ifdef HAVE_GETPWUID_R
#define POSIX
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
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

FILE *
open_stream( string_t file_name, string_t stream_mode )
/* Open file FILE_NAME and create a stream from/to it in mode STREAM_MODE.
 * Works like "fopen", but calls "error" if it doesn't work. */
{ 
  return fopen( file_name, stream_mode );
} 

/*---------------------------------------------------------------------------*/

void 
close_stream(FILE **stream_p)
/* Close the stream *STREAM_P
 * and set *STREAM_P to NULL. Don't do anything if *STREAM_P == NULL.
 */
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
             FILE *stream)
/* Read ITEM_COUNT items, of size ITEM_SIZE each, from STREAM,
 * and store them at *ADDRESS.
 */
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
                 FILE *stream)
/* Read ITEM_COUNT items, of size ITEM_SIZE each, from STREAM
 * into allocated memory block,
 * and return a pointer to that block.
 * The block must be freed after use. */
{ 
  void *block;

  block = new_vector( item_size, item_count );
  read_vector(block, item_size, item_count, stream);
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


}}}

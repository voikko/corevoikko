/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module defines a new data type, "pool_t", for growing vectors of items
 * of an arbitrary type.

 * A pool is implemented as a linked list of chunks each of which is of fixed
 * size once allocated. Individual subvectors of items may be allocated at
 * once, and it is guaranteed that these subvectors are contiguous (like
 * arrays) .*/

/* Includes. ================================================================*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include "basic.h"
#include "files.h"
#include "pools.h"

/* Constants. ===============================================================*/

enum {MIN_CHUNK_SIZE = 400};
/* The minimum size of the data area in a chunk. */

/* Macros. ==================================================================*/

#define CHUNK_DATA(chunk_p) ((u_byte_t *) ((chunk_p) + 1))
/* Use this macro to access the data in a chunk. */

/* Types. ===================================================================*/

typedef struct /* A block of memory that is part of a pool. */
{ 
  list_node_t *next;
  int_t chunk_size; /* The maximum number of items in this chunk. */
  int_t item_count; /* The actual number of items in this chunk. */
  /* For 64-bit pointers, we are 8-byte aligned here.
   * For 32-bit pointers, we are 4-byte aligned here. */
  /* Items follow here. */
} chunk_t;

/* The pool implementation type.
 * (Use "new_pool" before you use any other functions on a pool variable.) */
struct pool
{ 
  int_t item_size; /* Size of the items that are stored in this pool. */
  int_t item_count; /* The overall number of items stored in this pool. */
  int_t chunk_size; /* The size of new chunks. */
  list_t chunk_list; /* The chunks of this pool. */
};

/* Functions. ===============================================================*/

pool_t 
new_pool( int_t item_size )
/* Create a new pool that records items of size ITEM_SIZE. */
{
  pool_t pool;

  pool = (pool_t) new_mem( sizeof( struct pool ) );
  pool->item_size = item_size;
  pool->item_count = 0;
  pool->chunk_size = MIN_CHUNK_SIZE / item_size;
  clear_list( &pool->chunk_list );
  return pool;
}

/*---------------------------------------------------------------------------*/

void 
clear_pool( pool_t pool )
/* Clear POOL. Do not free any memory used by the pool. */
{
  /* Free all chunks of this pool. */
  while (pool->chunk_list.first != NULL) 
    free_first_node( &pool->chunk_list );
  pool->item_count = 0;
}

/*---------------------------------------------------------------------------*/

void *
pool_to_vector( pool_t pool )
/* Return pool as a C vector (contiguous memory).
 * The vector must be freed after use. */
{
  void *vector;
  u_byte_t *vector_p; /* Position where next chunk is to be copied. */
  chunk_t *chunk;
  
  /* Collect all chunks in a new vector. */
  vector = new_vector( pool->item_size, pool->item_count );
  vector_p = (u_byte_t *) vector;
  FOREACH( chunk, pool->chunk_list, chunk_t ) 
  { 
    memcpy( vector_p, CHUNK_DATA( chunk ), 
	    pool->item_size * chunk->item_count );
    vector_p += pool->item_size * chunk->item_count;
  }
  return vector;
}

/*---------------------------------------------------------------------------*/

void *
get_pool_space( pool_t pool, int_t item_count, int_t *index )
/* Get space for ITEM_COUNT contiguous items in POOL.
 * Return its address as the function's result.
 * Return its index in *INDEX, if INDEX != NULL. */
{
  void *new_p; /* Pointer to the pool space. */
  chunk_t *chunk; /* Address of a chunk pointer. */

  /* Check if there is enough space in the last chunk. */
  chunk = (chunk_t *) pool->chunk_list.last;
  if (chunk == NULL || chunk->item_count + item_count > chunk->chunk_size) 
  { 
    if (pool->chunk_size < item_count) 
      pool->chunk_size = item_count;
    chunk = (chunk_t *) new_node( &pool->chunk_list,
		      sizeof( chunk_t ) + pool->item_size * pool->chunk_size,
		      LIST_END );
    chunk->chunk_size = pool->chunk_size;
    chunk->item_count = 0;
  }

  /* Remember address and index of current position in pool. */
  new_p = (void *) (CHUNK_DATA( chunk ) + pool->item_size * chunk->item_count);
  if (index != NULL) 
    *index = pool->item_count;

  /* Adjust indices. */
  chunk->item_count += item_count;
  pool->item_count += item_count;
 
  return new_p;
}

/*---------------------------------------------------------------------------*/

int_t 
pool_item_count( pool_t pool )
/* Return the number of the items in POOL. */
{
  return pool->item_count;
}

/*---------------------------------------------------------------------------*/

void *
pool_item( pool_t pool, int_t index )
/* Return the address of item with INDEX in pool POOL,
 * or NULL if there is no such item. */
{
  chunk_t *chunk;

  if (index < 0 || index >= pool->item_count) 
    return NULL;

  /* Find the right chunk. */
  chunk = (chunk_t *) pool->chunk_list.first;
  while (index >= chunk->item_count) 
  { 
    index -= chunk->item_count;
    chunk = (chunk_t *) chunk->next;
  }

  /* Return the address of the item. */
  return CHUNK_DATA( chunk ) + pool->item_size * index;
}

/*---------------------------------------------------------------------------*/

void 
free_pool( pool_t *pool )
/* Free all memory used by *POOL. */
{
  if (*pool == NULL) 
    return;
  clear_pool( *pool );
  free_mem( pool );
}

/* End of file. =============================================================*/

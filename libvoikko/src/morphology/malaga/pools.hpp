/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module defines a new data type, "pool_t", for growing vectors of items 
 * of an arbitrary type. */

namespace libvoikko { namespace morphology { namespace malaga {

/* Types. ===================================================================*/

typedef struct pool *pool_t; /* The abstract data type. */

/* Functions. ===============================================================*/

extern pool_t new_pool( int_t item_size );
/* Create a new pool that records items of size ITEM_SIZE. */

extern void free_pool( pool_t *pool );
/* Free all memory used by *POOL. */

extern void clear_pool( pool_t pool );
/* Clear POOL. */

extern void *pool_to_vector( pool_t pool );
/* Return POOL as a C vector (contiguous memory).
 * The vector must be freed after use. */

extern void *get_pool_space( pool_t pool, int_t item_count, int_t *index );
/* Get space for ITEM_COUNT contiguous items in POOL.
 * Return its address as the function's result. 
 * Return its index in *INDEX, if INDEX != NULL. */

extern int_t pool_item_count( pool_t pool );
/* Return the number of the items in POOL. */

extern void *pool_item( pool_t pool, int_t index );
/* Return the address of item with INDEX in pool POOL,
 * or NULL if there is no such item. */

}}}

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

/* This module defines a new data type, "pool_t", for growing vectors of items 
 * of an arbitrary type. */

#ifndef LIBVOIKKO_MORPHOLOGY_MALAGA_POOLS_HPP
#define LIBVOIKKO_MORPHOLOGY_MALAGA_POOLS_HPP

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

extern void *get_pool_space( pool_t pool, int_t item_count, int_t *index );
/* Get space for ITEM_COUNT contiguous items in POOL.
 * Return its address as the function's result. 
 * Return its index in *INDEX, if INDEX != NULL. */

}}}

#endif

/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2010 Harri Pitkänen <hatapitk@iki.fi>
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

package org.puimula.libvoikko;

import java.util.HashMap;

/**
 * A result from morphological analysis. One analysis is provided
 * for each possible reading of the word. An analysis is a map
 * of attribute-value pairs. For more information about provided
 * attributes see doc/morphological-analysis.txt within libvoikko
 * sources.
 */
public class Analysis extends HashMap<String, String> {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    

}

/**
 * Copyright (c) 2002-2014 "Neo Technology,"
 * Network Engine for Objects in Lund AB [http://neotechnology.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.kernel.impl.store.counts.keys;

import org.neo4j.kernel.impl.api.CountsVisitor;

public final class IndexCountsKey extends IndexKey
{
    IndexCountsKey( int labelId, int propertyKeyId )
    {
        super( labelId, propertyKeyId, CountsKeyType.INDEX_COUNTS );
    }

    @Override
    public void accept( CountsVisitor visitor, long updates, long size )
    {
        visitor.visitIndexCounts( labelId(), propertyKeyId(), updates, size );
    }

    @Override
    public int compareTo( CountsKey other )
    {
        if ( other instanceof org.neo4j.kernel.impl.store.counts.keys.IndexCountsKey )
        {
            org.neo4j.kernel.impl.store.counts.keys.IndexCountsKey
                    that = (org.neo4j.kernel.impl.store.counts.keys.IndexCountsKey) other;
            int cmp = this.labelId() - that.labelId();
            if ( cmp == 0 )
            {
                cmp = this.propertyKeyId() - that.propertyKeyId();
            }
            return cmp;
        }
        else
        {
            return recordType().ordinal() - other.recordType().ordinal();
        }
    }
}

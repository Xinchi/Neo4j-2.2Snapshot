/**
 * Copyright (c) 2002-2014 "Neo Technology,"
 * Network Engine for Objects in Lund AB [http://neotechnology.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.kernel.ha.cluster.member;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import org.neo4j.cluster.InstanceId;
import org.neo4j.cluster.member.ClusterMemberEvents;
import org.neo4j.cluster.member.ClusterMemberListener;
import org.neo4j.cluster.protocol.cluster.Cluster;
import org.neo4j.cluster.protocol.cluster.ClusterConfiguration;
import org.neo4j.cluster.protocol.cluster.ClusterListener;
import org.neo4j.cluster.protocol.heartbeat.Heartbeat;
import org.neo4j.cluster.protocol.heartbeat.HeartbeatListener;
import org.neo4j.helpers.Predicate;
import org.neo4j.kernel.ha.cluster.HighAvailabilityModeSwitcher;
import org.neo4j.kernel.impl.store.StoreId;
import org.neo4j.kernel.impl.util.CopyOnWriteHashMap;
import org.neo4j.kernel.impl.util.StringLogger;
import org.neo4j.kernel.logging.Logging;

/**
 * Keeps an up to date list of members, their roles and availability for
 * display for example in JMX.
 */
public class ClusterMembers
{
    public static final Predicate<ClusterMember> ALIVE = new Predicate<ClusterMember>()
    {
        @Override
        public boolean accept( ClusterMember item )
        {
            return item.isAlive();
        }
    };

    private final InstanceId me;
    private final StringLogger log;

    public static Predicate<ClusterMember> inRole( final String role )
    {
        return new Predicate<ClusterMember>()
        {
            @Override
            public boolean accept( ClusterMember item )
            {
                return item.hasRole( role );
            }
        };
    }

    private final Map<InstanceId, ClusterMember> members = new CopyOnWriteHashMap<>();

    public ClusterMembers( Cluster cluster, Heartbeat heartbeat, ClusterMemberEvents events, InstanceId me,
            Logging logging )
    {
        this.me = me;
        this.log = logging.getMessagesLog( getClass() );
        cluster.addClusterListener( new HAMClusterListener() );
        heartbeat.addHeartbeatListener( new HAMHeartbeatListener() );
        events.addClusterMemberListener( new HAMClusterMemberListener() );
    }

    public Iterable<ClusterMember> getMembers()
    {
        return members.values();
    }

    public ClusterMember getSelf()
    {
        for ( ClusterMember clusterMember : getMembers() )
        {
            if ( clusterMember.getInstanceId().equals( me ) )
            {
                return clusterMember;
            }
        }
        return null;
    }

    public synchronized void waitForEvent( long timeout ) throws InterruptedException
    {
        wait( timeout );
    }

    private synchronized void eventOccurred()
    {
        notifyAll();
    }

    private ClusterMember getMember( InstanceId server )
    {
        ClusterMember clusterMember = members.get( server );
        if ( clusterMember == null )
        {
            throw new IllegalStateException( "Member " + server + " not found in " + new HashMap<>( members ) );
        }
        return clusterMember;
    }

    private class HAMClusterListener extends ClusterListener.Adapter
    {
        @Override
        public void enteredCluster( ClusterConfiguration configuration )
        {
            String oldMembers = members.values().toString();

            Map<InstanceId, ClusterMember> newMembers = new HashMap<>();
            for ( InstanceId memberClusterId : configuration.getMemberIds() )
            {
                newMembers.put( memberClusterId, new ClusterMember( memberClusterId, true ) );
            }
            members.clear();
            members.putAll( newMembers );

            log.debug( "Entered cluster. Configuration: " + configuration + ". OldMembers: " + oldMembers + ", " +
                       "NewMembers: " + members.values() );
        }

        @Override
        public void leftCluster()
        {
            String oldMembers = members.values().toString();

            members.clear();

            log.debug( "Left cluster. OldMembers: " + oldMembers + ". NewMembers: " + members.values() );
        }

        @Override
        public void joinedCluster( InstanceId member, URI memberUri )
        {
            String oldMembers = members.values().toString();

            members.put( member, new ClusterMember( member ) );

            log.debug( "Joined cluster. Instance: " + member + ", URI: " + memberUri + ". OldMembers: " + oldMembers +
                       ". NewMembers: " + members.values() );
        }

        @Override
        public void leftCluster( InstanceId instanceId, URI member )
        {
            String oldMembers = members.values().toString();

            members.remove( instanceId );

            log.debug( "Left cluster. Instance: " + instanceId + ", URI: " + member + ". OldMembers: " + oldMembers +
                       ". NewMembers: " + members.values() );
        }
    }

    private class HAMClusterMemberListener extends ClusterMemberListener.Adapter
    {
        private InstanceId masterId = null;

        @Override
        public void coordinatorIsElected( InstanceId coordinatorId )
        {
            String oldMembers = members.values().toString();

            if ( coordinatorId.equals( this.masterId ) )
            {
                return;
            }
            this.masterId = coordinatorId;
            Map<InstanceId, ClusterMember> newMembers = new HashMap<>();
            for ( Map.Entry<InstanceId, ClusterMember> memberEntry : members.entrySet() )
            {
                newMembers.put( memberEntry.getKey(), memberEntry.getValue().unavailableAs(
                        HighAvailabilityModeSwitcher.MASTER ).unavailableAs( HighAvailabilityModeSwitcher.SLAVE ) );
            }
            members.clear();
            members.putAll( newMembers );

            log.debug( "Coordinator is elected. Instance: " + coordinatorId + ". OldMembers: " + oldMembers + ". " +
                       "NewMembers: " + members.values() );
        }

        @Override
        public void memberIsAvailable( String role, InstanceId instanceId, URI roleUri, StoreId storeId )
        {
            String oldMembers = members.values().toString();

            members.put( instanceId, getMember( instanceId ).availableAs( role, roleUri, storeId ) );
            eventOccurred();

            log.debug( "Member is available. Role: " + role + ", Instance: " + instanceId + ". OldMembers: " +
                       oldMembers + ". NewMembers: " + members.values() );
        }

        @Override
        public void memberIsUnavailable( String role, InstanceId unavailableId )
        {
            String oldMembers = members.values().toString();

            ClusterMember member;
            try
            {
                member = getMember( unavailableId );
                members.put( unavailableId, member.unavailableAs( role ) );
            }
            catch ( IllegalStateException e )
            {
                // Unknown member
            }

            log.debug( "Member is available. Instance: " + unavailableId + ", Role: " + role + ". OldMembers: " +
                       oldMembers + ". NewMembers: " + members.values() );
        }

        @Override
        public void memberIsFailed( InstanceId instanceId )
        {
            String oldMembers = members.values().toString();

            // Make it unavailable for all its current roles
            ClusterMember member = getMember( instanceId );
            for ( String role : member.getRoles() )
            {
                member = member.unavailableAs( role ); // ClusterMember is copy-on-write
            }
            members.put( instanceId, member ); // replace with the new copy

            log.debug( "Member is failed. Instance: " + instanceId + ". OldMembers: " + oldMembers + ". NewMembers: " +
                       members.values() );
        }
    }

    private class HAMHeartbeatListener extends HeartbeatListener.Adapter
    {
        @Override
        public void failed( InstanceId server )
        {
            String oldMembers = members.values().toString();

            if ( members.containsKey( server ) )
            {
                members.put( server, getMember( server ).failed() );
            }

            log.debug( "Failed. Instance: " + server + ". OldMembers: " + oldMembers + ". NewMembers: " +
                       members.values() );
        }

        @Override
        public void alive( InstanceId server )
        {
            String oldMembers = members.values().toString();

            if ( members.containsKey( server ) )
            {
                members.put( server, getMember( server ).alive() );
            }

            log.debug( "Alive. Instance: " + server + ". OldMembers: " + oldMembers + ". NewMembers: " +
                       members.values() );
        }
    }
}

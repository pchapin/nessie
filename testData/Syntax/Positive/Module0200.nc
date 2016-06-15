
module NeighborManagerC {
    provides interface NeighborManagement;
}
implementation {
    
    // Neighbor zero is 'self' (the current node).
    int neighbor_mapping[MAX_NEIGHBORS + 1] = { NODE_ID };
    int count = 0;
    
    // Assign a neighbor number to a (possibly) new node ID value.
    command error_t NeighborManagement.allocate_neighbor_number( int node_id )
    {
        int index;
        
        // Is this node already a known neighbor?
        for( index = 0; index <= count; ++index ) {
            if( neighbor_map[index] == node_id ) return SUCCESS;
        }
    
        // If not, add it to the map if there is space.
        if( index > MAX_NEIGHBORS) return FAIL;
        neighbor_map[index] = node_id;
        return SUCCESS;
    }
    
    // Look up the neighbor number associated with the given node ID value.
    command int NeighborManagement.get_neighbor_number( int node_id )
    {
        int index;
        
        // Is this node a known neighbor?
        for( index = 0; index <= count; ++index ) {
            if( neighbor_map[index] == node_id ) return index;
        }
        
        // Is there a better way to handle the error?
        return 0;
    }
    
    
    // Look up the node ID value associated with the given neighbor number.
    command int NeighborManagement.get_node_id( int neighbor_number )
    {
        // Is there a better way to handle the error?
        if( neighbor_number > count ) return 0;
        return neighbor_map[neighbor_number];
    }
    
    
    // Returns the number of (known) neighbors.
    command int NeighborManagement.neighbor_count( )
    {
        return count;
    }

}


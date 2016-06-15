# 1 "./RemoteSelectorC.nc"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "./RemoteSelectorC.nc"

# 1 "./ComponentManager.h" 1

// The parser does not distinguish between structure tags and typedef names. After the typedefs
// below the parser will not allow "struct component_id" (for example) because it regards the
// name "component_id" as the name of a type.

typedef struct component_id {
   uint16_t node_id;
   uint8_t local_id;
} component_id;

typedef struct componet_set {
    component_id *ids;
    int count;
} component_set;
# 3 "./RemoteSelectorC.nc" 2

module RemoteSelectorC {
    provides interface ComponentManager;
    # 3 "SomeFile.nc"  // Are line directives properly ignored here?
}
implementation {
    // See Module0240.nc about why this is commented out.
    //# 4 "SomeFile.nc"  // Are line directives properly ignored here?
    component_id broadcast = { 0xFFFF, 0x01 };
    # 5 "SomeFile.nc"  // Are line directives properly ignored here?

    command component_set ComponentManager.elements()
    {
        // Line directives as statements are no longer supported.
        // See the comment at 'statement' in nesC.g
        //
        //# 6 "SomeFile.nc"
        component_set result = { &broadcast, 1 };
        //# 7 "SomeFile.nc"
        return result;
    }
}

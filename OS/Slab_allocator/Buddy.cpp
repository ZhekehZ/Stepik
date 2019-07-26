#include <memory>
#include "Buddy.h"
/* (A buddy allocator should be here). */

void *alloc_slab(int order) {
	size_t size = PAGE_SIZE*(1 << order);
#ifdef _WIN32
    return _aligned_malloc(size, size);
#elif __linux__
    return aligned_alloc(size, size);
#endif
}

void free_slab(void *slab) {
#ifdef _WIN32
    _aligned_free(slab);
#elif __linux__
    free(slab);
#endif
}

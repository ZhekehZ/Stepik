#ifndef BUDDY_ALLOCATOR
#define BUDDY_ALLOCATOR

const size_t PAGE_SIZE = 4096;
void *alloc_slab(int order);
void free_slab(void *slab);

#endif // BUDDY_ALLOCATOR
#ifndef SLAB_ALLOCATOR
#define SLAB_ALLOCATOR

struct slab_meta;
struct cache {
	slab_meta *list_empty, *list_busy, *list_full;

	size_t block_size;
	size_t num_blocks;
	int slab_order; 
};

void cache_setup(struct cache *cache, size_t block_size);
void *cache_alloc(struct cache *c);
void cache_free(struct cache *cache, void *ptr);
void cache_shrink(struct cache *cache);

#endif // SLAB_ALLOCATOR
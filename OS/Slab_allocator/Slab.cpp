#include <cstdlib>
#include "Slab.h"
#include "Buddy.h"

struct slab_meta {
	char *free_block;
	char *not_explored;
	char *end_slab;
	size_t num_free, num_blocks;
	slab_meta *next_slab;
	slab_meta *prev_slab;

	static slab_meta* init(void *mem, size_t num_blocks, size_t block_size) {
		if (mem == nullptr) return nullptr;
		if (num_blocks < 1) return nullptr;
		if (block_size < 2 * sizeof(char**))
			block_size = 2 * sizeof(char**);
		char * slab_start = ((char *)mem) + sizeof(slab_meta);
		slab_meta *s = (slab_meta*) mem;
		s->end_slab = slab_start + num_blocks * block_size;
		s->not_explored = slab_start;
		s->free_block = nullptr;
		s->next_slab = nullptr;
		s->prev_slab = nullptr;
		s->num_free = num_blocks;
		s->num_blocks = num_blocks;
		return s;
	}

	bool is_full() {
		return num_free == 0;
	}

	bool is_empty() {
		return num_blocks == num_free;
	}

	void *alloc_block() {
		if (free_block == nullptr) {
			if (not_explored < end_slab) {
				size_t block_size = (end_slab - not_explored) / num_free;
				void *block = (void*)not_explored;
				not_explored += block_size;
				num_free--;
				return block;
			}
			else return nullptr;
		}
		else {
			void *block = (void*)free_block;

			char **next_block = (char**)free_block;
			char **prev_block = (char**)(free_block + sizeof(char*));

			if (*next_block == free_block) {
				free_block = nullptr;
			}
			else {
				char **prev_next_block = (char**)(*prev_block);
				char **next_prev_block = (char**)((*next_block) + sizeof(char*));
				*prev_next_block = *next_block;
				*next_prev_block = *prev_block;
				free_block = *next_block;
			}

			num_free--;
			return block;
		}
	}

	void release_block(void* block) {
		if (block == nullptr) return;
		char **block_next = (char**)block;
		char **block_prev = block_next + 1;
		if (free_block == nullptr) {
			*block_next = (char*)block;
			*block_prev = (char*)block;
		}
		else {
			char **prev_next_block = (char**)free_block;
			char **next_prev_block = ((char**)(*prev_next_block)) + 1;
			*block_prev = free_block;
			*block_next = *prev_next_block;
			*prev_next_block = (char*)block;
			*next_prev_block = (char*)block;
		}
		free_block = (char *)block;
		num_free++;
	}
};

void insert_slab(slab_meta **list, slab_meta *slab) {
	if (slab == nullptr) return;
	if (*list == nullptr) {
		slab->next_slab = slab;
		slab->prev_slab = slab;
		*list = slab;
	}
	else {
		slab_meta *slab_head = *list;
		slab_meta *slab_next = slab_head->next_slab;
		slab->next_slab = slab_next;
		slab->prev_slab = slab_head;
		slab_head->next_slab = slab;
		slab_next->prev_slab = slab;
		*list = slab;
	}
}

slab_meta *pop_head_slab(slab_meta **list) {
	if (*list == nullptr) return nullptr;

	slab_meta *slab_head = *list;
	if (slab_head->next_slab == slab_head) {
		*list = nullptr;
	}
	else {
		slab_meta *slab_prev = slab_head->prev_slab;
		slab_meta *slab_next = slab_head->next_slab;
		slab_prev->next_slab = slab_next;
		slab_next->prev_slab = slab_prev;
		*list = slab_next;
	}

	return slab_head;
}

slab_meta *erase_slab(slab_meta **list, slab_meta *slab) {
	if (slab == nullptr) return nullptr;
	if (*list == nullptr) return nullptr;
	*list = slab;
	return pop_head_slab(list);
}

slab_meta * expand_slab_list(slab_meta **list, cache *c) {
	if (c == nullptr) return nullptr;
	void * raw_slab = alloc_slab(c->slab_order);
	if (raw_slab == nullptr) return nullptr;
	slab_meta *slab = slab_meta::init(raw_slab, c->num_blocks, c->block_size);
	insert_slab(list, slab);
	return slab;
}

void remove_head_slab(slab_meta **list) {
	slab_meta *slab = pop_head_slab(list);
	free_slab((void*)slab);
}

void cache_setup(struct cache *cache, size_t block_size){
	if (cache == nullptr) return;
	cache->list_empty = nullptr;
	cache->list_busy = nullptr;
	cache->list_full = nullptr;

	cache->block_size = block_size;
	cache->slab_order = 5;
	cache->num_blocks = (PAGE_SIZE * (1 << cache->slab_order)) / cache->block_size;
}

void cache_release(struct cache *cache) {
	while (cache->list_full)
		remove_head_slab(&cache->list_full);
	while (cache->list_busy)
		remove_head_slab(&cache->list_busy);
	while (cache->list_empty)
		remove_head_slab(&cache->list_empty);
}

void *cache_alloc(struct cache *c) {
	if (c == nullptr) return nullptr;
	if (c->list_busy == nullptr) {
		if (c->list_empty == nullptr) {
			slab_meta *slab = expand_slab_list(&c->list_busy, c);
			return slab? slab->alloc_block(): nullptr;
		}
		else {
			slab_meta *slab = pop_head_slab(&c->list_empty);
			insert_slab(&c->list_busy, slab);
			return slab ? slab->alloc_block() : nullptr;
		}
	}
	else {
		slab_meta *slab = c->list_busy;
		if (slab == nullptr) return nullptr;
		void *data = slab->alloc_block();
		if (slab->is_full()) {
			slab = pop_head_slab(&c->list_busy);
			insert_slab(&c->list_full, slab);
		}
		return data;
	}
}

slab_meta *get_slab(void *ptr, cache* c){
	if (ptr == nullptr || c == nullptr) return nullptr;
	size_t bitptr = (size_t) ptr;
	size_t slab_size = PAGE_SIZE * (1 << c->slab_order);
	bitptr = bitptr & ~(slab_size - 1);
	return (slab_meta*)bitptr;
}

void cache_free(struct cache *cache, void *ptr) {
	if (ptr == nullptr || cache == nullptr) return;
	slab_meta *slab = get_slab(ptr, cache);
	if (slab->is_full()) {
		slab = erase_slab(&cache->list_full, slab);
		slab->release_block(ptr);
		insert_slab(&cache->list_busy, slab);
	}
	else {
		slab->release_block(ptr);
		if (slab->is_empty()) {
			slab = erase_slab(&cache->list_busy, slab);
			insert_slab(&cache->list_empty, slab);
		}
	}
}

void cache_shrink(struct cache *cache) {
	if (cache == nullptr) return;
	while (cache->list_empty)
		remove_head_slab(&cache->list_empty);
}
#include <iostream>
#include <vector>
#include "Slab.h"

void test() {
	std::vector<void*> ptrs;
	cache c;
	cache_setup(&c, 12345);
	for (int i = 0; i < 10000; i++) {
		ptrs.push_back(cache_alloc(&c));
	}
	for (auto a : ptrs) {
		cache_free(&c, a);
	}
	std::cout << __func__ << " passed.\n";
}

int main()
{
	test();
	return 0;
}
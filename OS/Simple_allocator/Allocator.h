#ifndef MY_ALLOCATOR
#define MY_ALLOCATOR
#include <cstdlib>

void mysetup(void *buf, std::size_t size);
void *myalloc(std::size_t size);
void myfree(void *p);

#endif // MY_ALLOCATOR
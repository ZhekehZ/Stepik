#include "Allocator.h"

int main() {
	#define SZ 156
	int buffer[SZ];
	void* buf = (void*)buffer;

	mysetup(buf, SZ * sizeof(int));
	void *p1 = myalloc(152);
	void *p2 = myalloc(100);
	myfree(p1);
	myfree(p2);
	return 0;
}
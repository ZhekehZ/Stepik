#include <cstdlib>

char *head, *buf_data;
int buf_size;

void mysetup(void *buf, std::size_t size){
	buf_data = (char*)buf;
	buf_size = (int)size;

	int *lm = (int*)buf_data;
	int *rm = (int*)(buf_data + size - sizeof(int));
	char* *lp = (char**)(buf_data + sizeof(int));
	char* *rp = (char**)(buf_data + sizeof(int) + sizeof(char*));
	*lm = *rm = buf_size;
	*lp = *rp = buf_data;

	head = buf_data;
}

void* extract(char* block) {
	int *lm = (int*)block;
	int *rm = (int*)(block + *lm - sizeof(int));
	char* *lp = (char**)(block + sizeof(int));
	char* *rp = (char**)(block + sizeof(int) + sizeof(char*));

	if (*lp == *rp && *lp == block) {
		char* retblock = block;
		head = NULL;
		*lm = *rm = -*lm;
		return (void*)(retblock + sizeof(int));
	}
	else {
		char* *nextleft = (char**)(*rp + sizeof(int));
		char* *prevright = (char**)(*lp + sizeof(int) + sizeof(char*));

		char* retblock = block;
		*nextleft = *lp;
		*prevright = *rp;
		head = *rp;

		*lm = *rm = -*lm;
		return (void*)(retblock + sizeof(int));
	}
};

void *myalloc(std::size_t size){
	if (size < 2 * (sizeof(int) + sizeof(char*)))
		size = 2 * (sizeof(int) + sizeof(char*));

	if (head == NULL)
		return NULL;

	char* start = head;
	int *lm = (int*)head;
	char* *rp = (char**)(head + sizeof(int) + sizeof(char*));
	while (*lm < 2 * sizeof(int) + (int)size) {
		head = *rp;
		if (head == start)
			return NULL;
		lm = (int*)head;
		rp = (char**)(head + sizeof(int) + sizeof(char*));
	}

	if (*lm > size + 4 * sizeof(int) + 2 * sizeof(char*)) {
		int *lm = (int*)head;
		int *rm = (int*)(head + *lm - sizeof(int));

		int busysize = (int)size + 2 * sizeof(int);
		int freesize = *lm - busysize;

		int *newrm = (int*)(head + freesize - sizeof(int));
		*lm = *newrm = freesize;

		int *newlm = (int*)(head + freesize);
		*newlm = *rm = -busysize;
		
		char *data = head + freesize + sizeof(int);
		return (void*)data;
	}
	else {
		return extract(head);
	}
}

void myfree(void *p){
	if (p == NULL) return;
	char *block = (char*)p - sizeof(int);

	int *lm = (int*)block;
	int *rm = (int*)(block - *lm - sizeof(int));
	char* *lp = (char**)(block + sizeof(int));
	char* *rp = (char**)(block + sizeof(int) + sizeof(char*));

	if (head == NULL) {
		*lm = *rm = -*lm;
		head = *lp = *rp = block;
	}
	else {
		char* *prevr = (char**)(head + sizeof(int) + sizeof(char*));
		char* *nextl = (char**)(*prevr + sizeof(int));
		
		*lp = *nextl;
		*rp = *prevr;
		*prevr = *nextl = block;
		*lm = *rm = -*lm;
		head = block;

		bool left = block > buf_data;
		if (left) {
			int *lsz = (int*)(block - sizeof(int));
			left = *lsz > 0;
		}

		if (left) {
			int *lsz = (int*)(block - sizeof(int));
			char *lblock = block - *lsz;
			char *rblock = block;

			extract(rblock);
			int *rlm = (int*)rblock;
			int *rrm = (int*)(rblock - *rlm - sizeof(int));
			int *llm = (int*)lblock;

			int newsize = *llm - *rlm;
			lm = llm;
			*llm = *rrm = newsize;
			block = lblock;
		}

		bool right = block + *lm < buf_data + buf_size;
		if (right) {
			int *rsz = (int*)(block + *lm);
			right = *rsz > 0;
		}

		if (right) {
			int *rsz = (int*)(block + *lm);
			char *lblock = block;
			char *rblock = block + *lm;

			extract(rblock);
			int *rlm = (int*)rblock;
			int *rrm = (int*)(rblock - *rlm - sizeof(int));
			int *llm = (int*)lblock;

			int newsize = *llm - *rlm;
			*llm = *rrm = newsize;
			block = lblock;
		}
	}
}



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
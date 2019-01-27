#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/*Структура, соответствующая контексту, описанному в Switch.S*/
struct switch_frame {
	uint64_t rflags;
	uint64_t r15;
	uint64_t r14;
	uint64_t r13;
	uint64_t r12;
	uint64_t rbp;
	uint64_t rbx;
	uint64_t rip;
} __attribute__((packed)); 
/*
  __attribute__((packed)) - указание для gcc упаковать поля структуры с 
  минимально возможным выравниванием
*/

/*Дескриптор потока*/
struct thread {
	void *context;
};


void switch_threads(struct thread *from, struct thread *to)
{
	void __switch_threads(void **prev, void *next);

	/*
	  Функция переключения описана в Switch.S
	  Принимает указатель на то, куда записать указатель на 
	    сохраненный контекст, и указатель на новый контест
	*/
	__switch_threads(&from->context, to->context);
}


/*Функция создания потока, где entry - основная функция нового потока*/
struct thread *__create_thread(size_t stack_size, void (*entry)(void))
{
	/*После указателя на контекст будем хранить сам контекст*/
	const size_t size = stack_size + sizeof(struct thread);
	struct switch_frame frame; //новый контекст
	struct thread *thread = malloc(size); //выделям место под дескриптор + контекст

	if (!thread)
		return thread; //ошибка! возвращаем NULL

	memset(&frame, 0, sizeof(frame)); //обнуляем "регистры"
	frame.rip = (uint64_t)entry; /*устанавливаем указатель на следующую инструкцию потока,
					единственное поле в контексте, необходимое для запуска
					нового потока*/

	/*Как было сказано, контекст храним сразу после дескриптора*/
	thread->context = (char *)thread + size - sizeof(frame); 
	/*Копируем созданный контекст по адресу, указанному в дескрипторе*/
	memcpy(thread->context, &frame, sizeof(frame));
	/*
	  После предыдущей операции thread указывает на следующую сруктуру:
	  	[context <free_space> rflags r15 r14 r13 r12 rbp rbx rip] 
	  где context указывает на ячейку памяти, с которой начинается rflags, 
	    то есть на вершину стека нового процесса
	*/
	return thread;
}

struct thread *create_thread(void (*entry)(void))
{
	const size_t default_stack_size = 4096;

	return __create_thread(default_stack_size, entry);
}

void destroy_thread(struct thread *thread)
{
	free(thread);
}


static struct thread _thread0; /*ткущий процесс
				rip устанавливать не надо, так как он сам сохранится 
				в случае вызова switch_threads*/
static struct thread *thread[3]; 


static void thread_entry1(void)
{
	printf("In thread1, switching to thread2...\n");
	switch_threads(thread[1], thread[2]);
	printf("Back in thread1, switching to thread2...\n");
	switch_threads(thread[1], thread[2]);
	printf("Back in thread1, switching to thread0...\n");
	switch_threads(thread[1], thread[0]);
}

static void thread_entry2(void)
{
	printf("In thread2, switching to thread1...\n");
	switch_threads(thread[2], thread[1]);
	printf("Back in thread2, switching to thread1...\n");
	switch_threads(thread[2], thread[1]);
}

int main(void)
{
	thread[0] = &_thread0;
	thread[1] = create_thread(&thread_entry1);
	thread[2] = create_thread(&thread_entry2);

	printf("In thread0, switching to thread1...\n");
	switch_threads(thread[0], thread[1]);
	printf("Retunred to thread 0\n");

	destroy_thread(thread[2]);
	destroy_thread(thread[1]);

	return 0;
}
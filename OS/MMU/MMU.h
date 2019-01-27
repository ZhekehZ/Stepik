#ifndef MY_MMU
#define MY_MMU
#include <cstdint>
#include <cstdlib>
#include <unordered_map>

struct MemoryEmulator{
	std::unordered_map<uint64_t, uint64_t> mem;
	uint64_t CR3;

	void init(uint64_t cr3);
	void Write(uint64_t addr, uint64_t value);
	uint64_t Read(uint64_t addr) const;
	std::pair<bool, uint64_t> LogicToPhys(uint64_t addr) const;
};

#endif // MY_MMU
#include "MMU.h"

using namespace std;

struct LogicalAddr {
	uint64_t PML4, DirectoryPtr, Directory, Table, Offset;

	LogicalAddr(uint64_t addr) {
		auto getbits = [](uint64_t x, int from, int to) {
			return x >> from & ~(~0 << (to - from)); 
		};
		PML4 = getbits(addr, 39, 48);
		DirectoryPtr = getbits(addr, 30, 39);
		Directory = getbits(addr, 21, 30);
		Table = getbits(addr, 12, 21);
		Offset = getbits(addr, 0, 12);
	}
};

struct Entry {
	bool P;
	uint64_t PhysicalAddress;

	Entry(uint64_t line) {
		P = line & 1;
		PhysicalAddress = line >> 12 << 12;
	}
};

void MemoryEmulator::init(uint64_t cr3) {
	CR3 = cr3;
};

void MemoryEmulator::Write(uint64_t addr, uint64_t value) {
	mem[addr] = value;
};

uint64_t MemoryEmulator::Read(uint64_t addr) const {
	auto it = mem.find(addr);
	return it == mem.end() ? 0 : it->second;
};

pair<bool, uint64_t> MemoryEmulator::LogicToPhys(uint64_t addr) const {
	LogicalAddr laddr(addr);
	Entry entry3{Read(CR3 + laddr.PML4 * 8) };
	if (!entry3.P)  return{ false, 0 };
	
	Entry entry2{ Read(entry3.PhysicalAddress + laddr.DirectoryPtr * 8) };
	if (!entry2.P)  return{ false, 0 };
	
	Entry entry1{ Read(entry2.PhysicalAddress + laddr.Directory * 8) };
	if (!entry1.P)  return{ false, 0 };
	
	Entry entry{ Read(entry1.PhysicalAddress + laddr.Table * 8) };
	if (!entry.P)  return{ false, 0 };
	return {true, entry.PhysicalAddress + laddr.Offset};
};
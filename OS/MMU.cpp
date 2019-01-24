#include <fstream>
#include <iostream>
#include <sstream>
#include <unordered_map>
#include <string>

using namespace std;

istream& input = ifstream("in.txt");
ostream& output = ofstream("out.txt");

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

struct MEM {
	unordered_map<uint64_t, uint64_t> mem;
	uint64_t CR3;

	void init(uint64_t cr3) {
		CR3 = cr3;
	}

	void Write(uint64_t addr, uint64_t value) {
		mem[addr] = value;
	}

	uint64_t Read(uint64_t addr) const {
		auto it = mem.find(addr);
		return it == mem.end() ? 0 : it->second;
	}

	pair<bool, uint64_t> LogicToPhys(uint64_t addr) const {
		LogicalAddr laddr(addr);
		uint64_t kek;
		Entry entry3{ kek = Read(CR3 + laddr.PML4 * 8) };
		if (!entry3.P)  return{ false, 0 };
		
		Entry entry2{ Read(entry3.PhysicalAddress + laddr.DirectoryPtr * 8) };
		if (!entry2.P)  return{ false, 0 };
		
		Entry entry1{ Read(entry2.PhysicalAddress + laddr.Directory * 8) };
		if (!entry1.P)  return{ false, 0 };
		
		Entry entry{ Read(entry1.PhysicalAddress + laddr.Table * 8) };
		if (!entry.P)  return{ false, 0 };
		return {true, entry.PhysicalAddress + laddr.Offset};
	}
} memory_emulator;

int main() {
	uint64_t q, m, r;
	input >> m >> q >> r;

	memory_emulator.init(r);

	while (m --> 0) {
		uint64_t paddr, value;
		input >> paddr >> value;
		memory_emulator.Write(paddr, value);
	}

	uint64_t f = 0, c = 0;
	while (q --> 0) {
		uint64_t addr;
		input >> addr;
		auto res = memory_emulator.LogicToPhys(addr);
		if (res.first == false)
			output << "fault\n", f++;
		else
			output << res.second << "\n", c++;
	}
	cout << "faults: " << f << "\ncorrect: " << c << "\n";
	return 0;
}

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include "MMU.h"

using namespace std;

ifstream input{"in.txt"};
ofstream output{"out.txt"};
MemoryEmulator memory_emulator;

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
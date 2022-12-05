#include <iostream>
#include <fstream>
#include <limits>

using namespace std;

// VM
// processeur 
//    registres -> lire et �crire no de registres
//    ALU -> calculs / fonctions
// m�moire d'instructions | ram -> lire et �crire (adresses) : tableau

int main()
{
	int16_t registres[8];

	int16_t ram[std::numeric_limits<int16_t>::max()];

	ifstream file;
	file.open("..\\..\\result.asc", ios::in | ios::binary | ios::ate);
	if (!file.is_open())
	{
		std::cout << "Cannot open file\n";
		file.close();
		return 1;
	}
	streampos size = file.tellg(); // size = taille du programme

	file.seekg(0, ios::beg);
	file.read((char*)ram, size); // bytes du programme dans ram

	file.close();

	size_t ip = 0;

	while (true) {
		int16_t opcode = (ram[ip]) >> 13;
		int16_t reg;

		switch (opcode) {
		case 0:
			reg = ram[ip] & 0b111;
			return registres[reg];
		case 1:
			reg = (ram[ip] & 0b111000000) >> 6;
			registres[reg] = ram[ip + 1];
			break;
		default:
			break;
		}

		ip += 2;
	}
	return 0;
}
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
	int16_t registres[8]{};

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
		int16_t spec = (ram[ip] & 0b1110000000000) >> 10;
		int16_t imm = (ram[ip] & 0b1000000000) >> 9;
		int16_t rd = (ram[ip] & 0b111000000) >> 6;
		int16_t rs2 = (ram[ip] & 0b111000) >> 3;
		int16_t rs1 = ram[ip] & 0b111;
		int16_t s1 = imm ? ram[ip + 1] : registres[rs1];

		switch (opcode) {
		case 0:
			return s1;
		case 1:
			registres[rd] = s1;
			break;
		case 2:
			switch (spec) {
			case 0:
				registres[rd] = registres[rs2] + s1;
				break;
			case 1:
				registres[rd] = registres[rs2] - s1;
				break;
			case 2:
				registres[rd] = registres[rs2] * s1;
				break;
			case 3:
				registres[rd] = registres[rs2] / s1;
				break;
			case 4:
				registres[rd] = registres[rs2] % s1;
				break;
			case 5:
				registres[rd] = s1 - registres[rs2];
				break;
			case 6:
				registres[rd] = s1 % registres[rs2];
				break;
			case 7:
				registres[rd] = s1 / registres[rs2];
				break;
			}

			break;
		default:
			break;
		}

		ip += imm ? 2 : 1;
	}
	return 0;
}
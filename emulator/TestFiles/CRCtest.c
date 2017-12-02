#include <stdio.h>
#include "xcustom.h"

#define k_UPDATE 0
#define k_CALC 1
#define XCUSTOM_ACC 0
#define crcUpdate(y, taps, length) ROCC_INSTRUCTION(XCUSTOM_ACC, y, taps, length, k_UPDATE)
#define crcCalc(y, memsrc, length) ROCC_INSTRUCTION(XCUSTOM_ACC, y, memsrc, length, k_CALC)

int main() {
  printf("Running CRC\n");
	uint16_t data [] = {0xdead, 0xbeef, 0x0bad, 0xf00d}, y;
	uint32_t taps = 0xD5;
	uint32_t length = 16;

	uint16_t crc = 0xF;
    printf("Taps start:  %zu\n", taps);
  	crcUpdate(y, 0xD5, 8);
    printf("Should be F:  %zu\n", crc);
  	crcCalc(crc, &data, 16);
    printf("CRC:  %zu\n", crc);
  	printf("Test:  %zu\n", y);
	return 0;
}
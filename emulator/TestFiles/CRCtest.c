#include <stdio.h>
#include "xcustom.h"
#include <inttypes.h>

#define k_UPDATE 0
#define k_CALC 1
#define XCUSTOM_ACC 0
#define crcUpdate(y, taps, length) ROCC_INSTRUCTION(XCUSTOM_ACC, y, taps, length, k_UPDATE)
#define crcCalc(y, memsrc, length) ROCC_INSTRUCTION(XCUSTOM_ACC, y, memsrc, length, k_CALC)

int main() {
  printf("Running CRC\n");
  //uint16_t data [] = {0xdead, 0xbeef, 0x0bad, 0xf00d}, y;
  uint64_t data [] = {0xdeadbeef0badf00d}, y;
	uint32_t taps = 0x814141AB;
	uint32_t length = 32;
  uint32_t memlen = 64;

	uint64_t crc = 0;
  printf("CRC polynomial:  %lu\n", taps);
  printf("CRC size:  %lu\n", length);
  printf("CRC calculated across: %llu\n", memlen);
	crcUpdate(y, taps, length);
  printf("CRC variable initial state:  %lu\n", crc);
	crcCalc(crc, &data, memlen);
  printf("CRC variable final state:  %lu\n", crc);
	return 0;
}
#include <stdio.h>
#include "xcustom.h"
#include <inttypes.h>

#define k_SETPOLY 0
#define k_SETSTARTENDVALS 1
#define k_SETFLIPS 2
#define k_CALC 3
#define XCUSTOM_ACC 0
#define crcSetPoly(y, taps, length) ROCC_INSTRUCTION(XCUSTOM_ACC, y, taps, length, k_SETPOLY)
#define crcSetStartEndVals(y, start, endXOR) ROCC_INSTRUCTION(XCUSTOM_ACC, y, start, endXOR, k_SETSTARTENDVALS)
#define crcSetFlips(y, flipIn, flipOut) ROCC_INSTRUCTION(XCUSTOM_ACC, y, flipIn, flipOut, k_SETFLIPS)
#define crcCalc(y, memsrc, length) ROCC_INSTRUCTION(XCUSTOM_ACC, y, memsrc, length, k_CALC)

int main() {
  printf("Running CRC\n");
  //uint16_t data [] = {0xdead, 0xbeef, 0x0bad, 0xf00d}, y;
  uint64_t data [] = {0xdeadbeef0badf00d}, y;

  uint32_t polys [] =     {0x814141AB,  0x7, 0xD5,   0x1021,  0x4C11DB7,  0x4C11DB7, 0xC867, 0xC867, 0xC867};
  uint32_t flipIns [] =   {0,             0,    0,        0,          1,          0,      0,      0,      0};
  uint32_t flipOuts [] =  {0,             0,    0,        0,          1,          0,      0,      0,      0};
  uint32_t startVals [] = {0,             0,    0,        0, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFF,      0, 0xFFFF};
  uint32_t endXORs [] =   {0,             0,    0,        0, 0xFFFFFFFF, 0xFFFFFFFF,      0,      0,      0};
  uint32_t lengths [] =   {32,            8,    8,       16,         32,         32,     16,     16,     16};
  uint32_t memLens [] =   {64,           64,   64,       64,         64,         64,     64,     64,     64};

	uint64_t crc = 0;
  for (int i = 0; i < sizeof(polys)/sizeof(polys[0]); i++) {
    uint32_t poly = polys[i];
    uint32_t length = lengths[i];
    uint32_t memLen = memLens[i];
    uint32_t flipIn = flipIns[i];
    uint32_t flipOut = flipOuts[i];
    uint32_t startVal = startVals[i];
    uint32_t endXOR = endXORs[i];
    printf("CRC polynomial:  %lu\n", poly);
    printf("CRC size:  %lu\n", length);
    printf("CRC memLen: %llu\n", memLen);
    crcSetPoly(y, poly, length);
    crcSetStartEndVals(y, startVal, endXOR);
    crcSetFlips(y, flipIn, flipOut);
  	crcCalc(crc, &data, memLen);
    printf("CRC output:  %lu\n\n", crc);
  } // 0xDE 0xAD 0xBE 0xEF 0x0B 0xAD 0xF0 0x0D
	return 0;
}
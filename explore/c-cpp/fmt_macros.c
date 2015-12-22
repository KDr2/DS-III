/*
 *
 * see
 *  - /usr/include/inttypes.h
 *  - GLIBC/sysdeps/generic/inttypes.h
 *
 */

#ifdef __cplusplus
#define __STDC_FORMAT_MACROS
#endif

#include <inttypes.h>
#include <stdio.h>

int main(int argc, char* argv[]) {
    uint8_t u8 = 10;
    int16_t i16 = 20;
    int32_t i32 = 30;
    uint64_t u64 =40;
    printf("Dec:%"PRIu8",%"PRIi16",%"PRIi32",%"PRIu64"\n",
        u8, i16, i32, u64);
    printf("Oct:%"PRIo8",%"PRIo16",%"PRIo32",%"PRIo64"\n",
        u8, i16, i32, u64);
    printf("Hex:%02"PRIx8",%02"PRIx16",%02"PRIx32",%02"PRIx64"\n",
        u8, i16, i32, u64);
    return 0;
}

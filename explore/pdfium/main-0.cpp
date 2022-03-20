#include <vector>
#include <cstdint>
#include "public/fpdfview.h"
#include "core/fxcrt/fx_memory_wrappers.h"

int main()
{
    FPDF_InitLibrary();

    std::vector<uint8_t, FxAllocAllocator<uint8_t>> v;
    v.reserve(16);
    v.resize(512);
    v.push_back(1);

    FPDF_DestroyLibrary();
    return 0;
}

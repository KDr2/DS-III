#include "common_utils.h"

PDFSaver::PDFSaver() {
    file = new std::ofstream;
    flags = FPDF_INCREMENTAL; // FPDF_NO_INCREMENTAL
};

PDFSaver::~PDFSaver() {
    delete file;
};

void PDFSaver::save(FPDF_DOCUMENT document, std::string path) {
    writer.version = 1;
    writer.WriteBlock = [](
        FPDF_FILEWRITE *pThis,
        const void *data, unsigned long size) mutable -> int {
        void *real_this = (int8_t*)pThis - offsetof(PDFSaver, writer);
        ((PDFSaver*)real_this)->file->write(static_cast<const char *>(data), size);
        return 1;
    };

    file->open(path);
    FPDF_SaveAsCopy(document, &writer, flags);
    file->close();
}

FPDF_WIDESTRING wideStringFromWString(std::wstring &wstr) {
    size_t length = sizeof(uint16_t) * (wstr.length() + 1);
    auto *result(static_cast<unsigned short *>(malloc(length)));
    auto *result_span = reinterpret_cast<uint8_t *>(result);
    size_t i = 0;
    for (wchar_t w : wstr) {
        result_span[i++] = w & 0xff;
        result_span[i++] = (w >> 8) & 0xff;
    }
    result_span[i++] = 0;
    result_span[i] = 0;

    return result;
}

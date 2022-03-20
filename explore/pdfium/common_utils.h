// -*- mode: c++ -*-
#pragma once

#include <fstream>

#include "public/fpdfview.h"
#include "public/fpdf_save.h"

class PDFSaver {
public:
    PDFSaver();
    ~PDFSaver();

    void save(FPDF_DOCUMENT document, std::string path);

    std::ofstream *file;
    FPDF_DWORD flags;
    FPDF_FILEWRITE writer;
};

FPDF_WIDESTRING wideStringFromWString(std::wstring &wstr);

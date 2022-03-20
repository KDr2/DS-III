#include "common_utils.h"

#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <memory>

#include "public/fpdf_edit.h"

#include "fpdfsdk/cpdfsdk_helpers.h"
#include "fpdfsdk/cpdfsdk_pageview.h"


int main(int argc, char* argv[]){
    (void)argc;
    FPDF_InitLibrary();

    FPDF_DOCUMENT document = (FPDF_DOCUMENT)FPDF_LoadDocument(argv[1], "");
    if (document == nullptr) {
        printf("load document failed!\n");
        FPDF_DestroyLibrary();
        return 1;
    }

    int p = FPDF_GetPageCount(document);
    std::cout << "PAGE COUNT: " << p << "\n";

    FPDF_PAGEOBJECT text = FPDFPageObj_NewTextObj(document, "Arial", 14);
    std::wstring x = L"TEST TEXT";
    FPDFText_SetText(text, wideStringFromWString(x));
    FPDFPageObj_Transform(text, 1, 0, 0, 1, 100, 500);

    FPDF_PAGE page = FPDF_LoadPage(document, 0); // the first page
    FPDFPage_InsertObject(page, text);
    FPDFPage_GenerateContent(page);
    FPDF_ClosePage(page);

    PDFSaver saver;
    saver.save(document, "./out.pdf");
    FPDF_CloseDocument(document);

    FPDF_DestroyLibrary();
    return 0;
}

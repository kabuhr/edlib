#include <edlib.h>

void edlibAlignP(const char* query, const int queryLength,
		 const char* target, const int targetLength,
		 EdlibAlignConfig* config, EdlibAlignResult* result);
void edlibFreeAlignResultP(EdlibAlignResult* result);

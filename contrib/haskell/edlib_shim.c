#include "edlib_shim.h"

#include <stdio.h>
#include <stdlib.h>

void edlibAlignP(const char* query, const int queryLength,
		 const char* target, const int targetLength,
		 EdlibAlignConfig* config, EdlibAlignResult* result)
{
	*result = edlibAlign(query, queryLength, target, targetLength, *config);
}

void edlibFreeAlignResultP(EdlibAlignResult* result)
{
	edlibFreeAlignResult(*result);
}

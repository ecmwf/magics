#include "magics_api.h"
#include <stdio.h>

int main()
{

        mag_open();
        mag_setc ("output_format","ps");
        mag_setc ("output_name","testc"); // which is different each time

        mag_coast();
        mag_close();
        return 0;
}

/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

#include "magics_api.h"
#include <cairo.h>

int main(int argc, char **argv)
{
        /* First setup your cairo context */
        cairo_surface_t *surface;
        cairo_t* cr;

        surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 800, 500);
        cr = cairo_create (surface);
        cairo_set_source_rgb (cr, 0.0, 0.0, 1.0);

        /* Then call Magics++ */
        mag_open();
        mag_setp("output_cairo_drawing_context", cr);
        mag_setc ("output_format","cairo");
        mag_coast();
        mag_close();

        /* Then continue with cairo ... */
        cairo_destroy (cr);
        cr = cairo_create (surface);
        cairo_set_source_rgb (cr, 0.0, 0.0, 1.0);
        cairo_select_font_face (cr, "serif", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD);
        cairo_set_font_size (cr, 32.0);

        cairo_move_to (cr, 80., 100.0);
        cairo_show_text (cr, "Hello, this is CAIRO calling Magics++");

        cairo_destroy (cr);
        cairo_surface_write_to_png (surface, "cairo.png");
        cairo_surface_destroy (surface);
}

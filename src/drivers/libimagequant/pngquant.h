/* pngquant.c - quantize the colors in an alphamap down to a specified number
**
** Copyright (C) 1989, 1991 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
**
** - - - -
**
** © 1997-2002 by Greg Roelofs; based on an idea by Stefan Schneider.
** © 2009-2014 by Kornel Lesiński.
**
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without modification,
** are permitted provided that the following conditions are met:
**
** 1. Redistributions of source code must retain the above copyright notice,
**    this list of conditions and the following disclaimer.
**
** 2. Redistributions in binary form must reproduce the above copyright notice,
**    this list of conditions and the following disclaimer in the documentation
**    and/or other materials provided with the distribution.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
** AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
** IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
** DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
** FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
** DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
** SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
** CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
** OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
*/



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "rwpng.h"  /* typedefs, common macros, public prototypes */
#include "libimagequant.h"

struct pngquant_options {
    liq_attr *liq;
    liq_image *fixed_palette_image;
    liq_log_callback_function *log_callback;
    void *log_callback_user_info;
    float floyd;
    bool using_stdin, using_stdout, force, fast_compression, ie_mode,
        min_quality_limit, skip_if_larger,
        verbose;
};

pngquant_error  prepare_output_image(liq_result *result, liq_image *input_image, png8_image *output_image);
void            set_palette(liq_result *result, png8_image *output_image);
pngquant_error  read_image(liq_attr *options, const char *filename, int using_stdin, png24_image *input_image_p, liq_image **liq_image_p, bool keep_input_pixels, bool verbose);
pngquant_error  write_image(png8_image *output_image, png24_image *output_image24, const char *outname, struct pngquant_options *options);
char           *add_filename_extension(const char *filename, const char *newext);

void verbose_printf(struct pngquant_options *context, const char *fmt, ...);
pngquant_error pngquant_file(const char *filename, const char *outname, struct pngquant_options *options);


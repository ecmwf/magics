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
#include <stdbool.h>
#include <unistd.h>

extern char *optarg;
extern int optind, opterr;

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

static pngquant_error prepare_output_image(liq_result *result, liq_image *input_image, png8_image *output_image);
static void set_palette(liq_result *result, png8_image *output_image);
static pngquant_error read_image(liq_attr *options, const char *filename, int using_stdin, png24_image *input_image_p, liq_image **liq_image_p, bool keep_input_pixels, bool verbose);
static pngquant_error write_image(png8_image *output_image, png24_image *output_image24, const char *outname, struct pngquant_options *options);
static char *add_filename_extension(const char *filename, const char *newext);
static bool file_exists(const char *outname);

static void verbose_printf(struct pngquant_options *context, const char *fmt, ...)
{
    if (context->log_callback) {
        va_list va;
        va_start(va, fmt);
        int required_space = vsnprintf(NULL, 0, fmt, va)+1; // +\0
        va_end(va);

        char buf[required_space];
        va_start(va, fmt);
        vsnprintf(buf, required_space, fmt, va);
        va_end(va);

        context->log_callback(context->liq, buf, context->log_callback_user_info);
    }
}

static void log_callback(const liq_attr *attr, const char *msg, void* user_info)
{
    fprintf(stderr, "%s\n", msg);
}


static void print_full_version(FILE *fd)
{
    fprintf(fd, "pngquant, by Greg Roelofs, Kornel Lesinski.\n"
        #ifndef NDEBUG
                    "   DEBUG (slow) version.\n" /* NDEBUG disables assert() */
        #endif
        #if USE_SSE
                    "   Compiled with SSE instructions.\n"
        #endif
        );
    rwpng_version_info(fd);
    fputs("\n", fd);
}




pngquant_error pngquant_file(const char *filename, const char *outname, struct pngquant_options *options);


pngquant_error pngquant_file(const char *filename, const char *outname, struct pngquant_options *options)
{
    pngquant_error  retval             = SUCCESS;
    liq_image*      input_image        = NULL;
    png24_image     input_image_rwpng  = {};

   retval = read_image(options->liq, filename, options->using_stdin, &input_image_rwpng, &input_image, false, options->verbose);

    int quality_percent = 90; // quality on 0-100 scale, updated upon successful remap
    png8_image output_image = {};

    if (SUCCESS == retval) {
        verbose_printf(options, "  read %luKB file", (input_image_rwpng.file_size+1023UL)/1024UL);

        // when using image as source of a fixed palette the palette is extracted using regular quantization
        liq_result *remap = liq_quantize_image(options->liq, options->fixed_palette_image ? options->fixed_palette_image : input_image);

        if (remap) {
            liq_set_output_gamma(remap, 0.45455); // fixed gamma ~2.2 for the web. PNG can't store exact 1/2.2
            liq_set_dithering_level(remap, options->floyd);

            retval = prepare_output_image(remap, input_image, &output_image);
            if (SUCCESS == retval) {
                if (LIQ_OK != liq_write_remapped_image_rows(remap, input_image, output_image.row_pointers)) {
                    retval = OUT_OF_MEMORY_ERROR;
                }

                set_palette(remap, &output_image);

                double palette_error = liq_get_quantization_error(remap);
                if (palette_error >= 0) {
                    quality_percent = liq_get_quantization_quality(remap);
                    verbose_printf(options, "  mapped image to new colors...MSE=%.3f (Q=%d)", palette_error, quality_percent);
                }
            }
            liq_result_destroy(remap);
        } else {
            retval = TOO_LOW_QUALITY;
        }
    }

    if (SUCCESS == retval) {
        output_image.fast_compression = options->fast_compression;
        output_image.chunks = input_image_rwpng.chunks; input_image_rwpng.chunks = NULL;
        retval = write_image(&output_image, NULL, outname, options);
    }
/*
    if (options->using_stdout && keep_input_pixels && (TOO_LARGE_FILE == retval || TOO_LOW_QUALITY == retval)) {
        // when outputting to stdout it'd be nasty to create 0-byte file
        // so if quality is too low, output 24-bit original
        pngquant_error write_retval = write_image(NULL, &input_image_rwpng, outname, options);
        if (write_retval) {
            retval = write_retval;
        }
    }
*/
    liq_image_destroy(input_image);
    rwpng_free_image24(&input_image_rwpng);
    rwpng_free_image8(&output_image);
    return retval;
}


static void set_palette(liq_result *result, png8_image *output_image)
{
    const liq_palette *palette = liq_get_palette(result);

    // tRNS, etc.
    output_image->num_palette = palette->count;
    output_image->num_trans = 0;
    for(unsigned int i=0; i < palette->count; i++) {
        liq_color px = palette->entries[i];
        if (px.a < 255) {
            output_image->num_trans = i+1;
        }
        output_image->palette[i] = (png_color){.red=px.r, .green=px.g, .blue=px.b};
        output_image->trans[i] = px.a;
    }
}


static bool file_exists(const char *outname)
{
    FILE *outfile = fopen(outname, "rb");
    if ((outfile ) != NULL) {
        fclose(outfile);
        return true;
    }
    return false;
}


static char *temp_filename(const char *basename) {
    size_t x = strlen(basename);

    char *outname = (char *)malloc(x+1+4);
    if (!outname) return NULL;

    strcpy(outname, basename);
    strcpy(outname+x, ".tmp");

    return outname;
}


static const char *filename_part(const char *path)
{
    const char *outfilename = strrchr(path, '/');
    if (outfilename) {
        return outfilename+1;
    } else {
        return path;
    }
}

static pngquant_error write_image(png8_image *output_image, png24_image *output_image24, const char *outname, struct pngquant_options *options)
{
    FILE *outfile;
    char *tempname = NULL;

        tempname = temp_filename(outname);
        if (!tempname) return OUT_OF_MEMORY_ERROR;

        if ((outfile = fopen(tempname, "wb")) == NULL) {
            fprintf(stderr, "  error: cannot open '%s' for writing\n", tempname);
            free(tempname);
            return CANT_WRITE_ERROR;
        }

        if (output_image) {
            verbose_printf(options, "  writing %d-color image as %s", output_image->num_palette, filename_part(outname));
        } else {
            verbose_printf(options, "  writing truecolor image as %s", filename_part(outname));
        }

    pngquant_error retval;
        if (output_image) {
            retval = rwpng_write_image8(outfile, output_image);
        } else {
            retval = rwpng_write_image24(outfile, output_image24);
        }

    if (!options->using_stdout) {
        fclose(outfile);

        if (SUCCESS == retval) {
            // Image has been written to a temporary file and then moved over destination.
            // This makes replacement atomic and avoids damaging destination file on write error.
            if (0 != rename(tempname, outname)) {
                retval = CANT_WRITE_ERROR;
            }
        }

        if (retval) {
            unlink(tempname);
        }
        free(tempname);
    }

    if (retval && retval != TOO_LARGE_FILE) {
        fprintf(stderr, "  error: failed writing image to %s\n", outname);
    }

    return retval;
}

static pngquant_error read_image(liq_attr *options, const char *filename, int using_stdin, png24_image *input_image_p, liq_image **liq_image_p, bool keep_input_pixels, bool verbose)
{
    FILE *infile;

    if ((infile = fopen(filename, "rb")) == NULL) {
        fprintf(stderr, "  error: cannot open %s for reading\n", filename);
        return READ_ERROR;
    }

    pngquant_error retval;
        retval = rwpng_read_image24(infile, input_image_p, verbose);

        fclose(infile);

    if (retval) {
        fprintf(stderr, "  error: rwpng_read_image() error %d with file %s\n", retval, filename);
        return retval;
    }

    *liq_image_p = liq_image_create_rgba_rows(options, (void**)input_image_p->row_pointers, input_image_p->width, input_image_p->height, input_image_p->gamma);

    if (!*liq_image_p) {
        return OUT_OF_MEMORY_ERROR;
    }

    if (!keep_input_pixels) {
        if (LIQ_OK != liq_image_set_memory_ownership(*liq_image_p, LIQ_OWN_ROWS | LIQ_OWN_PIXELS)) {
            return OUT_OF_MEMORY_ERROR;
        }
        input_image_p->row_pointers = NULL;
        input_image_p->rgba_data = NULL;
    }

    return SUCCESS;
}

static pngquant_error prepare_output_image(liq_result *result, liq_image *input_image, png8_image *output_image)
{
    output_image->width = liq_image_get_width(input_image);
    output_image->height = liq_image_get_height(input_image);
    output_image->gamma = liq_get_output_gamma(result);

    /*
    ** Step 3.7 [GRR]: allocate memory for the entire indexed image
    */

    output_image->indexed_data = (unsigned char *)malloc(output_image->height * output_image->width);
    output_image->row_pointers = (unsigned char **)malloc(output_image->height * sizeof(output_image->row_pointers[0]));

    if (!output_image->indexed_data || !output_image->row_pointers) {
        return OUT_OF_MEMORY_ERROR;
    }

    for(unsigned int row = 0;  row < output_image->height;  ++row) {
        output_image->row_pointers[row] = output_image->indexed_data + row*output_image->width;
    }

    const liq_palette *palette = liq_get_palette(result);
    // tRNS, etc.
    output_image->num_palette = palette->count;
    output_image->num_trans = 0;
    for(unsigned int i=0; i < palette->count; i++) {
        if (palette->entries[i].a < 255) {
            output_image->num_trans = i+1;
        }
    }

    return SUCCESS;
}

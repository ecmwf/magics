#include <magics_api.h>

// Global variables

const double dPageWidth  = 21.0;
const double dPageHeight = 29.7;
const int    nNumSymbols = 27;



/*****************************************************************************
*
* Function    : PageFooter
* 
* Description : Prints a standard page footer to the bottom of the current
*               plot. The supplied page number is part of the footer text.
*
*****************************************************************************/

void PageFooter (int nPageNumber)
{
    char szFooter [64];

    sprintf (szFooter, "MAGICS Reference Tables, Page %d", nPageNumber);

    mag_setc ("text_mode",             "positional");
    mag_setr ("text_box_x_position",    12.0);
    mag_setr ("text_box_y_position",    0.05);
    mag_setr ("text_box_x_length",      dPageWidth / 2);
    mag_setr ("text_box_y_length",      0.8);
    mag_setr ("text_reference_character_height",  0.25);
    mag_setc ("text_colour",           "black");
    mag_setc ("text_line_1",            szFooter);
    mag_text ();
}




/*****************************************************************************
*
* Function    : GenerateColourTable
* 
* Description : Prints a table listing all the available colour names.
*               Each name is printed in its own colour.
*
*****************************************************************************/

void GenerateColourTable (void)
{
    const int    nNumColumns  = 3;
    const double dTitleHeight = 1.0;
    const double dTopMargin   = dTitleHeight * 1.8;
    const double dLeftMargin  = 0.0;
    int nNumColours, nNumRows;
    int nCol, nRow;
    int i;
    double dHeightOfColourEntry;
    double dX, dY;
    double dRowHeight;

    char *aszColours[] = 
    {
	    "foreground",
	    "red",
	    "green",
	    "blue",
	    "yellow",
	    "cyan",
	    "magenta",
	    "black",
	    "avocado",
	    "beige",
	    "brick",
	    "brown",
	    "burgundy",
	    "charcoal",
	    "chestnut",
	    "coral",
	    "cream",
	    "evergreen",
	    "gold",
	    "grey",
	    "khaki",
	    "kelly_green",
	    "lavender",
	    "mustard",
	    "navy",
	    "ochre",
	    "olive",
	    "peach",
	    "pink",
	    "rose",
	    "rust",
	    "sky",
	    "tan",
	    "tangerine",
	    "turquoise",
	    "violet",
	    "reddish_purple",
	    "purple_red",
	    "purplish_red",
	    "orangish_red",
	    "red_orange",
	    "reddish_orange",
	    "orange",
	    "yellowish_orange",
	    "orange_yellow",
	    "orangish_yellow",
	    "greenish_yellow",
	    "yellow_green",
	    "yellowish_green",
	    "bluish_green",
	    "blue_green",
	    "greenish_blue",
	    "purplish_blue",
	    "blue_purple",
	    "bluish_purple",
	    "purple",
	    "white"
    };


    nNumColours = sizeof (aszColours) / sizeof (char *);
    nNumRows = nNumColours / nNumColumns;
    dHeightOfColourEntry = (dPageHeight - dTopMargin) / nNumRows;


    // write the page title
    
    mag_setc ("text_mode",             "positional");
    mag_setr ("text_box_x_position",    5.0);
    mag_setr ("text_box_y_position",    dPageHeight - (dTitleHeight * 1.1));
    mag_setr ("text_reference_character_height",  dTitleHeight);
    mag_setc ("text_colour",           "black");
    mag_setc ("text_line_1",           "MAGICS Colours");
    mag_text ();


    // set up the text parameters that will stay the same for each colour

    mag_setr ("text_box_x_length",                6.85);
    mag_setr ("text_box_y_length",                1.2);
    mag_setr ("text_reference_character_height",  0.85);


    // for each colour, plot a text box with coloured text
   
    for (i = 0; i < nNumColours; i++)
    {
        nCol = i % nNumColumns;
        nRow = (int) (i / nNumColumns);

        dX = (dPageWidth  / nNumColumns) * (nCol) + dLeftMargin;
        dY = dHeightOfColourEntry * (nNumRows - nRow - 1);

        mag_setr ("text_box_x_position",   dX);
        mag_setr ("text_box_y_position",   dY);
        mag_setc ("text_colour",           aszColours[i]);
        mag_setc ("text_line_1",           aszColours[i]);
        mag_text ();
    }
}




/*****************************************************************************
*
* Function    : GenerateMarkerSymbolTable
* 
* Description : Prints a table listing all the available symbols.
*
*****************************************************************************/

void GenerateMarkerSymbolTable (void)
{
    const int    nNumCols          = 2;
    const int    nNumSymbolsPerCol = (nNumSymbols + 1) / nNumCols;
    const double dXInc             = 5.0;
    const double dYInc             = dPageHeight / nNumSymbolsPerCol;
    
    int i, nCol, nRow;
    double dX, dY;
    double dDummy = 7;
    char   szIndex [16];


    // set some global symbol parameters

    mag_setr ("symbol_height", 0.4);
    mag_set1r("symbol_input_number_list", &dDummy, 1); // should not need this, but seg fault without!
    mag_setc ("symbol_position_mode", "paper");


    // set some global text parameters

    mag_setc ("text_colour", "black");
    mag_setr ("text_box_x_length",                0.6);
    mag_setr ("text_box_y_length",                0.6);
    mag_setr ("text_reference_character_height",  0.5);


    for (i = 0; i < nNumSymbols; i++)
    {
        nCol = (i / nNumSymbolsPerCol);
        nRow = i - (nCol * nNumSymbolsPerCol);
        dX   = 10.0 + nCol * dXInc;
        dY   = dPageHeight - ((nRow + 1) * dYInc);


        //  plot a symbol at this position

        mag_set1r ("symbol_input_x_position", &dX,  1);
        mag_set1r ("symbol_input_y_position", &dY,  1);
        mag_seti  ("symbol_marker", i);
        mag_symb  ();


        // plot a text box with this symbol's index

        sprintf  (szIndex, "%d", i);
        mag_setr ("text_box_x_position",   dX - 5.0);
        mag_setr ("text_box_y_position",   dY);
        mag_setc ("text_line_1",           szIndex);
        mag_text ();
    }
}




/*****************************************************************************
*
* Function    : GenerateFontTable
* 
* Description : Prints a table listing all the characters in the font.
*
*****************************************************************************/

void GenerateFontTable (char *szFont)
{
    const int    nNumRows = 32;
    const int    nNumCols = 7;
    const double dYInc    = 25.0 / nNumRows;
    const double dXInc    = 20.0 / nNumCols;
    int    nIndex = 32, nCol, nRow;
    double dX, dY;
    char   szTitle [64];
    char   szOctString  [16], szSymbName [16];

    // set the font
    
    mag_setc ("text_font", szFont);


    // Plot the title
    // Note that #043 is the '#' character itself

    mag_setc ("text_mode", "positional");
    mag_setr ("text_box_x_position", 2.0);
    mag_setr ("text_box_y_position", 28.0);
    mag_setr ("text_box_x_length",   18.0);
    mag_setr ("text_box_y_length",   1.2);
    mag_setr ("text_reference_character_height", 0.8);
    sprintf  (szTitle, "%s Format: octal #043nnn", szFont);
    mag_setc ("text_line_1", szTitle);
    mag_setc ("text_colour",  "black");
    mag_text ();




    // Set up our table parameters

    mag_setr ("text_box_x_length",    2.0);
    mag_setr ("text_box_y_length",    2.0);
    mag_setr ("text_reference_character_height", 0.4);


    for (nCol = 0; nCol < nNumCols; nCol++)
    {
        for (nRow = 0; nRow < nNumRows; nRow++)
        {
            dX   = 0.5   + nCol * dXInc;
            dY   = 28.0  - ((nRow + 1) * dYInc);

            sprintf  (szOctString, "%03o ", nIndex);

            mag_setc ("text_colour",  "black");
            mag_setr ("text_box_x_position", dX);
            mag_setr ("text_box_y_position", dY);
            mag_setc ("text_line_1", szOctString);
            mag_text ();

            sprintf  (szSymbName, "#%o ", nIndex);
            mag_setc ("text_colour",        "blue");
            mag_setr ("text_box_x_position", dX + 0.7);
            mag_setc ("text_line_1", szSymbName);
            mag_text ();

            nIndex++;
        }
    }
}



/*****************************************************************************
*
* Function    : GeneratearChartHatchPatternsTable
* 
* Description : Prints a table showing all available bar chart hatch styles.
*
*****************************************************************************/

void GeneratearChartHatchPatternsTable ()
{
    const int nNumPatterns = 6;
    double adX  [6] = {1, 2, 3, 4, 5, 6};
    double adYL [6] = {0, 0, 0, 0, 0, 0};
    double adYU [6] = {3 ,5, 9, 1 ,2 ,9};


    // set up the axes    

	mag_setc ("axis_orientation","horizontal");
	mag_setc ("axis_position","bottom");
	mag_setr ("axis_min_value",0.0);
	mag_setr ("axis_max_value",7.0);
	mag_setr ("axis_tick_interval",1.0);
	mag_axis ();
	mag_setc ("axis_orientation","vertical");
	mag_setc ("axis_position","left");
	mag_setr ("axis_min_value",1.0);
	mag_setr ("axis_max_value",10.0) ;
	mag_setr ("axis_tick_interval",1.0);
	mag_axis ();


    // draw the bars

	mag_setc  ("graph_type",               "bar");
	mag_setr  ("graph_bar_width",           1.20);
	mag_setc  ("graph_shade",              "on");
	mag_setc  ("graph_shade_style",        "hatch");
	mag_setc  ("graph_shade_colour",       "red");
	mag_set1r ("graph_bar_x_values",        adX,6);
	mag_set1r ("graph_bar_y_lower_values",  adYL, 6);
	mag_set1r ("graph_bar_y_upper_values",  adYU,6);
	mag_graph ();

}




/*****************************************************************************
*
* Function    : main
* 
* Description : Program entry point.
*
*****************************************************************************/

int main()
{
    int nPageNumber = 1;

    // open magics and set the output device

    mag_open ();
    mag_setc ("output_format",    "ps");
    mag_setc ("output_name",      "ref_tables");

    mag_setr ("super_page_x_length", dPageWidth);
    mag_setr ("super_page_y_length", dPageHeight);
    mag_setr ("page_x_length",       dPageWidth);
    mag_setr ("page_y_length",       dPageHeight);

    mag_setc ("subpage_frame",      "off");
    mag_setc ("page_frame",         "off");
    mag_setc ("page_id_line",       "off");

    mag_setc ("subpage_map_projection","none");
    mag_setr ("subpage_lower_left_longitude",  -50.0);
    mag_setr ("subpage_lower_left_latitude",   -80.0);
    mag_setr ("subpage_upper_right_longitude",  65.0);
    mag_setr ("subpage_upper_right_latitude",   80.0);


    // generate each page of the booklet, complete with page footers

    GenerateColourTable ();
    PageFooter (nPageNumber++);

    mag_new ("super_page");
    GenerateMarkerSymbolTable ();
    PageFooter (nPageNumber++);


    // generate the font tables

    mag_new ("super_page");
    GenerateFontTable ("arial");
    PageFooter (nPageNumber++);

    mag_new ("super_page");
    GenerateFontTable ("helvetica");
    PageFooter (nPageNumber++);

    mag_new ("super_page");
    GenerateFontTable ("symbol");
    PageFooter (nPageNumber++);

    // generate the bar chart hatch table

    mag_new ("super_page");
    GeneratearChartHatchPatternsTable ();
    PageFooter (nPageNumber++);

    mag_close ();

    return 0;
}

#include <magics_api.h>

/* This example demonstrates a feature new to Magics++: boxplots. */

#define NUM_BOXES 4

int main()
{
	/* Our data values */
	double adPositions [NUM_BOXES] = {2.0, 4.0, 6.5, 8.0};
	double adMinValues [NUM_BOXES] = {1.0, 3.0, 5.2, 4.0};
	double adMaxValues [NUM_BOXES] = {5.0, 7.2, 9.4, 8.0};
	double adMedValues [NUM_BOXES] = {3.0, 5.0, 7.7, 6.0};
	double adUppValues [NUM_BOXES] = {4.0, 6.0, 8.5, 7.0};
	double adLowValues [NUM_BOXES] = {2.3, 4.0, 6.1, 5.0};

	/* open magics and set the output filename */

	mag_open ();
	mag_setc ("OUTPUT_NAME", "boxplot");

	/* Set up our data for the boxplots */

	mag_set1r ("BOXPLOT_POSITIONS",        adPositions, NUM_BOXES);
	mag_set1r ("BOXPLOT_MINIMUM_VALUES",   adMinValues, NUM_BOXES);
	mag_set1r ("BOXPLOT_MAXIMUM_VALUES",   adMaxValues, NUM_BOXES);
	mag_set1r ("BOXPLOT_MEDIAN_VALUES",    adMedValues, NUM_BOXES);
	mag_set1r ("BOXPLOT_BOX_UPPER_VALUES", adUppValues, NUM_BOXES);
	mag_set1r ("BOXPLOT_BOX_LOWER_VALUES", adLowValues, NUM_BOXES);

	/* Draw the boxplots using the default plotting attributes */

	mag_boxplot ();

	mag_setc ("TEXT_LINE_1",  "Boxplot - default plotting attributes");
	mag_text ();


	mag_close ();

	return 0;
}


#=============================================================
# This one is actually printing all the paletttes as legends
#=============================================================
from Magics.macro import *
import json
import subprocess

def load_palettes(json_file):
    """ 
    Load dictionary with palettes from json file 
    """
    with open (json_file,"r") as docs:
        list_of_colours = json.load(docs)
    return list_of_colours

def make_a_level_list(N_colours):
    """ Makes a list of levels for plotting """
    list_of_levels = []
    for i in range(N_colours):
        list_of_levels.append(float(i))
    return list_of_levels    

def plot_the_legend(out_name, colour_list):
    """ 
    Plot some random data to get the legend 
    for the pallete to use on Confluence 
    """
    N_colours = len(colour_list)+1
    list_of_levels = make_a_level_list(N_colours)
    data = mgrib(grib_input_file_name='total_precipitation.grib',grib_field_position=1)
    output1 = output(
            output_formats                = ['png'],
            output_name                   = out_name,
            subpage_y_position=-10.,
            output_name_first_page_number = 'off')

    #Defining the contour
    contour = mcont(
            contour_legend_only = 'on',
            contour_level_tolerance = 0,
            contour = 'off',
            contour_highlight = 'off',
            contour_hilo = 'off',
            contour_label = 'off',
            contour_level_selection_type = 'level_list',
            contour_level_list = list_of_levels, 
            contour_level_count = len(colour_list)+1,
            contour_shade = 'on',
            contour_shade_colour_list = colour_list,
            contour_shade_colour_method = 'list',
            contour_shade_method = 'area_fill',
            legend = 'on')

    #Defining the legend
    legend = mlegend(
            legend_entry_border = "off",
            legend_display_type = 'continuous',
            legend_box_mode = "positional",
            legend_box_x_position = 4.00,
            legend_box_y_position = 15.00,
            legend_box_x_length = 24.00,
            legend_box_y_length = 4.00)

    #Plotting
    plot(output1,mcoast(),data,contour,legend)

def crop_the_legend(out_name):
    cmd = ['convert','-crop','578x32+142+100',out_name+'.png',out_name+'.png']
    subprocess.call(cmd)

def put(file, where, comment):
    cmd = "java -jar /home/graphics/cgs/atlassian-cli-2.5.0/lib/confluence-cli-2.5.0.jar --server http://ussoftware.ecmwf.int:8081/wiki --user cgs --password sjnclEC4 --action addAttachment --space MAGP --file '%s' --comment '%s' --title '%s' " % (file, comment, where)
    print cmd
    os.system(cmd)

def main():
    list_of_colors = load_palettes("../../share/magics/styles/palettes.json")
    for key,val in list_of_colors.iteritems(): 
        colour_list = val['contour_shade_colour_list']
        list_name = key
        print list_name + ' = ' + str(colour_list)
        plot_the_legend(list_name, colour_list)
	crop_the_legend(list_name)
	put(list_name + ".png", "Predefined palettes in Magics",  "Palette" )
        

if __name__ == "__main__":    
    main()

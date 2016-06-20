# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

""" Explore reading and writing netCDF files.
"""

from scipy.io import netcdf as nc
import magmacro as magics

def extract_header_info(filename):
    """ Load a netcdf filename and print out the metadata of the file
    """
    ncfile = nc.netcdf_file(filename)
    
    print "Content of %s:" % filename
    print " "*3+"Available dimensions: %s" % ncfile.dimensions
    print " "*3+"Variables:"

    for var_name in ncfile.variables.keys():
        print ' '*6+'%s:' % var_name
        var = ncfile.variables[var_name]
        for attr_name in dir(var):
            if not attr_name.startswith('_') and attr_name not in ['data']:
                attr = getattr(var,attr_name)
                if str(type(attr)).find('instancemethod') == -1:
                    print ' '*9,
                    print attr_name, ":", attr
                else:
                    # FIXME: call the method, and print its result
                    pass

    return ncfile.variables


def plot_data_on_map():
    """ Extract the data from the nc object and plot a field over a world map
    """
    ncfile = nc.netcdf_file("output.nc")
    var = ncfile.variables
    param = var["207.210"]
    lat = var["latitude"].data
    lon = var["longitude"].data
    
    data = param.data
    scaling = getattr(param,'scale_factor')
    data = data.squeeze()
    data2 = data * scaling
    print data.dtype 
    print data.shape
    
    steplat = (lat[-1]-lat[0])/(len(lat))
    
    print (lat[0] + steplat*len(lat)), lat[-1]
    
    steplon = (lon[-1]-lon[0])/(len(lon))
    print "type=", steplat.dtype
    
    
    print(data)
    cont = magics.mcont(input_field=data2, 
            input_field_initial_latitude =lat[0]*1.,
            input_field_latitude_step = steplat,
            input_field_initial_longitude =lon[0]*1.,
            input_field_longitude_step =steplon)
            
    magics.plot(magics.mcoast(), cont)
    
    return data
    
    


if __name__ == "__main__":
    variable_dict = extract_header_info("output.nc")
    data = plot_data_on_map()

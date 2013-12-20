import traceback
import sys
import numpy

import gribapi
import Magics

INPUT='data/20110910_1200_130_temp_850_24.grib'
VERBOSE=1 # verbose error reporting


def plotData():

    #
    #  G R I B A P I
    #
    f = open(INPUT)
    gid = gribapi.grib_new_from_file(f)
    values = gribapi.grib_get_values(gid)
#    print values.dtype
#    print values.shape

    Ni=gribapi.grib_get(gid,'Ni')
    Nj=gribapi.grib_get(gid,'Nj')
    print 'GRIB_API: %d (%dx%d=%d) values found in %s' % (len(values),Nj,Ni,Nj*Ni,INPUT)
    latitudeOfFirstGridPointInDegrees=gribapi.grib_get(gid,'latitudeOfFirstGridPointInDegrees')
    longitudeOfFirstGridPointInDegrees=gribapi.grib_get(gid,'longitudeOfFirstGridPointInDegrees')
    latitudeOfLastGridPointInDegrees=gribapi.grib_get(gid,'latitudeOfLastGridPointInDegrees')
    longitudeOfLastGridPointInDegrees=gribapi.grib_get(gid,'longitudeOfLastGridPointInDegrees')
    jDirectionIncrementInDegrees=gribapi.grib_get(gid,'jDirectionIncrementInDegrees')
    iDirectionIncrementInDegrees=gribapi.grib_get(gid,'iDirectionIncrementInDegrees')

#    for key in ('max','min','average'):
#        print ' %s=%.10e' % (key,gribapi.grib_get(gid,key))

    gribapi.grib_release(gid)
    f.close()

    #
    #  M A G I C S
    #
    Magics.init()
    Magics.setc("OUTPUT_FORMAT","ps")
    Magics.setc("OUTPUT_NAME",  "py_arrays_from_grib")

    Magics.setr("INPUT_FIELD_INITIAL_LATITUDE" ,latitudeOfFirstGridPointInDegrees)
    Magics.setr("INPUT_FIELD_INITIAL_LONGITUDE",longitudeOfFirstGridPointInDegrees) 
    Magics.setr("INPUT_FIELD_LATITUDE_STEP" ,-jDirectionIncrementInDegrees)
    Magics.setr("INPUT_FIELD_LONGITUDE_STEP",iDirectionIncrementInDegrees)
    values2 = numpy.array(values-273.15)          # convert degree K to C
#    print values2.dtype
#    print values2.shape
    val = values2.reshape(Nj,Ni)
#    print val.dtype
#    print val.shape
    Magics.set2r("INPUT_FIELD",val)
    
#    Magics.setc ("contour_grid_value_plot",         "on")
#    Magics.setc ("contour_grid_value_plot_type",    "value")
#    Magics.seti ("contour_grid_value_lat_frequency", 8)
#    Magics.seti ("contour_grid_value_lon_frequency", 8)
#    Magics.setr ("contour_grid_value_height",        0.3)        
#    Magics.setc ("contour",          "off")
    Magics.cont()

    Magics.coast()
    Magics.finalize()



def main():
    try:
        plotData()
    except gribapi.GribInternalError,err:
        if VERBOSE:
            traceback.print_exc(file=sys.stderr)
        else:
            print >>sys.stderr,err.msg
        return 1

#    plotData()

if __name__ == "__main__":
    sys.exit(main())

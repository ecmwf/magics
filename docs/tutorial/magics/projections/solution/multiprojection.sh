# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


#Global
magmlx multiprojection.magml -title=Global


#Europe
magmlx multiprojection.magml -title=Europe -proj=polar_stereographic -vert=0 -lllat=21.51 -lllon=-37.27 -urlat=51.28 -urlon=65.

#North America
magmlx multiprojection.magml -title=NorthAmerica -proj=polar_stereographic -vert=-100 -lllat=-5 -lllon=-140 -urlat=30. -urlon=-15.

#South America
magmlx multiprojection.magml -title=SouthAmerica -lllat=-65 -lllon=-125 -urlat=20. -urlon=5.

#Asia
magmlx multiprojection.magml -title=Asia -lllat=0 -lllon=55 -urlat=80. -urlon=175.

#Australia
magmlx multiprojection.magml -title=Australia -lllat=-55 -lllon=90 -urlat=-5 -urlon=190.

#Africa
magmlx multiprojection.magml -title=Africa -lllat=-40 -lllon=-45 -urlat=40. -urlon=75.


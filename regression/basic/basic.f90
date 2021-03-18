! (C) Copyright 1996-2016 ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
! In applying this licence, ECMWF does not waive the privileges and immunities 
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.
!


        program grib


        call popen

        call psetc('output_name', 'basic')
        call psetc('output_format', 'png')
        call psetc('output_name_first_page_number', 'off')

        call psetc('page_id_line', 'off')

        call psetc('grib_input_file_name', 'data.grib')
        call pgrib

        call pcont
        call pset1c("text_lines", (/"<b>grib-api test (key=name):</b> <grib_info key='name'/>"/), 1)
        call ptext

        call pcoast

        call pclose

        end

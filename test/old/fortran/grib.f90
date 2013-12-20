
        program grib


        call popen

        call psetc('output_name', 'grib')
        call psetc('output_format', 'png')
        call psetc('output_name_first_page_number', 'off')

        call psetc('page_id_line', 'off')

        call psetc('grib_input_file_name', 'msl.grib')
        call pgrib

        call pcont

        call pset1c("text_lines", (/"<b>grib-api test (key=name):</b> <grib_info key='name'/>"/), 1)
        call ptext

        call pcoast

        call pclose

        end

      program  magics

C Define the colours for the shading
      character*25 colours
      dimension colours(64)

C Define the title
      character*500 title
      dimension title(2)

      data colours /  'HSL(0,0,1)', 
     +    'HSL(29,0.14,0.92)', 
     +    'HSL(29,0.29,0.83)', 
     +    'HSL(29,0.43,0.75)', 
     +    'HSL(300,0.08,0.92)', 
     +    'HSL(360,0.16,0.84)', 
     +    'HSL(13,0.3,0.75)', 
     +    'HSL(18,0.44,0.67)', 
     +    'HSL(300,0.16,0.83)', 
     +    'HSL(340,0.22,0.75)', 
     +    'HSL(360,0.34,0.67)', 
     +    'HSL(8,0.47,0.58)', 
     +    'HSL(300,0.24,0.75)', 
     +    'HSL(330,0.28,0.67)', 
     +    'HSL(349,0.38,0.58)', 
     +    'HSL(360,0.5,0.5)', 
     +    'HSL(180,0.17,0.92)', 
     +    'HSL(120,0.08,0.84)', 
     +    'HSL(57,0.17,0.75)', 
     +    'HSL(44,0.3,0.67)', 
     +    'HSL(209,0.14,0.84)', 
     +    'HSL(187,0,0.75)', 
     +    'HSL(29,0.15,0.67)', 
     +    'HSL(29,0.29,0.59)', 
     +    'HSL(239,0.16,0.75)', 
     +    'HSL(299,0.08,0.67)', 
     +    'HSL(360,0.17,0.58)', 
     +    'HSL(13,0.3,0.5)', 
     +    'HSL(258,0.21,0.67)', 
     +    'HSL(299,0.16,0.59)', 
     +    'HSL(341,0.22,0.5)', 
     +    'HSL(360,0.33,0.42)', 
     +    'HSL(180,0.34,0.83)', 
     +    'HSL(161,0.22,0.75)', 
     +    'HSL(120,0.16,0.67)', 
     +    'HSL(78,0.21,0.58)', 
     +    'HSL(193,0.3,0.75)', 
     +    'HSL(180,0.17,0.67)', 
     +    'HSL(120,0.08,0.58)', 
     +    'HSL(59,0.16,0.5)', 
     +    'HSL(209,0.29,0.67)', 
     +    'HSL(209,0.15,0.58)', 
     +    'HSL(217,0,0.5)', 
     +    'HSL(29,0.14,0.42)', 
     +    'HSL(224,0.3,0.58)', 
     +    'HSL(237,0.17,0.5)', 
     +    'HSL(299,0.08,0.42)', 
     +    'HSL(360,0.16,0.33)', 
     +    'HSL(180,0.5, 0.75)', 
     +    'HSL(169,0.38,0.67)', 
     +    'HSL(150,0.28,0.58)', 
     +    'HSL(120,0.24,0.5)', 
     +    'HSL(188,0.47,0.67)', 
     +    'HSL(180,0.34,0.59)', 
     +    'HSL(160,0.22,0.5)', 
     +    'HSL(120,0.16,0.42)', 
     +    'HSL(198,0.44,0.58)', 
     +    'HSL(193,0.3,0.5)', 
     +    'HSL(180,0.17,0.42)', 
     +    'HSL(120,0.08,0.33)', 
     +    'HSL(209,0.43,0.5)', 
     +    'HSL(209,0.29,0.42)', 
     +    'HSL(209,0.14,0.33)', 
     +    'HSL(191,0,0.25)' /
C Open Magics
      call ???

      call psetc('output_name', 'cloud_cover_asia')

C Set the geographical area:
C Here we use Asia
      call pset?(???)

C load the grib data
      call psetc('grib_input_file_name', 'cloud_cover.grb')
      call pgrib

c define the shading
      call pset?(???)
      call pset1c('contour_shade_colour_list', colours, 64)
      call pcont


C Add the coastlines
      call pset?(???)
      call ???

C Add a title
      call pset?(??)
      title(1)="Cloud cover valid for <grib_info key='valid-date'/>" 
      title(2)="<font colour='HSL(29,0.43,0.75)'> Low </font>"//
     +           '<font colour="HSL(360,0.5,0.5)"> L+M </font>'//
     +           '<font colour="HSL(300,0.24,0.75)"> Medium </font>'//
     +           '<font colour="HSL(209,0.43,0.5)"> M+H </font>'//
     +           '<font colour="HSL(180,0.5, 0.75)"> High </font>'//
     +           '<font colour="HSL(120,0.24,0.5)"> H+L </font>' 

      call pset1c('text_lines', title, 2)
      call ????


C Close Magics : execute the plot
      call ???

      end

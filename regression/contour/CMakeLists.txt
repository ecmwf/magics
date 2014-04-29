

#Extensive contour testing



list ( APPEND zs  cont_attributes_ex  cont_auto_ex cont_cellshade_test1 cont_dotshade_test1 cont_hatchshade_ex cont_linestyles_ex cont_markershade_ex cont_gridshade_test1 cont_labels cont_noshade_ex cont_solidshade_ex cont_solidshade_test1 cont_simple)

foreach( z ${zs} )
    add_magics_test(NAME contour_${z} SOURCES ${z}.F IMAGE ${z}.ps   THRESHOLD 2)
endforeach()

add_magics_test(NAME contour_cont08 SOURCES cont08.F IMAGE cont08.ps  DATA z500.grb THRESHOLD 2)
add_magics_test(NAME contour_alternate SOURCES cont_alternate.F IMAGE cont_alternate.ps  DATA t300.grb THRESHOLD 2)
add_magics_test(NAME contour_auto_t1279 SOURCES cont_auto_t1279.F IMAGE cont_auto_t1279.ps  DATA t1279_gg640_lev85.grib THRESHOLD 2)
add_magics_test(NAME contour_auto_t799 SOURCES cont_auto_t799.F IMAGE cont_auto_t799.ps  DATA t799_ll.grib THRESHOLD 2)
add_magics_test(NAME contour_flat_field_1 SOURCES cont_flat_field_1.F IMAGE cont_flat_field_1.ps  DATA cloudcover.grib THRESHOLD 2)
add_magics_test(NAME contour_gridvals_ex SOURCES cont_gridvals_ex.F IMAGE cont_gridvals_ex.ps  DATA r850.grb THRESHOLD 2)
add_magics_test(NAME contour_methods_ex SOURCES cont_methods_ex.F IMAGE cont_methods_ex.ps   THRESHOLD 2)
add_magics_test(NAME contour_res_ex SOURCES cont_res_ex.F IMAGE cont_res_ex.ps   THRESHOLD 2)
add_magics_test(NAME contour_akima_example SOURCES cont_akima_example.F IMAGE cont_akima_example.ps  DATA rh850.grib THRESHOLD 2)
add_magics_test(NAME contour_solidshade_test2 SOURCES cont_solidshade_test2.F IMAGE cont_solidshade_test2.ps   THRESHOLD 2)
add_magics_test(NAME contour_missing SOURCES cont_missing.F IMAGE cont_missing.ps  THRESHOLD 2)
add_magics_test(NAME contour_missing_2 SOURCES cont_missing_2.F IMAGE cont_missing_2.ps  DATA CWAO_Z.grib THRESHOLD 2)
add_magics_test(NAME contour_solidshade_test3 SOURCES cont_solidshade_test3.F IMAGE cont_solidshade_test3.ps  DATA lsp25.grib THRESHOLD 2)
add_magics_test(NAME contour_split SOURCES cont_split.F IMAGE cont_split.ps  DATA Tdiff.grib THRESHOLD 2)

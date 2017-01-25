add_magics_test(NAME data_gaussian SOURCES data_gaussian.F IMAGE data_gaussian.ps DATA  regular_gaussian_q300.grib gaussian_q300.grib  THRESHOLD 5)
add_magics_test(NAME data_grib2 SOURCES data_grib2.F IMAGE data_grib2.ps DATA 10_meter_v_velocity_fc_sfc_grib2.grib temperature_fc_pl_grib2.grib THRESHOLD 5)
add_magics_test(NAME data_gribs SOURCES data_gribs.F IMAGE data_gribs.ps DATA  EPS_27.grib pot_temp_system3.grib   THRESHOLD 5)
add_magics_test(NAME data_hirlam SOURCES data_hirlam.F IMAGE data_hirlam.ps 
                    DATA hirlam-sweden-t.grb hirlam-sweden-uv.grb hirlam-iceland-t.grb hirlam-iceland-uv.grb  THRESHOLD 5)
add_magics_test(NAME data_invalid_values SOURCES data_invalid_values.F IMAGE data_invalid_values.ps DATA t2m_masked.grib t2m_masked_1_5.grib THRESHOLD 5)
add_magics_test(NAME data_rgg640 SOURCES data_rgg640.F IMAGE data_rgg640.ps DATA  rgg640.grib   THRESHOLD 5)
add_magics_test(NAME data_rll_1-deg SOURCES data_rll_1-deg.F IMAGE data_rll_1-deg.ps DATA  prob_icefreesee_0101.grib   THRESHOLD 5)
add_magics_test(NAME data_t1279_gg640 SOURCES data_t1279_gg640.F IMAGE data_t1279_gg640.ps DATA t1279_gg640_lev85.grib   THRESHOLD 5)
add_magics_test(NAME data_t1279_gg640_cell SOURCES data_t1279_gg640_cell.F IMAGE data_t1279_gg640_cell.ps DATA   t1279_gg640_lev85.grib   THRESHOLD 5)
add_magics_test(NAME data_t3999_rgg2000 SOURCES data_t3999_rgg2000.F IMAGE data_t3999_rgg2000.ps DATA  orog.3999.grib   THRESHOLD 5)
add_magics_test(NAME data_t799 SOURCES data_t799.F IMAGE data_t799.ps DATA t799_ll.grib   THRESHOLD 5)
add_magics_test(NAME data_t799_t2m SOURCES data_t799_t2m.F IMAGE data_t799_t2m.ps DATA   t799_t2m_ll.grib   THRESHOLD 5)
add_magics_test(NAME dataonly_linear SOURCES dataonly_linear.F IMAGE dataonly_linear.ps DATA z500.grb  THRESHOLD 5)

add_magics_test(NAME data_octrahydro SOURCES octrahydro.py IMAGE octrahydro.png DATA octrahydro.grib THRESHOLD 2 PYTHON)
add_magics_test(NAME data_subreducedgaussian SOURCES subreducedgaussian.py IMAGE subreducedgaussian.png DATA subreducedgaussian.grib THRESHOLD 1 PYTHON)
add_magics_test(NAME data_1000-global-reduced-gg SOURCES t1000-global-reduced-gg.py IMAGE t1000-global-reduced-gg.png DATA t1000-global-reduced-gg.grb THRESHOLD 1 PYTHON)



add_magics_test(NAME data_mars_array SOURCES mars_array.py IMAGE mars_array.png DATA z500.grb THRESHOLD 1 PYTHON)

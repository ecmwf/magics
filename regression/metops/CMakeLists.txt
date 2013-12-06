add_magics_test(NAME metops_cloudcover SOURCES metops_cloudcover.F IMAGE metops_cloudcover.ps 
            DATA  cloudcover.grib   THRESHOLD 50)
add_magics_test(NAME metops_cloudcover_linear SOURCES metops_cloudcover_linear.F 
            IMAGE metops_cloudcover_linear.ps DATA cloudcover.grib   THRESHOLD 50)
add_magics_test(NAME metops_msl_and_precip SOURCES metops_msl_and_precip.F 
            IMAGE metops_msl_and_precip.ps DATA  total_precip.grib msl.grib   THRESHOLD 50)
add_magics_test(NAME metops_msl_and_t SOURCES metops_msl_and_t.F IMAGE metops_msl_and_t.ps 
            DATA   p12pt850_MSL.grib p12pt850_T.grib p12pt850_MSL_+60_025.grib p12pt850_T_+60_025.grib  THRESHOLD 50)
add_magics_test(NAME metops_p12zt500 SOURCES metops_p12zt500.F 
            IMAGE metops_p12zt500.ps 
            DATA  p12zt500_Z.grib p12zt500_T.grib   THRESHOLD 50)
add_magics_test(NAME metops_plspcpsn SOURCES metops_plspcpsn.F IMAGE metops_plspcpsn.ps
            DATA  plspcpsn_lspacc.grib plspcpsn_cpacc.grib plspcpsn_sfacc.grib THRESHOLD 50)
add_magics_test(NAME metops_t2_and_wind SOURCES metops_t2_and_wind.F IMAGE metops_t2_and_wind.ps 
            DATA  t2m_fc12.grib wind30m_fc12.grib  THRESHOLD 50)
add_magics_test(NAME metops_t_and_z SOURCES metops_t_and_z.F IMAGE metops_t_and_z.ps 
            DATA  t850_fc_12.grib z500_fc_12.grib   THRESHOLD 50)
add_magics_test(NAME metops_t_and_z_new SOURCES metops_t_and_z_new.F IMAGE metops_t_and_z_new.ps 
            DATA t850_fc_12.grib z500_fc_12.grib   THRESHOLD 50)
add_magics_test(NAME metops_weather_risk SOURCES metops_weather_risk.F IMAGE metops_weather_risk.ps 
        DATA  em.grb efi_2tm.grb efi_fx.grb efi_tp.grb THRESHOLD 50)

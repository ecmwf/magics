#include <magics_api.h>

void init() {
  mag_open ();
}

void finalize() {
  mag_close();
}

void coast() {
  mag_coast();
} 

void grib() {
  mag_grib();
}

void test() {
  mag_test();
}
void legend() {
  mag_legend();
}

void odb() {
  mag_odb();
}

void netcdf() {
  mag_netcdf();
}

void cont() {
  mag_cont();
}
void minput() {
  mag_input();
}
void mtable() {
  mag_table();
}
void obs() {
  mag_obs();
}

void raw() {
  mag_raw();
}

void image() {
  mag_image();
}

void plot() {
  mag_plot();
}

void text() {
  mag_text();
}

void wind() {
  mag_wind();
}
void line() {
  mag_line();
}

void symb() {
  mag_symb();
}

void boxplot() {
  mag_boxplot();
} 
void taylor() {
  mag_taylor();
} 
void tephi() {
  mag_tephi();
} 

void mimport() {
  mag_import();
}

void wrepjson() {
  mag_wrepjson();
}

void epsgraph() {
  mag_epsgraph();
}

void epscloud() {
  mag_epscloud();
}
void epsplumes() {
  mag_epsplumes();
}

void epswind() {
  mag_epswind();
}
void epswave() {
  mag_epswave();
}

void epsbar() {
  mag_epsbar();
}
void epsshading() {
  mag_epsshading();
 }

void mapgen() {
  mag_mapgen();
}

void new_page(const char* page)
{
  mag_new(page);
}

void reset(const char* name)
{
  mag_reset(name);
}

void setc(const char* name, const char* value)
{
  mag_setc(name, value);
}

void setr(const char* name, const double value)
{
  // std::cout << "setr "<<name<<" = "<<value<< std::endl;
  mag_setr(name, value);
}

void seti(const char* name, const int value)
{
  mag_seti(name, value);
}

double enqr(const char* name)
{
  double value = 0.;
  mag_enqr (name, &value);
  return value;
}

int enqi(const char* name)
{
  int value = 0;
  mag_enqi ( name, &value);
  return value;
}

char* enqc (const char* name)
{
  char *value = 0;
  mag_enqc (name, value);
  // std::cout << "nnn" << name<< "  "<< value << std::endl;
  return value;
}

void set1r(const char* name,double *data, const int dim)
{
  mag_set1r(name, data, dim);
}

void set2r(const char* name, double *data, const int dim1, const int dim2)
{
 mag_set2r(name, data, dim2, dim1);
}

void set3r(const char* name, double *data, const int dim1, const int dim2, const int dim3)
{
 mag_set3r(name, data, dim1, dim2, dim3);
}

void set1i(const char* name, int* data, const int dim)
{
 mag_set1i(name, data, dim);
}

void set2i(const char* name, int *data, const int dim1, const int dim2)
{
 mag_set2i(name, data, dim1, dim2);
}

void set3i(const char* name, int *data, const int dim1, const int dim2, const int dim3)
{
 mag_set3i(name, data, dim1, dim2, dim3);
}

void set1c(const char* name, const char** data, const int dim)
{
 mag_set1c(name, data, dim);
}

void pie() {
 mag_pie();
}

void graph() {
  mag_graph();
}

void axis() {
  mag_axis();
}

void geo() {
  mag_geo();
}

void eps() {
  mag_eps();
}

void info() {
  mag_info();
}

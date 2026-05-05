#ifndef NetcdfProj4MatrixInterpretor_H
#define NetcdfProj4MatrixInterpretor_H

#include <memory>
#include "Matrix.h"
#include "NetcdfInterpretor.h"

namespace magics {

class NetcdfProj4MatrixInterpretor : public NetcdfInterpretor {
public:
    NetcdfProj4MatrixInterpretor() = default;
    ~NetcdfProj4MatrixInterpretor() override = default;

    // -----------------------------------------------------------------
    // 1) static guess – returns nullptr if we cannot find a proj4 string
    // -----------------------------------------------------------------
    static NetcdfInterpretor* guess(const NetcdfInterpretor& from);

    // -----------------------------------------------------------------
    // 2) matrix creation – always uses a Proj4Matrix built from the proj4 string
    // -----------------------------------------------------------------
    bool interpretAsMatrix(Matrix** matrix) override;

    // -----------------------------------------------------------------
    // 3) set geographic extent on the transformation (converts projected
    //    meter corners to lat/lon so WMS reports the correct bounding box)
    // -----------------------------------------------------------------
    void visit(Transformation& transformation) override;

    // -----------------------------------------------------------------
    // 4) expose the proj4 string as meta‑data (optional but handy)
    // -----------------------------------------------------------------
    void visit(MetaDataCollector& mdc) override;

private:
    std::string proj4_;   // the proj4 definition we discovered
    std::string xVar_;    // name of the x‑coordinate variable (default "x")
    std::string yVar_;    // name of the y‑coordinate variable (default "y")
    std::unique_ptr<Proj4Matrix> matrix_;
};

} // namespace magics

#endif // NetcdfProj4MatrixInterpretor_H
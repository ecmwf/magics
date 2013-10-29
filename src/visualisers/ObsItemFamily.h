/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

#ifndef ObsItemFamily_H
#define ObsItemFamily_H




#include "ObsItem.h"
#include "UserPoint.h"
#include "ObsWindAttributes.h"
#include "ObsStationRingAttributes.h"
#include "ObsIdentifierAttributes.h"
#include "ObsTimePlotAttributes.h"
#include "ObsTemperatureAttributes.h"
#include "ObsPressureAttributes.h"
#include "ObsPressureLevelAttributes.h"
#include "ObsPressureTendencyAttributes.h"
#include "ObsDewPointAttributes.h"
#include "ObsVisibilityAttributes.h"
#include "ObsPresentWeatherAttributes.h"
#include "ObsPastWeatherAttributes.h"
#include "ObsCloudAttributes.h"
#include "ObsHeightAttributes.h"
#include "ObsThicknessAttributes.h"
#include "ObsDemoItem2Attributes.h"
#include "Symbol.h"

class ObsItemBox : public ObsItem
{
public:
	ObsItemBox() {}
	~ObsItemBox() {}
	
	virtual void set(const map<string, string>& def)
	{   
		row_ = atoi(find(def, "row").c_str());
		column_ = atoi(find(def, "column").c_str());
		colour_ = find(def, "colour");
	}
protected:		
	int row_;
	int column_;
	string colour_;
};

class ObsStationRing : public ObsItemBox
{
public:
	ObsStationRing() : attributes_(0)  {}
	~ObsStationRing()  {}
	void visit(std::set<string>& tokens)
	{
		if ( !attributes_ ) attributes_ = new ObsStationRingAttributes();
		if (!attributes_->visible_) return;
		tokens.insert("latitude");
		tokens.insert("longitude");
		tokens.insert("cloud_amount");
	}

	void operator()(CustomisedPoint&,  ComplexSymbol& symbol) const
	{ 
		if (!attributes_->visible_) return;
		SymbolItem*  station = new SymbolItem();
		station->x(column_);
		station->y(row_);		
		station->colour(*attributes_->colour_);
		station->symbol("circle");
		station->height(ring_size_);
		symbol.add(station);
	} 

protected:
	void print(ostream& out) const { out << "ObsStationRing";  }
	ObsStationRingAttributes* attributes_; // Here we are delegating instead of inheriting ...
};


class ObsTimePlot : public ObsItemBox
{
public:
	ObsTimePlot() : attributes_(0)  {}
	~ObsTimePlot()  {}
	void visit(std::set<string>& tokens);
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsTimePlot";  }
	ObsTimePlotAttributes* attributes_; // Here we are delegating instead of inheriting ...
};


class ObsWind : public ObsItemBox
{
public:
	ObsWind() : attributes_(0) { setOrigins(); }
	~ObsWind()  {}
	void visit(std::set<string>& tokens);

	virtual void operator()(CustomisedPoint&,  ComplexSymbol&) const;
	virtual void set(const map<string, string>& def)
	{
		row_ = atoi(find(def, "row").c_str());
		column_ = atoi(find(def, "column").c_str());
		colour_ = find(def, "colour");
		speed_ = find(def, "wind_speed", "wind_speed");
		direction_ = find(def, "wind_direction",  "wind_direction");
	}
protected:
	string speed_;
	string direction_;
	void setOrigins();
	void print(ostream& out) const { out << "ObsWind";  }
	ObsWindAttributes* attributes_; // Here we are delegating instead of inheriting ...
	
};

class ObsCloudAndWind : public ObsItemBox
{
public:
	ObsCloudAndWind() : attributes_(0) { setOrigins(); }
	~ObsCloudAndWind()  {}
	void visit(std::set<string>& tokens)
	{
		if (!attributes_ ) attributes_ = new ObsWindAttributes();
		if ( !colour_.empty() )  attributes_->colour_ = auto_ptr<Colour>(new Colour(colour_));
		if (!attributes_->visible_) return;
		tokens.insert("wind_speed");
		tokens.insert("wind_direction");
		tokens.insert("cloud_amount");
	}

	virtual void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void setOrigins();
	void print(ostream& out) const { out << "ObsWind";  }
	ObsWindAttributes* attributes_; // Here we are delegating instead of inheriting ...
	static map<int, string> origins_;
};



class ObsTemperature : public ObsItemBox
{
public:
	ObsTemperature() : attributes_(0) {}
	~ObsTemperature()  {}
	void visit(std::set<string>& tokens)
	{
		if ( !attributes_ ) attributes_ = new ObsTemperatureAttributes();
		if ( !colour_.empty() )  attributes_->colour_ = auto_ptr<Colour>(new Colour(colour_));
		if (!attributes_->visible_) return;
		tokens.insert("temperature");
	}
	void operator()(CustomisedPoint&,  ComplexSymbol& symbol) const;

protected:
	void print(ostream& out) const { out << "ObsTemperature";  }
	ObsTemperatureAttributes* attributes_; // Here we are delegating instead of inheriting ...
};

class ObsSeaTemperature : public ObsItemBox
{
public:
	ObsSeaTemperature() {}
	~ObsSeaTemperature()  {}
	void visit(std::set<string>& tokens);
	void operator()(CustomisedPoint&,  ComplexSymbol& symbol) const;

protected:
	void print(ostream& out) const { out << "ObsSeaTemperature";  }

};
class ObsWave : public ObsItemBox
{
public:
	ObsWave() {}
	~ObsWave()  {}
	void visit(std::set<string>& tokens);
	void operator()(CustomisedPoint&,  ComplexSymbol& symbol) const;

protected:
	void print(ostream& out) const { out << "ObsWave"; }
};
class ObsPressure : public ObsItemBox
{
public:
	ObsPressure() : attributes_(0) {}
	~ObsPressure()  {}
	void visit(std::set<string>& tokens);
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsPressure";  }
	ObsPressureAttributes* attributes_; // Here we are delegating instead of inheriting ...
};


class ObsPressureLevel : public ObsItemBox
{
public:
	ObsPressureLevel() : attributes_(0) {}
	~ObsPressureLevel()  {}
	void visit(std::set<string>& tokens);
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsPressureLevel";  }
	ObsPressureLevelAttributes* attributes_; // Here we are delegating instead of inheriting ...
};


class ObsPressureTendency : public ObsItemBox
{
public:
	ObsPressureTendency() : attributes_(0) {}
	~ObsPressureTendency()  {}
	void visit(std::set<string>& tokens);
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsPressureTendency";  }
	ObsPressureTendencyAttributes* attributes_; // Here we are delegating instead of inheriting ...
};


class ObsThickness : public ObsItemBox
{
public:
	ObsThickness() : attributes_(0) {}
	~ObsThickness()  {}
	void visit(std::set<string>& tokens);
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsThickness";  }
	ObsThicknessAttributes* attributes_; // Here we are delegating instead of inheriting ...
	
};

class ObsDewPoint : public ObsItemBox
{
public:
	ObsDewPoint()  {}
	~ObsDewPoint()  {}
	void visit(std::set<string>& tokens);
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsDewPoint";  }

};


class ObsHeight : public ObsItemBox
{
public:
	ObsHeight() : attributes_(0) {}
	~ObsHeight()  {}
	void visit(std::set<string>& tokens);
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsHeight";  }
	ObsHeightAttributes* attributes_; // Here we are delegating instead of inheriting ...
};


class ObsVisibility : public ObsItemBox
{
public:
	ObsVisibility() : attributes_(0) {}
	~ObsVisibility()  {}
	void visit(std::set<string>& tokens);
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsVisibility";  }
	ObsVisibilityAttributes* attributes_; // Here we are delegating instead of inheriting ...
};


class ObsPresentWeather : public ObsItemBox
{
public:
	ObsPresentWeather() : attributes_(0) {}
	~ObsPresentWeather()  {}
	void visit(std::set<string>& tokens);	
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsPresentWeather";  }
	ObsPresentWeatherAttributes* attributes_; // Here we are delegating instead of inheriting ...
};


class ObsIdentifier : public ObsItemBox
{
public:
	ObsIdentifier() : attributes_(0) {}
	~ObsIdentifier()  {}
	void visit(std::set<string>& tokens);	
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsIdentifier";  }
	ObsIdentifierAttributes* attributes_; // Here we are delegating instead of inheriting ...
};


class ObsPastWeather : public ObsItemBox
{
public:
	ObsPastWeather() : attributes_(0) {}
	~ObsPastWeather()  {}
	void visit(std::set<string>& tokens);	
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsPastWeather";  }
	ObsPastWeatherAttributes* attributes_; // Here we are delegating instead of inheriting ...
};


class ObsCloud : public ObsItemBox
{
public:
	ObsCloud(){}
	~ObsCloud()  {}
	void set(const map<string, string>& def);
	void visit(std::set<string>& tokens);	
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsCloud";  }
	int lowRow_;
	int lowColumn_;
	int mediumRow_;
	int mediumColumn_;
	int highRow_;
	int highColumn_;
};

class ObsDemoItem1 : public ObsItemBox
{
public:
	ObsDemoItem1()  {}
	~ObsDemoItem1()  {}

	void visit(std::set<string>& tokens);
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsDemoItem1";  }


};
class ObsDemoItem2 : public ObsItemBox
{
public:
	ObsDemoItem2()   { attributes_ = new ObsDemoItem2Attributes(); }
	~ObsDemoItem2()  {}

	void visit(std::set<string>& tokens);
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsDemoItem2";  }
	ObsDemoItem2Attributes* attributes_; // Here we are delegating instead of inheriting ...

};

class ObsEra : public ObsItemBox
{
public:
	ObsEra()   { }
	~ObsEra()  {}
	void set(const map<string, string>& def);
	void visit(std::set<string>& tokens);
	void operator()(CustomisedPoint&,  ComplexSymbol&) const;

protected:
	void print(ostream& out) const { out << "ObsEra";  }
	string key_;
	string colour_;

};
#endif

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




#include "Seconds.h"

using namespace magics;

Seconds::Seconds(double seconds):
	seconds_(seconds)
{
    
}

Seconds::Seconds(const timeval& time):
	seconds_(time.tv_sec + time.tv_usec / 1000000.0)
{
}

static struct  {
    int length_;
    const char *name_;
} periods[] = {
    {7 * 24 * 60 * 60, "week",},    
    {24 * 60 * 60, "day",}, 
    {60 * 60, "hour",}, 
    {60, "minute",},    
    {1, "second",}, 
    {0,0,},
};

namespace magics {
ostream& operator<<(ostream& s,const Seconds&  sec)
{
	double t = sec.seconds_;
    long n  = long(t);
    int flg = 0;

    for(int i=0;periods[i].length_;i++)
    {
        long m = n / periods[i].length_;
        if(m) {
            if(flg) s << ' ';
            s << m << ' ' << periods[i].name_;
            if(m>1) s << 's';
            n %= periods[i].length_;
            flg++;
        }
    }   

    if(!flg) s << t << " second";

	return s;
}
}
Seconds::operator string() const
{
	ostringstream s;

	s << *this << ends;

	return s.str();
}

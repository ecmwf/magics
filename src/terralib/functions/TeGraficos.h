#include <TeQtGraph.h>

void TeAdjustAxis(TeGraphPlot*, double, double, double, double);

void
TePlotGraphic(TeGraphPlot* gplot, QPaintDevice* pd, Labels label, double xmin, double ymin,
			  double xmax, double ymax, vector<double> vx, vector<double> vy)
{
	int	i;
	Real_Vector	x = new Real[vx.size()];
	Real_Vector	y = new Real[vy.size()];

	if(x== NULL || y==NULL)
		return;

	for(i=0; i<vx.size(); i++)
	{
		x[i] = vx[i];
		y[i] = vy[i];
	}

	gplot->reset();
	gplot->plotInit(5, 2, 2, 2);
	gplot->setColorText(0, 0, 160);
	TeAdjustAxis(gplot, xmin, ymin, xmax, ymax);
	gplot->setColorGrid (128, 0, 128);
	gplot->setColorLine (255, 0, 255);
	gplot->setColorCurve (0, 0, 200, 200);
	gplot->loadDirect ((int)BAR_BINARY, 0, vx.size(), label, x, y);
	gplot->plotCurves (); // na primeira vez o grafico sai estranho
	// plot outra vez
	TeColor textColor(0, 0, 160);
	gplot->reset();
	gplot->plotInit(5, 2, 2, 2);
	gplot->setColorText(0, 0, 160);
	TeAdjustAxis(gplot, xmin, ymin, xmax, ymax);
	gplot->setColorGrid (128, 0, 128);
	gplot->setColorLine (255, 0, 255);
	gplot->setColorCurve (0, 0, 200, 200);
	gplot->loadDirect ((int)BAR_BINARY, 0, vx.size(), label, x, y);
	gplot->plotCurves ();

	delete []x;
	delete []y;
}

void
TeAdjustAxis(TeGraphPlot* gplot, double xmin, double ymin, double xmax, double ymax)
{
	double	rangex, rangey;
	int		rx, ry;
	double	v, md;	

	if ((v = fabs(xmin)) > 0.)
	{
		md = 1.;
		while(v >= 10.)
		{
			v /= 10.;
			md *= 10.;
		}
		while(v < 1.)
		{
			v *= 10.;
			md /= 10.;
		}
		if (v != 1.)
		{
			if (xmin < 0)
				v = (long)(v + 1.);
			else
				v = (long)(v - 1.);
		}
		if (xmin > 0)
			xmin = v * md;
		else
			xmin = -v * md;
	}
	
	if ((v = fabs(xmax)) > 0.)
	{
		md = 1.;
		while(v >= 10.)
		{
			v /= 10.;
			md *= 10.;
		}
		while(v < 1.)
		{
			v *= 10.;
			md /= 10.;
		}
		if (v != 1.)
		{
			if (xmax > 0)
				v = (long)(v + 1.);
			else
				v = (long)(v - 1.);
		}
		if (xmax > 0)
			xmax = v * md;
		else
			xmax = -v * md;
	}	
	if (xmin<0 && xmax>0)
	{
		if (fabs(xmin) > xmax) 
			xmax = -xmin;
		else if (fabs(xmin) < xmax)
			xmin = -xmax;
	}
	else if (xmin>0 && xmax>0)
	{
		char b1[30], b2[30];
		sprintf(b1, "%.0f", xmin);
		sprintf(b2, "%.0f", xmax);
		if (strlen(b2) > strlen(b1))
			xmin = 0;
	}
	else if (xmin<0 && xmax<0)
	{
		char b1[30], b2[30];
		sprintf(b1, "%.0f", xmin);
		sprintf(b2, "%.0f", xmax);
		if (strlen(b1) > strlen(b2))
			xmax = 0;
	}
	
	if ((v = fabs(ymin)) > 0.)
	{
		md = 1.;
		while(v >= 10.)
		{
			v /= 10.;
			md *= 10.;
		}
		while(v < 1.)
		{
			v *= 10;
			md /= 10;
		}
		if (v != 1.)
		{
			if (ymin < 0)
				v = (long)(v + 1.);
			else
				v = (long)(v - 1.);
		}
		if (ymin > 0)
			ymin = v * md;
		else
			ymin = -v * md;
	}

	if ((v = fabs(ymax)) > 0.)
	{
		md = 1.;
		while(v >= 10.)
		{
			v /= 10.;
			md *= 10.;
		}
		while(v < 1.)
		{
			v *= 10.;
			md /= 10.;
		}
		if (v != 1.)
		{
			if (ymax > 0)
				v = (long)(v + 1.);
			else
				v = (long)(v - 1.);
		}
		if (ymax > 0)
			ymax = v * md;
		else
			ymax = -v * md;
	}
	if (ymin<0 && ymax>0)
	{
		if (fabs(ymin) > ymax)
			ymax = -ymin;
		else if (fabs(ymin) < ymax)
			ymin = -ymax;
	}
	else if (ymin>0 && ymax>0)
	{
		char b1[30], b2[30];
		sprintf(b1, "%.0f", ymin);
		sprintf(b2, "%.0f", ymax);
		if (strlen(b2) > strlen(b1))
			ymin = 0;
	}
	else if (ymin<0 && ymax<0)
	{
		char b1[30], b2[30];
		sprintf(b1, "%.0f", ymin);
		sprintf(b2, "%.0f", ymax);
		if (strlen(b1) > strlen(b2))
			ymax = 0;
	}

	if((rangex = xmax - xmin) > 0.)
	{
		while (rangex > 10.)
			rangex /= 10.;
		while (rangex < 1.)
			rangex *= 10.;
		rx = rangex;
		if (rangex > ((double)rx + rx*.000001))
			rx = (int)(rangex + 1.);
		if (rx<=1 || (rx>9 && rx<=10))
			rx = 10;
		else
			rx = rx * 2;
	}

	if((rangey = ymax - ymin) > 0.)
	{
		while (rangey > 10.)
			rangey /= 10.;
		while (rangey < 1.)
			rangey *= 10.;
		ry = rangey;
		if (rangey > ((double)ry + ry*.000001))
			ry = (int)(rangey + 1.);
		if (ry<=1 || (ry>9 && ry<=10))
			ry = 10;
		else
			ry = ry * 2;
	}

	gplot->setScale(xmin, xmax, ymin, ymax);
	gplot->setGridSize(rx, ry, 5, 5);		
}

void
TeGetHistogramValues (TeDatabasePortal *p, vector<double> vx, vector<double> vy)
{
	double	v, a, b;
	int		i;
	map<double, int>	h;

	bool bb = p->fetchRow(0);
	while(bb)
	{
		v = atof(p->getData(0));
		h[v] = 0;
		bb = p->fetchRow();
	}
		
	bb = p->fetchRow(0);
	while(bb)
	{
		v = atof(p->getData(0));
		i = h[v] + 1;
		h[v] = i;
		bb = p->fetchRow();
	}

	map<double, int>::iterator it;
	it = h.begin();
	while(it != h.end())
	{
		a = (*it).first;
		b = (double) (*it).second;
		vx.push_back(a);
		vy.push_back(b);
		it++;
	}
}
	
void
TeGetScaterValues (TeDatabasePortal *p, vector<double> vx, vector<double> vy)
{
	double	a, b;

	bool bb = p->fetchRow(0);
	while(bb)
	{
		a = atof(p->getData(0));
		b = atof(p->getData(1));
		vx.push_back(a);
		vy.push_back(b);
		bb = p->fetchRow();
	}
}


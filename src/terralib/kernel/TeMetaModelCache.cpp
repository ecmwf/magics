#include "TeMetaModelCache.h"

void TeMetaModelCache::clear()
{
	TeViewMap::iterator viewIt;
	for (viewIt = viewMap_.begin(); viewIt != viewMap_.end(); ++viewIt)
	{
		if(viewIt->second)
			delete viewIt->second;
	}

	TeLayerMap::iterator layerIt;
	for (layerIt = layerMap_.begin(); layerIt != layerMap_.end(); ++layerIt)
	{
		if (layerIt->second)
			delete layerIt->second;
	}

	TeProjectMap::iterator projectIt;
	for (projectIt = projectMap_.begin(); projectIt != projectMap_.end(); ++projectIt)
	{
		if (projectIt->second)
			delete projectIt->second;
	}

	TeThemeMap::iterator invThemeIt;
	for (invThemeIt = invalidThemeMap_.begin(); invThemeIt != invalidThemeMap_.end(); ++invThemeIt)
	{
		if (invThemeIt->second)
			delete invThemeIt->second;
	}

	legendMap_.clear();
	themeMap_.clear();
	invalidThemeMap_.clear();
	viewMap_.clear();
	layerMap_.clear();
	projectMap_.clear();
}


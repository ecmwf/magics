set( _terralib_srcs
# headers
            kernel/checkvec.h kernel/dynpq.h kernel/graph.h kernel/showseq.h
            kernel/Gra_algo.h
            kernel/TeAbstractFactory.h kernel/TeAbstractTheme.h
            kernel/TeAffineGT.h kernel/TeAffineGTFactory.h
            kernel/TeAgnostic.h kernel/TeAsciiFile.h
            kernel/TeAssertions.h kernel/TeAttribute.h
            kernel/TeBaseSTInstance.h kernel/TeBaseSTInstanceSet.h
            kernel/TeBox.h kernel/TeBufferRegion.h
            kernel/TeComposite.h kernel/TeCoord2D.h
            kernel/TeCounted.h
            kernel/TeDBConnection.h kernel/TeDBConnectionsPool.h
            kernel/TeDataTypes.h kernel/TeDatabase.h
            kernel/TeDatabaseFactory.h kernel/TeDatabaseFactoryParams.h
            kernel/TeDatum.h kernel/TeDecoder.h
            kernel/TeDecoderASCIIGrid.h kernel/TeDecoderDatabase.h
            kernel/TeDecoderFile.h kernel/TeDecoderMemory.h
            kernel/TeDecoderMemoryMap.h kernel/TeDecoderSPR.h
            kernel/TeDecoderSmartMem.h kernel/TeDecoderVirtualMemory.h
            kernel/TeDefines.h
            kernel/TeErrorLog.h kernel/TeErrorMessage.h
            kernel/TeException.h kernel/TeExternalTheme.h
            kernel/TeFactory.h kernel/TeFragmentation.h
            kernel/TeGTFactory.h kernel/TeGTParams.h
            kernel/TeGeometricTransformation.h kernel/TeGeometry.h
            kernel/TeGeometryAlgorithms.h kernel/TeGroupingAlgorithms.h
            kernel/TeImportRaster.h kernel/TeInitRasterDecoders.h
            kernel/TeIntersector.h
            kernel/TeKdTree.h
            kernel/TeLayer.h kernel/TeLegendEntry.h
            kernel/TeMappedMemory.h kernel/TeMatrix.h
            kernel/TeMeasure.h kernel/TeMetaModelCache.h
            kernel/TeMultiContainer.h kernel/TeMultiGeometry.h
            kernel/TeMutex.h
            kernel/TeNeighbours.h kernel/TeNetwork.h
            kernel/TeOverlay.h kernel/TeOverlayUtils.h
            kernel/TePieBar.h kernel/TePrecision.h
            kernel/TeProgress.h kernel/TeProgressBase.h
            kernel/TeProject.h kernel/TeProjection.h
            kernel/TeProjectiveGT.h kernel/TeProjectiveGTFactory.h
            kernel/TePrototype.h kernel/TeProxMatrixConstructionStrategy.h
            kernel/TeProxMatrixImplementation.h
            kernel/TeProxMatrixSlicingStrategy.h
            kernel/TeProxMatrixWeightsStrategy.h
            kernel/TeQuerier.h kernel/TeQuerierDB.h
            kernel/TeQuerierDBStr1.h kernel/TeQuerierDBStr2.h
            kernel/TeQuerierDBStr3.h kernel/TeQuerierImpl.h
            kernel/TeQuerierParams.h
            kernel/TeRTree.h kernel/TeRaster.h
            kernel/TeRasterMemManager.h kernel/TeRasterParams.h
            kernel/TeRasterRemap.h kernel/TeRasterTransform.h
            kernel/TeRepresentation.h
            kernel/TeSTEFunctionsDB.h kernel/TeSTElementSet.h
            kernel/TeSTEvent.h kernel/TeSTInstance.h
            kernel/TeSharedPtr.h kernel/TeSingleton.h
            kernel/TeSlice.h kernel/TeSpatialOperations.h
            kernel/TeStatistics.h kernel/TeStdFile.h
            kernel/TeTable.h kernel/TeTemporalSeries.h
            kernel/TeTheme.h kernel/TeThread.h
            kernel/TeThreadDatatypes.h kernel/TeThreadFunctor.h
            kernel/TeThreadParameters.h kernel/TeThreadSignal.h
            kernel/TeTime.h kernel/TeTimeInterval.h
            kernel/TeTin.h
            kernel/TeUtils.h
            kernel/TeVectorRemap.h kernel/TeView.h
            kernel/TeViewNode.h kernel/TeVisual.h
            kernel/yyTemporal.h
            utils/TeDatabaseUtils.h
            jpeg/ijl.h jpeg/jpeg.h
# cpp sources
            kernel/TeAbstractTheme.cpp
            kernel/TeAffineGT.cpp kernel/TeAffineGTFactory.cpp
            kernel/TeAgnostic.cpp kernel/TeAsciiFile.cpp
            kernel/TeBox.cpp kernel/TeBufferRegion.cpp
            kernel/TeCentroid.cpp
            kernel/TeDBConnection.cpp kernel/TeDBConnectionsPool.cpp
            kernel/TeDatabase.cpp kernel/TeDatabaseFactoryParams.cpp
            kernel/TeDatum.cpp kernel/TeDecoderASCIIGrid.cpp
            kernel/TeDecoderDatabase.cpp kernel/TeDecoderFile.cpp
            kernel/TeDecoderMemory.cpp kernel/TeDecoderMemoryMap.cpp
            kernel/TeDecoderSPR.cpp kernel/TeDecoderSmartMem.cpp
            kernel/TeDecoderVirtualMemory.cpp
            kernel/TeErrorLog.cpp kernel/TeErrorMessage.cpp
            kernel/TeException.cpp kernel/TeExternalTheme.cpp
            kernel/TeFragmentation.cpp
            kernel/TeGTFactory.cpp kernel/TeGTParams.cpp
            kernel/TeGeometricTransformation.cpp kernel/TeGeometry.cpp
            kernel/TeGeometryAlgorithms.cpp kernel/TeGroupingAlgorithms.cpp
            kernel/TeImportRaster.cpp kernel/TeInitRasterDecoders.cpp
            kernel/TeIntersector.cpp
            kernel/TeLayer.cpp kernel/TeLegendEntry.cpp
            kernel/TeMappedMemory.cpp kernel/TeMatrix.cpp
            kernel/TeMetaModelCache.cpp kernel/TeMultiGeometry.cpp
            kernel/TeMutex.cpp
            kernel/TeNeighbours.cpp kernel/TeNetwork.cpp
            kernel/TeOverlay.cpp kernel/TeOverlayUtils.cpp
            kernel/TeProgress.cpp kernel/TeProject.cpp
            kernel/TeProjection.cpp kernel/TeProjectiveGT.cpp
            kernel/TeProjectiveGTFactory.cpp
            kernel/TeProxMatrixConstructionStrategy.cpp
            kernel/TeProxMatrixImplementation.cpp
            kernel/TeProxMatrixStrategies.cpp
            kernel/TeQuerier.cpp kernel/TeQuerierDB.cpp
            kernel/TeQuerierDBStr1.cpp kernel/TeQuerierDBStr2.cpp
            kernel/TeQuerierDBStr3.cpp kernel/TeQuerierImpl.cpp
            kernel/TeQuerierParams.cpp
            kernel/TeRaster.cpp kernel/TeRasterMemManager.cpp
            kernel/TeRasterParams.cpp kernel/TeRasterRemap.cpp
            kernel/TeRasterTransform.cpp kernel/TeRepresentation.cpp
            kernel/TeSTElementSet.cpp kernel/TeSTEvent.cpp
            kernel/TeSTInstance.cpp kernel/TeSpatialOperations.cpp
            kernel/TeStdFile.cpp
            kernel/TeTable.cpp kernel/TeTemporalSeries.cpp
            kernel/TeTheme.cpp kernel/TeThread.cpp
            kernel/TeThreadFunctor.cpp kernel/TeThreadSignal.cpp
            kernel/TeTime.cpp kernel/TeTimeInterval.cpp
            kernel/TeTin.cpp
            kernel/TeUtils.cpp
            kernel/TeVectorRemap.cpp kernel/TeViewNode.cpp
            kernel/TeVisual.cpp
            kernel/lexTemporal.cpp kernel/yyTemporal.cpp

)
foreach( file ${_terralib_srcs} )
  list( APPEND terralib_srcs terralib/${file} )
endforeach()

set( terralib_srcs ${terralib_srcs} PARENT_SCOPE )

# libterralib_la_CXXFLAGS	= -DNO_HUGE -DXTFUNCPROTO -DLITTLEENDIAN -DS4LINUX -DS4MDX -DS4NO_STRNICMP -DXK_MISCELLANY -DNO_TEJPEG -DNO_TETIFF
# AM_CPPFLAGS		= -I$(top_srcdir)/src/terralib -I$(top_srcdir)/src/terralib/kernel -I$(top_srcdir)/src/terralib/utils

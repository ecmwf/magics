set( _basic_srcs
BasicGraphicsObject.cc
BasicGraphicsObject.h
BasicSceneObject.cc
BasicSceneObject.h
BasicSceneVisitor.cc
BasicSceneVisitor.h
FortranMagics.cc
FortranMagics.h
HistoVisitor.cc
HistoVisitor.h
ImportObjectHandler.cc
ImportObjectHandler.h
Layer.cc
Layer.h
LayoutManager.cc
LayoutManager.h
LegendVisitor.cc
LegendVisitor.h
MagicsEvent.h
MagicsObserver.h
MagnifierVisitor.cc
MagnifierVisitor.h
MultiVisdef.cc
MultiVisdef.h
ParameterSettings.cc
ParameterSettings.h
PreviewVisitor.cc
PreviewVisitor.h
RootSceneNode.cc
RootSceneNode.h
SceneNode.cc
SceneNode.h
SceneVisitor.cc
SceneVisitor.h
TagHandler.cc
TagHandler.h
TextVisitor.cc
TextVisitor.h
ViewNode.cc
ViewNode.h
Visdef.h
VisualAction.cc
VisualAction.h
WebFormat.cc
WebFormat.h
XmlMagics.cc
XmlMagics.h
)
if ( metview )
	list (APPEND metview_include 
			basic/MagicsObserver.h 
			basic/MagicsEvent.h
			basic/RootSceneNode.h
			basic/BasicSceneObject.h
			basic/BasicGraphicsObject.h
			basic/DisplayManager.h
			basic/Layer.h
			basic/SceneVisitor.h 
			basic/XmlMagics.h 
			basic/LegendVisitor.h 
			basic/Visdef.h 
			)
	set( metview_include ${metview_include} PARENT_SCOPE )
endif()

foreach( file ${_basic_srcs} )
  list( APPEND basic_srcs basic/${file} )
endforeach()

set( basic_srcs ${basic_srcs} PARENT_SCOPE )

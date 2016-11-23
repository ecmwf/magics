#!/usr/bin/env python 
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from xml.sax.handler import ContentHandler
from xml.sax import make_parser
from types import *

import sys


if(len(sys.argv) != 3) :
    print "\n\tYou need to give 2 input parameters:"
    print "\n\t  %s source.xml target.odt\n" % sys.argv[0]
    sys.exit()


class ObjectHandler(ContentHandler):
    name = ""
    classname = ""
    param = ""
    type = ""
    default=""

    def startElement(self, name, attrs):
        if (name == "magics") :
            definition.write('<?xml version="1.0" encoding="UTF-8"?>\n');
            definition.write('<office:document-content xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0" xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0" xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0" xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0" xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0" xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0" xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0" xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0" xmlns:math="http://www.w3.org/1998/Math/MathML" xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0" xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0" xmlns:ooo="http://openoffice.org/2004/office" xmlns:ooow="http://openoffice.org/2004/writer" xmlns:oooc="http://openoffice.org/2004/calc" xmlns:dom="http://www.w3.org/2001/xml-events" xmlns:xforms="http://www.w3.org/2002/xforms" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:rpt="http://openoffice.org/2005/report" xmlns:of="urn:oasis:names:tc:opendocument:xmlns:of:1.2" xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:grddl="http://www.w3.org/2003/g/data-view#" xmlns:tableooo="http://openoffice.org/2009/table" xmlns:field="urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0" xmlns:formx="urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0" xmlns:css3t="http://www.w3.org/TR/css3-text/" office:version="1.2" grddl:transformation="http://docs.oasis-open.org/office/1.2/xslt/odf2rdf.xsl">\n');
            definition.write('<office:scripts/>\n');
            definition.write('<office:font-face-decls>\n');
            definition.write('</office:font-face-decls>\n');
            definition.write('<office:automatic-styles>\n');
            definition.write('</office:automatic-styles>\n');
            definition.write('<office:body>\n');
            definition.write('<office:text>\n');
        elif (name == "class"):
            self.classname = attrs.get("name")
            definition.write('<text:p text:style-name="Standard">%s : Parameters</text:p>\n' % self.classname);
            definition.write('<table:table table:name="%sParameterTable" table:style-name="ParameterTable">\n' % self.classname);
            definition.write('<table:table-column table:style-name="%sParameterTable.A" table:number-columns-repeated="4"/>\n' % self.classname);
            definition.write('<table:table-header-rows>\n');
            definition.write('<table:table-row>\n');
            definition.write('<table:table-cell office:value-type="string">\n <text:p text:style-name="Para_Table_Heading">Parameter</text:p>\n</table:table-cell>\n');
            definition.write('<table:table-cell office:value-type="string">\n <text:p text:style-name="Para_Table_Heading">Type</text:p>\n</table:table-cell>\n');
            definition.write('<table:table-cell office:value-type="string">\n <text:p text:style-name="Para_Table_Heading">Function</text:p>\n</table:table-cell>\n');
            definition.write('<table:table-cell office:value-type="string">\n <text:p text:style-name="Para_Table_Heading">Default</text:p>\n</table:table-cell>\n');
            definition.write('</table:table-row>\n</table:table-header-rows>\n');
        elif (name == "parameter"):
            if (attrs.get("implemented") == 'no'):
                return
            if (attrs.get("visible") == 'no'):
                return
            self.param   = attrs.get("name")
            self.type    = attrs.get("from")
            self.default = attrs.get("default")


    def endElement(self, name):
        if (name == "magics") :
            definition.write('<text:p text:style-name="Standard"/>\n</office:text>\n</office:body>\n</office:document-content>\n')
        if (name == "class"):
            definition.write('</table:table>\n')
        if (name == "documentation"):
            definition.write('<table:table-row>\n'); 
            definition.write(' <table:table-cell table:style-name="ParameterTable.A1" office:value-type="string">\n'); 
            definition.write('  <text:p text:style-name="Para_Table_Contents">%s</text:p>\n' % self.param); 
            definition.write(' </table:table-cell>\n'); 
            definition.write(' <table:table-cell table:style-name="ParameterTable.A2" office:value-type="string">\n'); 
            definition.write('  <text:p text:style-name="Para_Table_Contents">%s</text:p>\n'% self.type); 
            definition.write(' </table:table-cell>\n'); 
            definition.write(' <table:table-cell table:style-name="ParameterTable.A3" office:value-type="string">\n'); 
            definition.write('  <text:p text:style-name="Para_Table_Contents">%s</text:p>\n'% self.text); 
            definition.write(' </table:table-cell>\n'); 
            definition.write(' <table:table-cell table:style-name="ParameterTable.A4" office:value-type="string">\n'); 
            definition.write('  <text:p text:style-name="Para_Table_Contents">%s</text:p>\n' % self.default); 
            definition.write(' </table:table-cell>\n'); 
            definition.write('</table:table-row>\n'); 
            
    def characters(self, content):
        self.text = content       


object = ObjectHandler()
saxparser = make_parser()
saxparser.setContentHandler(object)

datasource = open(sys.argv[1], "r")
definition = open(sys.argv[2], "w")
saxparser.parse(datasource)
definition.close()

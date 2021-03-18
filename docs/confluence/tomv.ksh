#!/bin/ksh

from=$1
to=$2

from=../src/xml
to=../../metview/share/metview/etc

mkdir -p xml

echo From: $from
echo To:   $to

for t in `ls $from/*.xml`
do
	echo $t
	python xml2split.py $t
done

######################################
#
#    D R I V E R S
#
echo ""
echo "Drivers ..."
for i in PostScript SVG KML Cairo
do
	echo "  Driver: $i"
	if [[ $i = "PostScript" ]];then
		p4 edit ${to}/PSOutputDef ${to}/EPSOutputDef
        python xml2mv.py ${from}/${i}Driver.xml ${to}/PSOutputDef  PSOUTPUT
        python xml2mv.py ${from}/${i}Driver.xml ${to}/EPSOutputDef EPSOUTPUT
	elif [[ $i = "Cairo" ]];then
		p4 edit ${to}/PNGOutputDef ${to}/PDFOutputDef
        python xml2mv.py ${from}/${i}Driver.xml ${to}/PDFOutputDef PDFOUTPUT
        python xml2mv.py ${from}/${i}Driver.xml ${to}/PNGOutputDef PNGOUTPUT
	else
        p4 edit ${to}/${i}OutputDef
        pyth
	fi
done

echo "Revert unchanged files"
p4 revert -a ${to}/*OutputDef

######################################
#
#    MSYMB 
#
echo ""
python xml2mv.py xml/SymbolPlotting.xml MSYMBDef MSYMB > MSYMBRules
p4 edit $to/MSYMBDef
p4 edit $to/MSYMBRules
mv MSYMBDef $to
mv xml/MSYMBRules $to
echo "Submit if changed" 
p4 submit -d "Refresh Icon def and rules"  $to/MSYMBDef
p4 submit -d "Refresh Icon def and rules"  $to/MSYMBRules



# MCONT -> Contour.xml
###python xml2mv.py xml/Contour.xml > xml/MCONTRules
###p4 edit $to/MCONTDef
###p4 edit $to/MCONTRules
###cp xml/MCONTDef $to
###cp xml/MCONTRules $to
###p4 submit -d "Refresh Icon def and rules"  $to/MCONTDef
###p4 submit -d "Refresh Icon def and rules"  $to/MCONTRules

# MCOAST -> Coastlines.xml
###python xml2mv.py xml/Coastlines.xml > xml/MCOASTRules
###p4 edit $to/MCOASTDef
###p4 edit $to/MCOASTRules
###cp xml/MCOASTDef $to
###cp xml/MCOASTRules $to
###p4 submit -d "Refresh Icon def and rules"  $to/MCOASTDef
###p4 submit -d "Refresh Icon def and rules"  $to/MCOASTRules


# MSYMB -> SymbolPlotting.xml

# MWIND -> Wind.xml
###python xml2mv.py xml/Wind.xml > xml/MWINDRules
###p4 edit $to/MWINDDef
###p4 edit $to/MWINDRules
###cp xml/MWINDDef $to
###cp xml/MWINDRules $to
###p4 submit -d "Refresh Icon def and rules"  $to/MWINDDef
###p4 submit -d "Refresh Icon def and rules"  $to/MWINDRules


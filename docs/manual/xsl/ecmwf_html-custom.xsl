<?xml version='1.0'  encoding="iso-8859-1" ?> 
<!DOCTYPE xsl:stylesheet
[
<!ENTITY nbsp "&#160;">
<!ENTITY copy "&#169;">
]>

<xsl:stylesheet  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"  version="1.0"> 

  <xsl:import href="/usr/local/apps/docbook/docbook-xsl-ecmwf/html.xsl"/> 


<xsl:template name="chunk-element-content">
  <xsl:param name="prev"/>
  <xsl:param name="next"/>
  <xsl:param name="nav.context"/>
  <xsl:param name="content">
    <xsl:apply-imports/>
  </xsl:param>


<html>
<head profile="http://purl.org/metadata/dublin_core">
<!-- ECMWF: Standard External Template -->
<!-- ECMWF: Version 1.01 -->
<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
<META content="$Author: sy8 $" name="DC.Creator" />
<!-- #BeginEditable "meta" --> 
<META content="ECMWF webgroup" name="DC.Publisher" />
<META content="webgroup documents" name="DC.Subject" />
<!-- #EndEditable --> <!-- #BeginEditable "metalink" --> 
<!-- Users insert meta LINK tags here -->
<!-- #EndEditable --> <!-- #BeginEditable "doctitle" --> 
	      <title><xsl:value-of select="./title"/></title>
<!-- #EndEditable --> <!-- #BeginEditable "customstyle" --> 
<style type="text/css">
</style>
<!-- #EndEditable --> 
<script type="text/javascript" language="JavaScript" src="/assets/js/ecmwf.js">
</script>
<link rel="stylesheet" href="/assets/css/ecmwf.css" type="text/css" />
</head>
<body bgcolor="#FFFFF2" text="#000000">
<a name="top"></a> 
<table summary="Formatting Table: Page Layout" width="750" border="0" cellspacing="0" cellpadding="0" bgcolor="#FFFFF2">
  <tr> 
    <td bgcolor="#CCCE9B" align="left" valign="middle"><a href="/"><img src="/assets/images/text_logo.jpg" border="0" alt="Home page" width="136" height="26" align="middle" /></a></td>
    <td bgcolor="#CCCE9B">&nbsp;</td>
    <td bgcolor="#CCCE9B" valign="middle"> 
     <form name="search" method="post" action="/cgi-bin/htsearch">
        <input type="hidden" name="restrict" value="" />
        <table summary="Formatting Table: Actions Menu" border="0" cellspacing="4" cellpadding="0" align="right">
          <tr> 
            <td><a href="/" class="menuitem">Home</a></td>
            <td>&nbsp;</td>
            <td><a href="/your/d/room/" class="menuitem">Your Room</a></td>
            <td>&nbsp;</td>
            <td><a href="/tools/login/" class="menuitem">Login</a></td>
            <td>&nbsp;</td>
            <td><a href="/about/contact/" class="menuitem">Contact</a></td>
            <td>&nbsp;</td>
            <td><a href="/about/feedback/" class="menuitem">Feedback</a></td>
            <td>&nbsp;</td>
            <td><a href="/about/sitemap/" class="menuitem">Site Map</a></td>
            <td>&nbsp;</td>
            <td><a href="/tools/qsearch/" class="menuitem">Search:</a></td>
            <td valign="bottom"> 
              <input type="hidden" name="exclude"  value="" />
              <input type="hidden" name="config"   value="wmss" />
              <input type="text" name="words" maxlength="128" size="8" style="height:20px; width:80px" />
            </td>
            <td>&nbsp;</td>
          </tr>
        </table>
      </form>
    </td>
  </tr>
  <tr> 
    <td align="center" valign="middle" width="136" bgcolor="#EEEEDE"><a href="http://www.ecmwf.int/samples/d/banner/page.html"><img alt="Discover this product" border="0" width="136" height="64" src="http://www.ecmwf.int/samples/d/banner/image.jpg" /></a></td>
    <td bgcolor="#EEEEDE" >&nbsp;</td>
    <td align="right" bgcolor="#EEEEDE" > 
      <table summary="Formatting Table: Top Navigation" cellpadding="2" cellspacing="0" border="0" width="100%" >
        <tr bgcolor="#EEEEDE"> 
          <td valign="top" bgcolor="#EEEEDE" width="16%"> <a href="/about/"><span class="topnav">About</span></a><br />
            <a class="topnav1" href="/about/overview/">Overview</a><br />
            <a class="topnav1" href="/about/contact/">Getting here</a><br />
            <a class="topnav1" href="/about/committees/">Committees</a> 
          </td>
          <td valign="top" bgcolor="#EEEEDE" width="16%"> <a href="/products/"><span class="topnav">Products</span></a><br />
            <a class="topnav1" href="/products/forecasts/">Forecasts</a><br />
            <a class="topnav1" href="/products/data/">Order Data</a><br />
            <a class="topnav1" href="/products/data/software/">Order Software</a><br />
          </td>
          <td valign="top" bgcolor="#EEEEDE" width="16%"> <a href="/services/"><span class="topnav">Services</span></a><br />
            <a class="topnav1" href="/services/computing/">Computing</a><br />
            <a class="topnav1" href="/services/archive/">Archive</a><br />
            <a class="topnav1" href="/services/prepifs/">PrepIFS</a><br />
          </td>
          <td valign="top" bgcolor="#EEEEDE" width="16%"> <a href="/research/"><span class="topnav">Research</span></a><br />
            <a class="topnav1" href="/research/ifs/">Modelling</a><br />
            <a class="topnav1" href="/research/era/">Reanalysis</a><br />
            <a class="topnav1" href="/research/seasonal/">Seasonal</a><br />
          </td>
          <td valign="top" bgcolor="#EEEEDE" width="16%"> <a href="/publications/"><span class="topnav">Publications</span></a><br />
            <a class="topnav1" href="/publications/newsletters/">Newsletters</a><br />
            <a class="topnav1" href="/publications/manuals/">Manuals</a><br />
            <a class="topnav1" href="/publications/library/">Library</a><br />
          </td>
          <td valign="top" bgcolor="#EEEEDE" width="20%"> <a href="/newsevents/"><span class="topnav">News&amp;Events</span></a><br />
            <a class="topnav1" href="/newsevents/employment/">Employment</a><br />
            <a class="topnav1" href="/newsevents/calendar/">Calendar</a><br />
            <a class="topnav1" href="/newsevents/itt/">Open Tenders</a><br />
          </td>
        </tr>
      </table>
    </td>
  </tr>
  <tr> 
    <td  align="left" valign="middle" width="136" bgcolor="#CCCE9B"><!-- #BeginEditable "spare1" -->&nbsp;<!-- #EndEditable --></td>
    <td bgcolor="#CCCE9B"  >&nbsp;</td>
    <td valign="middle" bgcolor="#CCCE9B" > 
      <table summary="Formatting Table: Location header" cellpadding="0" cellspacing="0" border="0" width="100%">
        <tr> 
          <td align="left"><span class="location">
            <!-- #BeginEditable "location" --> 
                      <xsl:call-template name="ecmwf.location">
                        <xsl:with-param name="nav.context" select="$nav.context"/>
                      </xsl:call-template>
            <!-- #EndEditable -->&nbsp;</span> 
          </td>
          <td align="right"> 
		<!-- #BeginEditable "topprevnext" -->
                      <xsl:call-template name="ecmwf.topprevnext">
                        <xsl:with-param name="prev" select="$prev"/>
                        <xsl:with-param name="next" select="$next"/>
                        <xsl:with-param name="up" select="parent::*"/>
                      </xsl:call-template>
		<!-- #EndEditable --> 
          </td>
        </tr>
      </table>
    </td>
  </tr>
  <tr> 
    <td align="left"><!-- #BeginEditable "spare2" -->&nbsp;<!-- #EndEditable --> 
    </td>
    <td>&nbsp;</td>
    <td> 
      <h1><!-- #BeginEditable "heading" --><!--<xsl:value-of select="./title"/>--><!-- #EndEditable --></h1>
    </td>
  </tr>
  <tr> 
    <td valign="top" width="136" background="/assets/images/map_backdrop.jpg"> 
      	<!-- #BeginEditable "submenu" -->
		<xsl:call-template name="ecmwf.submenu"/>
	<!-- #EndEditable --> &nbsp;<br />
    </td>
    <td>&nbsp;</td>
    <td  valign="top"> 


      <!-- #BeginEditable "content" -->
	      <xsl:copy-of select="$content"/>
      <!-- #EndEditable --> 

      <br />

      <!-- #BeginEditable "related" --> &nbsp; <!-- #EndEditable --> <br />

      <br />

    </td>
  </tr>
  <tr> 
    <td  bgcolor="#CCCE9B" valign="baseline"> 
      <table summary="Formatting Table: date footer" height="16" border="0" cellspacing="0" cellpadding="0">
        <tr> 
          <td><a href="#top"><img src="/assets/icons/up.gif" alt="Top of page" border="0" width="18" height="18" /></a></td>
          <td><span class="documentdate"><!-- #BeginEditable "version" --> 
            <!-- #BeginDate format:Ge1 -->autodate<!-- #EndDate -->
            <!-- #EndEditable --></span></td>
        </tr>
      </table>
    </td>
    <td valign="middle" bgcolor="#CCCE9B">&nbsp;</td>
    <td  bgcolor="#CCCE9B" align="right" valign="baseline"> 
      <table summary="Formatting table: main footer" width="100%" border="0" cellspacing="0" cellpadding="0">
        <tr align="right"> 
          <td align="left"><span class="info"><!-- #BeginEditable "info" -->&nbsp;&nbsp;<!-- #EndEditable --></span></td>
          <td bgcolor="#CCCE9B"><span class="editor"><a href="/tools/detailer/"><img alt="Page Details" src="/assets/icons/detailer.gif" border="0" width="18" height="18"/></a><!-- #BeginEditable "editor" -->&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<!-- #EndEditable --></span></td>
          <td bgcolor="#CCCE9B"><span class="copyright"><a href="/frontpage/copyright/">&copy; 
            ECMWF</a> </span></td>
          <td><!-- #BeginEditable "botprevnext" -->

              <xsl:call-template name="ecmwf.topprevnext">
                <xsl:with-param name="prev" select="$prev"/>
                <xsl:with-param name="next" select="$next"/>
                <xsl:with-param name="up"   select="parent::*"/>
              </xsl:call-template>
          
          <!-- #EndEditable --></td>
        </tr>
      </table>
    </td>
  </tr>
  <tr> 
    <td height="1" width="136" bgcolor="#CCCB9E"><img alt="shim" height="1" width="136" src="/assets/shim.gif" /></td>
    <td height="1" width="008" bgcolor="#CCCB9E"><img alt="shim" height="1" width="8" src="/assets/shim.gif" /></td>
    <td height="1" width="100%" bgcolor="#CCCB9E"><img alt="shim" height="1" width="160" src="/assets/shim.gif" /></td>
  </tr>
</table>
</body>
</html>

</xsl:template>
</xsl:stylesheet>

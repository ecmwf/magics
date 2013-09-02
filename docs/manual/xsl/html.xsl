<?xml version='1.0'  encoding="iso-8859-1" ?> 

<xsl:stylesheet  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"  version="1.0"> 

<!--  <xsl:import href="/usr/local/apps/docbook/docbook-xsl-ecmwf/w3ec2_html.xsl"/> -->
<!--    <xsl:import href="/usr/local/apps/docbook/docbook-xsl-ecmwf/ecmwf_html.xsl"/> -->
  <xsl:import href="ecmwf_html-custom_mod.xsl"/>  <!-- This is a dynamically-modified form of ecmwf_html-custom.xsl -->
<!--    <xsl:import href="./ecmwf-html.xsl"/>  -->


  <xsl:import href="en-custom-html.xsl"/>


  <xsl:param name="chapter.autolabel" select="0"/>
  <xsl:param name="section.autolabel" select="0"/>


  <!-- If we have a link to something that may exist both externally and also
       internally, then link to the internal one. This should be useful in the chapter
       of new features where we have such situations. Come to think of it though,
       we don't publish that chapter under HTML, so this is probably unnecessary. -->

  <xsl:param name="prefer.internal.olink" select="1"/>


 <!-- As of Feb 2009, this needs to be deactivated in order to get DocBook
      generation to work. It is not clear why, but perhaps something changed
      in the xsl docs that this one inherits?  -->
 <xsl:param name="tablecolumns.extension" select="0"/>



  <!-- Set the section depth to which we wish to generate an entry in the TOC -->

  <xsl:param name="toc.section.depth" select="1"/>


  <!-- Remove figures, tables etc from the TOC -->

  <xsl:param name="generate.toc">
  appendix toc,title
  article/appendix  nop
  article   toc,title
  book      toc,title
  chapter   toc,title
  part      toc,title
  preface   toc,title
  reference toc,title
  sect1     toc,title
  sect2     toc,title
  sect3     toc
  sect4     toc
  sect5     toc
  section   toc
  set       toc,title
  </xsl:param>

<xsl:param name="toc.title.p" select="1"/>
<xsl:param name="chunk.tocs.and.lots.has.title" select="1"></xsl:param>



  <!-- Remove the TM that appears after <productname> items by default -->

  <xsl:template match="productname">
    <xsl:call-template name="inline.charseq"/>
    <xsl:if test="@class != 'trade' or (@class = 'trade' and @role='showmark')">
      <xsl:call-template name="dingbat">
        <xsl:with-param name="dingbat" select="@class"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>



  <!-- Set the style for 'productname' and 'orgname' elements (references to MAGICS and ECMWF) -->

  <xsl:template match="productname">
    <xsl:call-template name="inline.boldseq"/>
  </xsl:template>

  <xsl:template match="orgname">
    <xsl:call-template name="inline.boldseq"/>
  </xsl:template>


  <!--  Application names should be in italics -->

  <xsl:template match="application">
    <xsl:call-template name="inline.italicseq"/>
  </xsl:template>



  <!-- Syntax highlight any source code -->

  <xsl:template match='comment'>
    <i class="hl-comment"><font color='green'><xsl:apply-templates/></font></i>
  </xsl:template>


  <xsl:template match='function'>
    <b class="hl-function"><font color='#000080'><xsl:apply-templates/></font></b>
  </xsl:template>


  <xsl:template match='parameter'>
    <code class="hl-function"><font color='#4040C0'><xsl:apply-templates/></font></code>
  </xsl:template>

  <xsl:template match='classname'>
    <code class="hl-function"><font color='#804060'><xsl:apply-templates/></font></code>
  </xsl:template>


  <!-- We reserve remarks for non-printing comments. Make them invisible. -->

  <xsl:template match="remark">
  </xsl:template>



  <!-- Turn on some borders and other attributes for special sections such
       as warnings and tips  -->

  <!--
  <xsl:template name="nongraphical.admonition">
    <hr/>
  </xsl:template>
  -->


<xsl:template name="nongraphical.admonition">
  <div class="{name(.)}">
    <xsl:if test="$admon.style">
      <xsl:attribute name="style">
        <xsl:value-of select="$admon.style"/>
      </xsl:attribute>
    </xsl:if>

    <hr/>
    <h3 class="title"><font color='#4040C0'>
      <xsl:call-template name="anchor"/>
      <xsl:if test="$admon.textlabel != 0 or title">
        <xsl:apply-templates select="." mode="object.title.markup"/>
      </xsl:if>
      </font>
    </h3>

    <xsl:apply-templates/>
    <hr/>
  </div>
</xsl:template>


  <!-- adjust how we place titles in relation to figures so that we avoid as far as
       possible having the title split from the figure -->
  
  <xsl:param name="formal.title.placement">
    figure after
    example before
    equation after
    table before
    procedure before
  </xsl:param>


<!-- Remove the bold (<b>) from figure headings. The original had <b> within
     the <p> tags -->
 
<xsl:template name="formal.object.heading">
  <xsl:param name="object" select="."/>
  <xsl:param name="title">
    <xsl:apply-templates select="$object" mode="object.title.markup">
      <xsl:with-param name="allow-anchors" select="1"/>
    </xsl:apply-templates>
  </xsl:param>

  <p class="title">
    <xsl:copy-of select="$title"/>
  </p>
</xsl:template>



<!-- Specify the names of the pages in the document hierarchy above
     the Magics++ user guide -->

<xsl:param name="ecmwf.parent.parent.parent.title">Magics</xsl:param>
<xsl:param name="ecmwf.parent.parent.parent.parent.title">Manuals</xsl:param>
<xsl:param name="ecmwf.parent.parent.parent.parent.parent.title">Publications</xsl:param>
<xsl:param name="ecmwf.parent.parent.parent.parent.parent.parent.title">Home</xsl:param>



</xsl:stylesheet>

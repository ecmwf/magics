<?xml version='1.0'  encoding="iso-8859-1" ?> 

<xsl:stylesheet  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                 xmlns:fo="http://www.w3.org/1999/XSL/Format"
                 xmlns:date="http://exslt.org/dates-and-times"
                 exclude-result-prefixes="date"
                 version="1.0">

<!--   <xsl:import href="/usr/local/apps/docbook/docbook-xsl-ecmwf/project_plans_fo.xsl"/> -->
<!--      <xsl:import href="/usr/local/apps/docbook/docbook-xsl/current/fo/docbook.xsl"/>  -->
   <xsl:import href="docbook-xsl/fo/docbook.xsl"/>
   <xsl:import href="en-custom-pdf.xsl"/>



  <xsl:param name="paper.type" select="'A4'"/> 
  <xsl:param name="draft.mode" select="'no'"/>
  <xsl:param name="fop.extensions" select="1"/>
  <xsl:param name="insert.link.page.number" select="'no'"/>
  <xsl:param name="insert.olink.page.number" select="'no'"/>

<xsl:attribute-set name="list.block.spacing">
  <xsl:attribute name="space-before.optimum">0.3em</xsl:attribute>
  <xsl:attribute name="space-before.minimum">0.2em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">0.4em</xsl:attribute>
  <xsl:attribute name="space-after.optimum">0.3em</xsl:attribute>
  <xsl:attribute name="space-after.minimum">0.2em</xsl:attribute>
  <xsl:attribute name="space-after.maximum">0.4em</xsl:attribute>
</xsl:attribute-set>


<xsl:attribute-set name="normal.para.spacing">
  <xsl:attribute name="space-before.optimum">0.5em</xsl:attribute>
  <xsl:attribute name="space-before.minimum">0.4em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">0.6em</xsl:attribute>
</xsl:attribute-set>




<!--
   <xsl:import href="/usr/local/apps/docbook/docbook-xsl-ecmwf/project_plans_fo.xsl"/>
   <xsl:import href="/usr/local/apps/docbook/docbook-xsl/current/fo/titlepage.templates.xsl"/>
-->
<!--  <xsl:import href="/usr/local/apps/docbook/docbook-xsl/docbook-xsl-1.68.1/fo/docbook.xsl"/> -->
<!--  <xsl:import href="./ecmwf-pdf.xsl"/> -->


<!--  <xsl:param name="shade.verbatim"    select="1"/> -->


  <!--
  <xsl:param name="draft.mode"            select="'yes'"></xsl:param>
  <xsl:param name="draft.watermark.image" select="'icons/draft.gif'"></xsl:param>
  -->
  


  <xsl:param name="double.sided" select="1"></xsl:param>
  <xsl:param name="marker.section.level" select="3" />


  <!-- Set our chapter/section numbering policy  -->
  
  <xsl:param name="chapter.autolabel" select="1"/>
  <xsl:param name="section.autolabel" select="0"/>

  <xsl:param name="olink.doctitle" select="1"/>


  <!-- failed attempt to change the chapter title from "Chapter N. xxx" to "Chapter N: xxx"
  <xsl:template match="chapter" mode="intralabel.punctuation">
    <xsl:text>:</xsl:text>
  </xsl:template> -->



  <!-- If we have a link to something that may exist both externally and also
       internally, then link to the internal one. This should be useful in the chapter
       of new features where we have such situations -->

  <xsl:param name="prefer.internal.olink" select="1"/>


  <!-- not setting keep.relative.image.uris can make life difficult when you 
       have xincludes which load graphics files. The path gets messed up. -->

  <xsl:param name="keep.relative.image.uris" select="1"/>


  <!-- Set this parameter to "1" to enable graphics beside the admonition
       text. This also removes the upper and lower borders around the text.
       Setting the extension allows the selection of one set of images over another
       according to type. -->

  <xsl:param name="admon.graphics"           select="0"/>
  <xsl:param name="admon.graphics.path">icons/</xsl:param>
  <xsl:param name="admon.graphics.extension" select="'.svg'"/>
  


  <!-- Make the horizontal rule below the page header red -->

  <xsl:param name="header.rule" select="1"/>

  <xsl:template name="head.sep.rule">
    <xsl:if test="$header.rule != 0">
      <xsl:attribute name="border-bottom-width">0.5pt</xsl:attribute>
      <xsl:attribute name="border-bottom-style">solid</xsl:attribute>
      <xsl:attribute name="border-bottom-color">red</xsl:attribute>
    </xsl:if>
  </xsl:template>

<!-- 
  <xsl:template name="user.head.content">
    <meta name="date">
      <xsl:attribute name="content">
        <xsl:call-template name="datetime.format">
          <xsl:with-param name="date" select="date:date-time()"/>
          <xsl:with-param name="format" select="'m/d/Y'"/>
        </xsl:call-template>
      </xsl:attribute>
    </meta>
  </xsl:template>
-->



<xsl:template match="revhistory" mode="titlepage.mode">
  <fo:block text-align="center">
    <xsl:call-template name="datetime.format">
      <xsl:with-param name="date" select="date:date-time()"/>
      <xsl:with-param name="format" select="'Y-m-d H:M'"/>
    </xsl:call-template>
  </fo:block>
</xsl:template>


  <!-- Remove the horizontal rule at the page footer -->

  <xsl:param name="footer.rule" select="0"/>



  <!-- Set the section depth to which we wish to generate an entry in the TOC -->

  <xsl:param name="toc.section.depth" select="2"/>

  <!-- Remove figures, tables etc from the TOC -->

<!--
<xsl:param name="generate.toc">
 book/chapter  toc,title
</xsl:param>
-->


  <xsl:param name="generate.toc">
  appendix toc,title
  article/appendix  nop
  article   toc,title
  book      nop
  chapter   toc,title
  part      toc,title
  preface   toc,title
  reference toc,title
  sect1     toc
  sect2     toc
  sect3     toc
  sect4     toc
  sect5     toc
  section   toc
  set       toc,title
  </xsl:param>


<!-- For the tutorial, we want to make some Table of Contents section headings bold. 
     In fact, it looks ok for the manual as well. -->

<xsl:template name="toc.line">
  <xsl:variable name="id">
    <xsl:call-template name="object.id"/>
  </xsl:variable>

  <xsl:variable name="label">
    <xsl:apply-templates select="." mode="label.markup"/>
  </xsl:variable>

  <fo:block xsl:use-attribute-sets="toc.line.properties"
            end-indent="{$toc.indent.width}pt"
            last-line-end-indent="-{$toc.indent.width}pt">
    <fo:inline keep-with-next.within-line="always">


      <!-- These next 2 bits are what we added to the original -->

      <xsl:variable name="level">
        <xsl:call-template name="section.level"/>
      </xsl:variable>

      <xsl:choose>
        <!--<xsl:when test="self::sect2">-->
        <xsl:when test="$level = 1">
          <xsl:attribute name="font-weight">bold</xsl:attribute>
        </xsl:when>
      </xsl:choose>

      <!-- end of bits that we added to the original -->


      <fo:basic-link internal-destination="{$id}">
        <xsl:if test="$label != ''">
          <xsl:copy-of select="$label"/>
          <xsl:value-of select="$autotoc.label.separator"/>
        </xsl:if>
        <xsl:apply-templates select="." mode="titleabbrev.markup"/>
      </fo:basic-link>
    </fo:inline>
    <fo:inline keep-together.within-line="always">
      <xsl:text> </xsl:text>
      <fo:leader leader-pattern="dots"
                 leader-pattern-width="3pt"
                 leader-alignment="reference-area"
                 keep-with-next.within-line="always"/>
      <xsl:text> </xsl:text> 
      <fo:basic-link internal-destination="{$id}">
        <fo:page-number-citation ref-id="{$id}"/>
      </fo:basic-link>
    </fo:inline>
  </fo:block>
</xsl:template>





<!-- There are cases where we do not want the print the name of the chapter in
     an olink cross-reference. Perhaps this could have been done with template matching,
     but I could not manage to do it that way. Instead, the approach was to copy the
     olink.document.citation template from common/olink.xsl and add just one clause
     to the end of the 'test' - we only give the citation if the 'type' attribute
     is not 'noref'. We set this attribute in 'generate_magml_tables.pl - it is not
     be used by DocBook so we can use it for our own purposes here. -->

<xsl:template name="olink.document.citation">
  <xsl:param name="olink.key" select="''"/>
  <xsl:param name="olink.lang" select="'en'"/>
  <xsl:param name="target.database"/>
  <xsl:param name="xrefstyle">


  <!--  experiment

  <xsl:variable name="type">
    <xsl:value-of select="@type"/>
  </xsl:variable>

  <xsl:message>
    <xsl:text>yyyyyyy</xsl:text>
    <xsl:value-of select="$type"/>
  </xsl:message>
  -->


    <xsl:choose>
      <xsl:when test="@role and not(@xrefstyle) 
                      and $use.role.as.xrefstyle != 0">
        <xsl:value-of select="@role"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@xrefstyle"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:param>

  <xsl:variable name="page">
    <xsl:for-each select="$target.database" >
      <xsl:value-of 
             select="key('targetptr-key', $olink.key)/@page" />
    </xsl:for-each>
  </xsl:variable>

  <xsl:variable name="targetdoc">
    <xsl:value-of select="substring-before($olink.key, '/')"/>
  </xsl:variable>

  <xsl:variable name="targetptr">
    <xsl:value-of 
          select="substring-before(substring-after($olink.key, '/'), '/')"/>
  </xsl:variable>

  <!-- Don't add docname if pointing to root element -->
  <xsl:variable name="rootptr">
    <xsl:for-each select="$target.database" >
      <xsl:value-of 
             select="key('targetdoc-key', $targetdoc)/div[1]/@targetptr" />
    </xsl:for-each>
  </xsl:variable>

  <xsl:variable name="docname">
    <xsl:for-each select="$target.database" >
      <xsl:value-of 
             select="key('targetdoc-key', $targetdoc)/div[1]/ttl" />
    </xsl:for-each>
  </xsl:variable>

  <xsl:if test="not(starts-with(normalize-space($xrefstyle), 'select:') 
              and (contains($xrefstyle, 'docname')))
              and ($olink.doctitle = 'yes' or $olink.doctitle = '1')
              and $current.docid != '' 
              and $rootptr != $targetptr
              and $current.docid != $targetdoc
              and $docname != ''
              and @type != 'noref'">
    <xsl:call-template name="substitute-markup">
      <xsl:with-param name="template">
        <xsl:call-template name="gentext.template">
          <xsl:with-param name="name" select="'olink.document.citation'"/>
          <xsl:with-param name="context" select="'xref'"/>
          <xsl:with-param name="lang" select="$olink.lang"/>
        </xsl:call-template>
      </xsl:with-param>
      <xsl:with-param name="docname" select="$docname"/>
      <xsl:with-param name="pagenumber" select="$page"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>










  <!-- Set the admonition text (Important/Tip/Warning etc) colour. See  -->

  <xsl:attribute-set name="admonition.title.properties">
    <xsl:attribute name="color">blue</xsl:attribute>
  </xsl:attribute-set>



  <!-- Alter the body text indent. The values are negative because the titles
       are negatively offset from the text.
       Note that this seems to be an old way of doing it. A more modern way is to set
       body.start.indent, but this does not work with this xsl processor. -->
 
 <xsl:param name="title.margin.left">
  <xsl:choose>
    <xsl:when test="$fop.extensions != 0">-0.1in</xsl:when>
    <xsl:when test="$passivetex.extensions != 0">-0.1in</xsl:when>
    <xsl:otherwise>0pt</xsl:otherwise>
  </xsl:choose>
</xsl:param> 
 



  <!-- Set the heights of the section level titles and page-break properties -->

  <xsl:attribute-set name="section.title.level1.properties">
    <xsl:attribute name="break-before">page</xsl:attribute>
    <xsl:attribute name="font-size">
        <xsl:value-of select="$body.font.master * 1.4"/>
        <xsl:text>pt</xsl:text>
    </xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="section.title.level2.properties">
    <xsl:attribute name="font-size">
      <xsl:value-of select="$body.font.master * 1.1"/>
      <xsl:text>pt</xsl:text>
    </xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="section.title.level3.properties">
    <xsl:attribute name="font-size">
      <xsl:value-of select="$body.font.master * 0.9"/>
      <xsl:text>pt</xsl:text>
    </xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="section.title.level4.properties">
    <xsl:attribute name="font-size">
      <xsl:value-of select="$body.font.master * 0.8"/>
      <xsl:text>pt</xsl:text>
    </xsl:attribute>
  </xsl:attribute-set>



  <xsl:attribute-set name="section.title.properties">
  </xsl:attribute-set>
  
  <xsl:attribute-set name="section.properties">
  </xsl:attribute-set>
  

<!--
  <xsl:attribute-set name="section.level1.properties">
    <xsl:attribute name="break-after">page</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="section.level2.properties">
  </xsl:attribute-set>

  <xsl:attribute-set name="section.level3.properties">
  </xsl:attribute-set>

  <xsl:attribute-set name="section.level4.properties">
  </xsl:attribute-set>
-->


  <xsl:param name="chunk.section.depth" select="2"/>


<!--
  <xsl:template name="book.titlepage.separator">
  </xsl:template>
-->



<!--
<xsl:attribute-set name="section.level1.properties">
  <xsl:attribute name="break-before">auto</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="section.level2.properties">
  <xsl:attribute name="break-before">auto</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="section.level3.properties">
  <xsl:attribute name="break-before">auto</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="section.level4.properties">
  <xsl:attribute name="break-before">auto</xsl:attribute>
</xsl:attribute-set>
-->




  <!-- Remove the TM that appears after <productname> items by default 

  <xsl:template match="productname">
    <xsl:call-template name="inline.charseq"/>
    <xsl:if test="@class != 'trade' or (@class = 'trade' and @role='showmark')">
      <xsl:call-template name="dingbat">
        <xsl:with-param name="dingbat" select="@class"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>-->



  <!-- Turn on page numbering for cross-references -->

  <xsl:param name="insert.xref.page.number">yes</xsl:param>


  <!-- Set the table cell padding attributes -->

  <xsl:attribute-set name="table.cell.padding">
    <xsl:attribute name="padding-left">2pt</xsl:attribute>
    <xsl:attribute name="padding-right">4pt</xsl:attribute>
    <xsl:attribute name="padding-top">1.5pt</xsl:attribute>
    <xsl:attribute name="padding-bottom">1.5pt</xsl:attribute>
  </xsl:attribute-set>



  <!-- Table/figure numbering should be, for example, 4-1 instead of 4.1 -->

  <xsl:template match="figure|table|example|procedure|chapter" mode="intralabel.punctuation">
      <xsl:text>-</xsl:text>
  </xsl:template>



  <!-- When we use literallayout to force honour line-breaks, xsl will
       pad the item unless we stop it (which we do here) -->

  <xsl:attribute-set name="verbatim.properties">
    <xsl:attribute name="space-before.minimum">0em</xsl:attribute>
    <xsl:attribute name="space-before.optimum">0em</xsl:attribute>
    <xsl:attribute name="space-before.maximum">0em</xsl:attribute>
    <xsl:attribute name="space-after.minimum">0em</xsl:attribute>
    <xsl:attribute name="space-after.optimum">0em</xsl:attribute>
    <xsl:attribute name="space-after.maximum">0em</xsl:attribute>
  </xsl:attribute-set>


  <!-- Turn on some borders and other attributes for special sections such
       as warnings and tips  -->

  <xsl:template name="nongraphical.admonition">
    <xsl:variable name="id">
      <xsl:call-template name="object.id"/>
    </xsl:variable>

    <fo:block space-before.minimum="0.8em"
              space-before.optimum="1em"
              space-before.maximum="1.2em"
              start-indent="0.25in"
              end-indent="0.25in"
              border-top="0.5pt solid black"
              border-bottom="0.5pt solid black"
              padding-top="4pt"
              padding-bottom="4pt"
              id="{$id}">
      <xsl:if test="$admon.textlabel != 0 or title">
        <fo:block keep-with-next='always'
                  xsl:use-attribute-sets="admonition.title.properties">
           <xsl:apply-templates select="." mode="object.title.markup"/>
        </fo:block>
      </xsl:if>

      <fo:block xsl:use-attribute-sets="admonition.properties">
        <xsl:apply-templates/>
      </fo:block>
    </fo:block>
  </xsl:template>



  <!-- Set the style for 'productname' and 'orgname' elements
       (references to MAGICS and ECMWF) -->

  <xsl:template match="productname">
    <xsl:call-template name="inline.boldseq"/>
  </xsl:template>

  <xsl:template match="orgname">
    <xsl:call-template name="inline.boldseq"/>
  </xsl:template>




  <!-- Set the style for 'application' elements -->

  <xsl:template match="application">
    <xsl:call-template name="inline.italicseq"/>
  </xsl:template>


  <!-- Set the style for 'function' elements -->

  <xsl:template match="function">
    <fo:inline color="rgb(0,0,128)">
      <xsl:choose>
        <xsl:when test="$function.parens != '0'
                        and (parameter or function or replaceable)">
          <xsl:variable name="nodes" select="text()|*"/>
          <xsl:call-template name="inline.boldmonoseq">
            <xsl:with-param name="content">
              <xsl:call-template name="simple.xlink">
                <xsl:with-param name="content">
                  <xsl:apply-templates select="$nodes[1]"/>
                </xsl:with-param>
              </xsl:call-template>
            </xsl:with-param>
          </xsl:call-template>
          <xsl:text>(</xsl:text>
          <xsl:apply-templates select="$nodes[position()>1]"/>
          <xsl:text>)</xsl:text>
        </xsl:when>
        <xsl:otherwise>
         <xsl:call-template name="inline.boldmonoseq"/>
        </xsl:otherwise>
      </xsl:choose>
    </fo:inline>
  </xsl:template>



  <!-- Set the styles for other source code elements -->

  <xsl:template match="parameter">
    <fo:inline color="rgb(64,64,192)">
      <xsl:call-template name="inline.monoseq"/>
    </fo:inline>
  </xsl:template>


  <xsl:template match="comment">
    <fo:inline color="rgb(32,128,32)">
      <xsl:call-template name="inline.italicmonoseq"/>
    </fo:inline>
  </xsl:template>


  <xsl:template match="classname">
    <fo:inline color="rgb(128,64,96)">
      <xsl:call-template name="inline.boldmonoseq"/>
    </fo:inline>
  </xsl:template>


  <!-- 
  <xsl:template match="function/parameter" priority="2">
    <fo:block>
    <xsl:call-template name="inline.boldmonoseq"/>
    <xsl:if test="following-sibling::*">
      <xsl:text>, </xsl:text>
    </xsl:if>
    </fo:block>
  </xsl:template>
  -->


  <!-- We reserve remarks for non-printing comments. Make them invisible. -->

  <xsl:template match="remark">
  </xsl:template>

<!-- 
  <xsl:template match="remark">
    <fo:inline color="rgb(200,64,64)">
      <xsl:call-template name="inline.italicmonoseq"/>
    </fo:inline>
  </xsl:template>
-->




  <xsl:attribute-set name="chapter.properties">
  </xsl:attribute-set>




  <xsl:attribute-set name="chapter.title.properties">
    <xsl:attribute name="break-after">auto</xsl:attribute>
    <xsl:attribute name="break-before">auto</xsl:attribute>
  </xsl:attribute-set>




  <xsl:attribute-set name="section.level0.properties">
    <xsl:attribute name="break-before">auto</xsl:attribute>
    <xsl:attribute name="break-after">auto</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="section.level1.properties">
    <xsl:attribute name="break-before">auto</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="section.level2.properties">
    <xsl:attribute name="break-before">auto</xsl:attribute>
  </xsl:attribute-set>


  <xsl:attribute-set name="section.level1.properties">
    <xsl:attribute name="break-after">auto</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="section.level2.properties">
    <xsl:attribute name="break-after">auto</xsl:attribute>
  </xsl:attribute-set>



<xsl:attribute-set name="table.properties">
  <xsl:attribute name="keep-together.within-column">always</xsl:attribute>
  <xsl:attribute name="keep-together.within-line">always</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="formal.object.properties">
   <xsl:attribute name="keep-together.within-column">always</xsl:attribute>
   <xsl:attribute name="keep-together.within-line">always</xsl:attribute>
</xsl:attribute-set>




<xsl:template match="row">
  <xsl:param name="spans"/>

  <fo:table-row>
    <xsl:attribute name="keep-together.within-column">always</xsl:attribute>
    <xsl:call-template name="anchor"/>

    <xsl:apply-templates select="(entry|entrytbl)[1]">
      <xsl:with-param name="spans" select="$spans"/>
    </xsl:apply-templates>
  </fo:table-row>

  <xsl:if test="following-sibling::row">
    <xsl:variable name="nextspans">
      <xsl:apply-templates select="(entry|entrytbl)[1]" mode="span">
        <xsl:with-param name="spans" select="$spans"/>
      </xsl:apply-templates>
    </xsl:variable>

    <xsl:apply-templates select="following-sibling::row[1]">
      <xsl:with-param name="spans" select="$nextspans"/>
    </xsl:apply-templates>
  </xsl:if>
</xsl:template>




<xsl:template name="chapter.titlepage.separator">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" break-after="auto"/>
</xsl:template>




  
  <!-- Set the table and figure numbering so that it includes the chapter number  -->
<!--   
  <xsl:template match="figure" mode="label.markup">
    <xsl:number level="multiple" count="chapter|figure" format="1.1"/>
  </xsl:template>
-->
<!--
  <xsl:template match="table" mode="label.markup">
    <xsl:number level="multiple" count="chapter|table" format="1-01"/>
  </xsl:template>
  
  <xsl:template match="figure|table|example|procedure" mode="intralabel.punctuation">
      <xsl:text> - </xsl:text>
  </xsl:template>
-->


  <!--  Here we specify whether figure, etc, titles are placed before or after
        the objects themselves. It seems sensible to have the titles after the
        objects in order to reduce instances where the object and title are
        separated (which can happen with large figures). -->

  <xsl:param name="formal.title.placement">
    figure after
    example before
    equation after
    table before
    procedure before
  </xsl:param>



<!--
  <xsl:template match="figure|table|example|procedure" mode="label.markup">
    <xsl:number level="multiple" count="chapter|figure" format="1.1"/>
  </xsl:template>
 -->
 <!-- 

  <xsl:template match="figure">
    <xsl:call-template name="label"/>
    <xsl:call-template name="mediaobject"/>
  </xsl:template> 



  <xsl:template match="figure//title">
    <fo:block>
       <xsl:number level="multiple"
                 count="chapter|section|"
                 format="1.1 "/>
       <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
 -->





<!--
  <xsl:template match="figure">
    <xsl:text>Here I am!</xsl:text>
    <fo:block break-after="page"/>
    <xsl:text>No, I'm here!</xsl:text>
    <xsl:apply-templates select="mediaobject"/>
    <xsl:apply-templates select="title"/>
  </xsl:template> 
 -->




<!--  The following works, but we'd need to add some fancy stuff to get it to
      automatically get the figure title and graphic source

<xsl:template match="figure">
   <fo:block break-before='page'/>
   <fo:table table-layout="fixed" width="100%">
      <fo:table-column column-width="proportional-column-width(1)"/>
      <fo:table-body>
         <fo:table-row padding-bottom="0.5em">
            <fo:table-cell>
               <fo:block font-family="Ariel"
                         font-style="bold"
                         font-size="10pt"
                         space-before="5mm"
                         space-after="5mm"
                         text-align="center">
                  <xsl:text>CELL TEXT 1</xsl:text>
                  <xsl:value-of select="title"/>
                  <xsl:value-of select="title"/>
               </fo:block>
            </fo:table-cell>
         </fo:table-row>
         <fo:table-row keep-with-previous="always">
            <fo:table-cell>
              <fo:block>
                <fo:external-graphic src="url(diagrams/text-d01.epsi)"/>
                </fo:block>
            </fo:table-cell>
         </fo:table-row>
     </fo:table-body>
  </fo:table>
  <fo:block break-after='page'/>
</xsl:template>

 -->


<!-- 

<xsl:template match="figure">
  <fo:block break-before='page'/>
    <xsl:sequence>
      <xsl:process select="mediaobject"/>
      <xsl:process select="title"/>
    </xsl:sequence>
 </xsl:template>

-->




  <!-- Ensure that we always put a page break before a new figure 

  <xsl:template match="figure">
       <fo:block break-after="page"/>
  </xsl:template> -->



  <!-- 

  <xsl:template match="processing-instruction('pagebreak')">
       <fo:block break-after="page"/>
  </xsl:template>


  <xsl:template match="figure">
    <fo:block>
      <xsl:process select="mediaobject"/>
      <xsl:process select="title"/>
    </fo:block>
  </xsl:template> 

  <xsl:template pattern="figure">
    <xsl:sequence>
      <xsl:process select="mediaobject"/>
      <xsl:process select="title"/>
    </xsl:sequence>
  </xsl:template> 
 -->



<xsl:template name="header.content">
  <xsl:param name="pageclass" select="''"/>
  <xsl:param name="sequence" select="''"/>
  <xsl:param name="position" select="''"/>
  <xsl:param name="gentext-key" select="''"/>

<!--
  <fo:block>
    <xsl:value-of select="$pageclass"/>
    <xsl:text>, </xsl:text>
    <xsl:value-of select="$sequence"/>
    <xsl:text>, </xsl:text>
    <xsl:value-of select="$position"/>
    <xsl:text>, </xsl:text>
    <xsl:value-of select="$gentext-key"/>
  </fo:block>
-->

  <fo:block>

    <!-- sequence can be odd, even, first, blank -->
    <!-- position can be left, center, right -->
    <xsl:choose>
      <xsl:when test="$sequence = 'blank'">
        <!-- nothing -->
      </xsl:when>

      <xsl:when test="$position='left'">
        <!-- Same for odd, even, empty, and blank sequences -->
        <xsl:call-template name="draft.text"/>
      </xsl:when>

      <xsl:when test="($sequence='odd' or $sequence='even') and $position='center'">
        <xsl:if test="$pageclass != 'titlepage'">
          <xsl:choose>
            <xsl:when test="ancestor::book and ($double.sided != 0)">
              <xsl:apply-templates select="." mode="titleabbrev.markup"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates select="." mode="titleabbrev.markup"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:if>
      </xsl:when>

      <xsl:when test="$position='center'">
        <!-- nothing for empty and blank sequences -->
      </xsl:when>

      <xsl:when test="$position='right'">
        <!-- Same for odd, even, empty, and blank sequences -->
        <xsl:call-template name="draft.text"/>
      </xsl:when>

      <xsl:when test="$sequence = 'first'">
        <!-- nothing for first pages -->
      </xsl:when>

      <xsl:when test="$sequence = 'blank'">
        <!-- nothing for blank pages -->
      </xsl:when>
    </xsl:choose>
  </fo:block>
</xsl:template>


<!-- give a bit more space to our centred titles in the header -->

<xsl:param name="header.column.widths">1 3 1</xsl:param>


<!--space-before="0.5em" font-style="italic" font-size="14.4pt" font-weight="bold" -->

<xsl:template match="copyright" mode="chapter.titlepage.recto.auto.mode">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" xsl:use-attribute-sets="chapter.titlepage.recto.style" space-before="1.5em" space-after="0.5em">
    <xsl:apply-templates select="." mode="chapter.titlepage.recto.mode"/>
  </fo:block>
</xsl:template>


<xsl:template match="legalnotice" mode="chapter.titlepage.recto.auto.mode">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" xsl:use-attribute-sets="chapter.titlepage.recto.style" space-before="0.5em" space-after="0.5em">
    <xsl:apply-templates select="." mode="chapter.titlepage.recto.mode"/>
  </fo:block>
</xsl:template>


<xsl:template match="pubdate" mode="chapter.titlepage.recto.auto.mode">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" xsl:use-attribute-sets="chapter.titlepage.recto.style" space-before="0.5em" space-after="0.5em">
    <xsl:apply-templates select="." mode="chapter.titlepage.recto.mode"/>
  </fo:block>
</xsl:template>






</xsl:stylesheet>

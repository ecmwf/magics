<?xml version='1.0'  encoding="iso-8859-1" ?> 
<xsl:stylesheet  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"  version="1.0"> 

<xsl:param name="local.l10n.xml" select="document('')"/>
<l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0">
  <l:l10n language="en">

   <l:gentext key="sectioncalled" text="section"/>


   <l:context name="xref">
      <l:template name="abstract" text="%t"/>
      <l:template name="answer" text="A:&#160;%n"/>
      <l:template name="appendix" text="%t"/>
      <l:template name="article" text="%t"/>
      <l:template name="authorblurb" text="%t"/>
      <l:template name="bibliodiv" text="%t"/>
      <l:template name="bibliography" text="%t"/>
      <l:template name="bibliomset" text="%t"/>
      <l:template name="biblioset" text="%t"/>
      <l:template name="blockquote" text="%t"/>
      <l:template name="book" text="%t"/>
      <l:template name="calloutlist" text="%t"/>
      <l:template name="caution" text="%t"/>
      <l:template name="chapter" text="%t"/>
      <l:template name="colophon" text="%t"/>
      <l:template name="constraintdef" text="%t"/>
      <l:template name="dedication" text="%t"/>
      <l:template name="equation" text="%t"/>
      <l:template name="example" text="%t"/>
      <l:template name="figure" text="%t"/>
      <l:template name="foil" text="%t"/>
      <l:template name="foilgroup" text="%t"/>
      <l:template name="formalpara" text="%t"/>
      <l:template name="glossary" text="%t"/>
      <l:template name="glossdiv" text="%t"/>
      <l:template name="important" text="%t"/>
      <l:template name="index" text="%t"/>
      <l:template name="indexdiv" text="%t"/>
      <l:template name="itemizedlist" text="%t"/>
      <l:template name="legalnotice" text="%t"/>
      <l:template name="listitem" text="%n"/>
      <l:template name="lot" text="%t"/>
      <l:template name="msg" text="%t"/>
      <l:template name="msgexplan" text="%t"/>
      <l:template name="msgmain" text="%t"/>
      <l:template name="msgrel" text="%t"/>
      <l:template name="msgset" text="%t"/>
      <l:template name="msgsub" text="%t"/>
      <l:template name="note" text="%t"/>
      <l:template name="orderedlist" text="%t"/>
      <l:template name="part" text="%t"/>
      <l:template name="partintro" text="%t"/>
      <l:template name="preface" text="%t"/>
      <l:template name="procedure" text="%t"/>
      <l:template name="productionset" text="%t"/>
      <l:template name="qandadiv" text="%t"/>
      <l:template name="qandaentry" text="Q:&#160;%n"/>
      <l:template name="qandaset" text="%t"/>
      <l:template name="question" text="Q:&#160;%n"/>
      <l:template name="reference" text="%t"/>
      <l:template name="refsynopsisdiv" text="%t"/>
      <l:template name="segmentedlist" text="%t"/>
      <l:template name="set" text="%t"/>
      <l:template name="setindex" text="%t"/>
      <l:template name="sidebar" text="%t"/>
      <l:template name="table" text="%t"/>
      <l:template name="task" text="%t"/>
      <l:template name="tip" text="%t"/>
      <l:template name="toc" text="%t"/>
      <l:template name="variablelist" text="%t"/>
      <l:template name="varlistentry" text="%n"/>
      <l:template name="warning" text="%t"/>
      <l:template name="olink.document.citation" text=" in %o"/>
      <l:template name="olink.page.citation" text=" (page %p)"/>
      <l:template name="page.citation" text=" on page %p"/>
      <l:template name="page" text="(page %p)"/>
      <l:template name="docname" text=" in %o"/>
      <l:template name="docnamelong" text=" in the document titled %o"/>
      <l:template name="pageabbrev" text="(p. %p)"/>
      <l:template name="Page" text="Page %p"/>
      <l:template name="bridgehead" text="&#8220;%t&#8221;"/>
      <l:template name="refsection" text="&#8220;%t&#8221;"/>
      <l:template name="refsect1" text="&#8220;%t&#8221;"/>
      <l:template name="refsect2" text="&#8220;%t&#8221;"/>
      <l:template name="refsect3" text="&#8220;%t&#8221;"/>
      <l:template name="sect1" text="&#8220;%t&#8221;"/>
      <l:template name="sect2" text="&#8220;%t&#8221;"/>
      <l:template name="sect3" text="&#8220;%t&#8221;"/>
      <l:template name="sect4" text="&#8220;%t&#8221;"/>
      <l:template name="sect5" text="&#8220;%t&#8221;"/>
      <l:template name="section" text="&#8220;%t&#8221;"/>
      <l:template name="simplesect" text=" &#8220;%t&#8221;"/>
   </l:context>

   <l:context name="xref-number-and-title">
      <l:template name="appendix" text="Appendix&#160;%n, %t"/>
      <l:template name="bridgehead" text="Section&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="chapter" text="Chapter&#160;%n, %t"/>
      <l:template name="equation" text="Equation&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="example" text="Example&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="figure" text="Figure&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="part" text="Part&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="procedure" text="Procedure&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="productionset" text="Production&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="qandadiv" text="Q &amp; A&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="refsect1" text="&#8220;%t&#8221;"/>
      <l:template name="refsect2" text="&#8220;%t&#8221;"/>
      <l:template name="refsect3" text="&#8220;%t&#8221;"/>
      <l:template name="refsection" text="&#8220;%t&#8221;"/>
      <l:template name="sect1" text="Section&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="sect2" text="Section&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="sect3" text="Section&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="sect4" text="Section&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="sect5" text="Section&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="section" text="Section&#160;%n, &#8220;%t&#8221;"/>
      <l:template name="simplesect" text="&#8220;%t&#8221;"/>
      <l:template name="table" text="Table&#160;%n, &#8220;%t&#8221;"/>
   </l:context>

  </l:l10n>
</l:i18n>




</xsl:stylesheet>  

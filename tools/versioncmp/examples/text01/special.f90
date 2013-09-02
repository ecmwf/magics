      program text01
!
!     this program demonstrates magics special characters
!
!     open magics
!
      call popen
      
!
!     define plot layout.  note the setting up of super_page and page
!     sizes to enable two pages to fit into the super_page.  the frame
!     around the subpage is turned off.
!     ----------------------------------------------------------------
      call psetc ('output_format', 'png')
      call psetr ('super_page_y_length',29.7)
      call psetr ('super_page_x_length',21.0)
      call psetr ('page_x_length',21.0)
      call psetr ('page_y_length',14.35)
      call psetc ('page_id_line', 'off')
      call psetc ('plot_start','top')
      call psetc ('subpage_frame','off')
!
!     position the text box
!     ---------------------
      call psetc ('text_mode','positional')
      call psetr ('text_box_x_position',3.5)
      call psetr ('text_box_y_position',6.0)
      call psetr ('text_box_x_length', 14.0)
      call psetr ('text_box_y_length',4.0)
!
!     write text on pages one and two of each super_page
!     ---------------------------------------------------
      call text1
      call pnew ('page')
      call text2
!
!     close magics
!     ------------
      call pclose
      end
      subroutine text1
!
!     write two lines of text in a text box. text mode is positional
!     --------------------------------------------------------------
      call pseti ('text_line_count',5)
      call psetc ('text_line_1','<font colour="red"> Use of special Characters </font>')
      call psetc ('text_line_2','XML entities  <font size="1"> &eacute;&ucirc;&copy;&deg; </font>')
      call psetc ('text_line_4','HTML  <font size="1">  &#233;&#251;&#169;&#176; </font>')
      call psetc ('text_line_5', 'Unicode   éû©° ')
      call ptext
!
      return
      end
      subroutine text2
!
!     write two lines of text in a text box. text mode is positional
!     --------------------------------------------------------------
      call pseti ('text_line_count',4)
      call psetc ('text_line_1','<font colour="red"> HTML formatting </font>')
      call psetc ('text_line_2','value <sub> sub </sub> ')
      call psetc ('text_line_3','value <sup> sup </sup> ')
      call psetc ('text_line_4','<u> underlined </u> <b> bold </b>')
      call ptext
!
      return
	  end

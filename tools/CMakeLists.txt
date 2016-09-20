ecbuild_add_resources( TARGET tools_scripts
                       SOURCES 
                            magml2json.pl
                            mergexml.pl
                            newbase.pl
                            newclass.pl
                            newdriver.pl
                            newfactory.pl
                            newprojection.pl
                            newtemplate.pl
                            xml2cc.pl
                            xml2cc_list.pl
                            xml2doc.pl
                            xml2dtd.pl
                            xml2icon.pl
                            xml2mv.pl
                            magicsCompatibilityChecker
                      DONT_PACK
                            ParaTemplate.odt
)

install ( FILES magicsCompatibilityChecker   
            DESTINATION ${INSTALL_BIN_DIR} 
                        PERMISSIONS OWNER_READ GROUP_READ WORLD_READ 
                                                OWNER_EXECUTE GROUP_EXECUTE WORLD_EXECUTE)


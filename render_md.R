rmarkdown::render("~/R/Project_2/project_2.Rmd", 
                  output_file = "~/R/Project_2/README.md", 
                  output_format = "md_document",
                  output_options = list(toc=TRUE, toc_depth=1,
                                        number_sections=TRUE, df_print="default"))


#rmarkdown::render(  input = 'file.RMD' , output_file = 'file.md'
 #                   , output_format=   md_document( ) )  


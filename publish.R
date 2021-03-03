#This Utility script processes CAISO_Analysis.Rmd to CAISO_Analysis.html

#Set up environment
  setwd("/home/noah/Documents/Code Samples/CAISO/")
  library(rmarkdown)

#Render Markdown Document as HTML
  render(input = "./CAISO_Analysis.Rmd",
         output_format = "html_document",
         output_file = "./CAISO_Analysis.html")

#Make a copy named index.html such that it is published by GitHub Pages service
  file.copy(from = "./CAISO_Analysis.html",
            to   = "./index.html", 
            overwrite = TRUE)

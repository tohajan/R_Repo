
# 1. CONVERT .Rmd FILE TO .R FILE

library(knitr)
# a. To extract only R codes from the Rmd file:
# purl("Data Handling & Graphics.Rmd") # the "purl" function is located in the "knitr" library

# b. To extract R codes along with documentation (i.e., the non-codes in an .Rmd file):
# purl("Data Handling & Graphics.Rmd", output = "Data Handling + Graphics.R", documentation = 2) 
# purl("Simulation.Rmd", output = "Simulation.R", documentation = 2)
# purl("Statistical Modeling.Rmd", output = "Statistical Modeling.R", documentation = 2)
# purl("C:/Users/tohaj/Desktop/R_Practice/Tidyverse/Descriptive and Exploratory.Rmd", 
#      output = "C:/Users/tohaj/Desktop/R_Practice/Tidyverse/Descriptive and Exploratory.R", 
#      documentation = 2)
# purl("C:/Users/tohaj/Desktop/R_Practice/Tidyverse/Linear Regression.Rmd",
#      output = "C:/Users/tohaj/Desktop/R_Practice/Tidyverse/Linear Regression.R",
#      documentation = 2)

# purl("C:/Users/tohaj/Desktop/R_Practice/Tidyverse/Multiple Linear Regression.Rmd",
#      output = "C:/Users/tohaj/Desktop/R_Practice/Tidyverse/Multiple Linear Regression.R",
#      documentation = 2)


# 2. MAKE AN .Rmd DOC RUN IN GITHUB (see complete details 
# at https://gist.github.com/JoshuaTPierce/b919168421b40e06481080eb53c3fb2f):
# In the markdown document, change "output=html_document" to "output=github_document"
# The above change will create a .md document which renders in github just like a normal .Rmd file in R
#############################################################
# Author: Assil BEN-AMOR (assil.benamor@reach-initiative.org)
# Created: 19/05/2022
# Last update: 19/05/2022
# Installing dependencies needed to run the scripts
##############################################################



#########################  Run this  #########################  

if (!require("pacman")) install.packages("pacman") 
if (!require("remotes")) install.packages("remotes") 
if (!require("surveyweights")) remotes::install_github("https://github.com/impact-initiatives/surveyweights")
if (!require("koboquest")) remotes::install_github("https://github.com/impact-initiatives/koboquest")
if (!require("hypegrammaR")) remotes::install_github("https://github.com/impact-initiatives/hypegrammaR")


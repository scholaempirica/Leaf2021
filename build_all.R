# This file was generated by project setup for project Leaf2021.
# As you work on the project, you should add commands to this file so that
# it contains the commands needed to rebuild the whole project from scratch.
# Typically, those will be source() or rmarkdown::render() calls, like below.


source('01_data-import.R', encoding = "UTF-8")
source('02_data-cleaning.R', encoding = "UTF-8")
rmarkdown::render('03_leaf_descriptives.Rmd', output_dir = 'reports-output')




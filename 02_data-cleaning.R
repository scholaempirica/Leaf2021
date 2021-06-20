# Data and packages -------------------------------------------------------

library(tidyverse)
library(writexl)

# Data cleaning -----------------------------------------------------------

leaf <- leaf %>%
        mutate(across(.cols = starts_with("S2Q5"),
                      .fns  = ~fct_relabel(., str_replace,  "^.+ = ", "")))

leaf_xlsx <- leaf
names(leaf_xlsx) <-  paste(names(leaf_xlsx), attr(leaf_xlsx, "variable.labels"), sep = "_") # Added labels to var names, so that they can
                                                                                            # can be preserved in .xlsx

# Data export -------------------------------------------------------------

write_rds(leaf, "data-processed/leaf.rds")
write_xlsx(leaf_xlsx, "data-processed/leaf.xlsx") # LEAF wanted an .xlsx format

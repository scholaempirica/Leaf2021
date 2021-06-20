# Packages ----------------------------------------------------------------

library(reschola)
library(tidyverse)

# Data import -------------------------------------------------------------

survey_ids <- c(325785, 859643)

surveys_sk <- map(survey_ids,
                  ~ls_responses(., lang = "sk", part = "complete")) # LEAF only wanted complete questionnaires

surveys_sk[[2]]$S7 = NULL # S7 was a "question" on consent with processing of personal information

leaf <- bind_rows(surveys_sk)

# Data export -------------------------------------------------------------

write_rds(leaf, "data-input/leaf.rds")

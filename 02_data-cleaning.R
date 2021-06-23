# Data and packages -------------------------------------------------------

library(tidyverse)
library(labelled)
library(writexl)

leaf <- read_rds("data-input/leaf.rds")

# Data cleaning -----------------------------------------------------------

leaf <- leaf %>%
        mutate(across(.cols = starts_with("S2Q5"),
                      .fns  = ~fct_relabel(., str_replace,  "^.+ = ", "")), # some variables had labels embedded in values, removed labels
               across(.cols = c("S2Q3_a", "S5Q1_a"),
                      .fns  = ~ str_extract(., "[:digit:]")), #some variable included labels combined with values
               across(.cols = c("S2Q3_a", "S5Q1_a"),
                      .fns  = ~as.numeric(.)),
               S4Q5 = fct_explicit_na(S4Q5, "Iné"), # Not-living-in-Slovakia is a valid value, not NA
               region = ifelse(S4Q6 == "Bratislava a Bratislavský kraj", "Bratislavský kraj", "Iný kraj")) #collapsed region for succinct tables down the line

attr(leaf, which = "variable.labels") <- c(attr(leaf, "variable.labels"), "Kraj bydliska")

# Collapsing country of living into smaller number of categories
england     <- c("Uk", "Spojene kralovstvo", "Velka britania", "Spojené kráľovstvo", "Anglicko", "United kingdom", "Veľká británia")
czechia     <- c("Česká republika", "Česko", "Cr", "Cesko", "Čr", "Česko ", "Cz", "Czechia", "Čr ", "Cz, praha")
austria     <- c("Rakúsko", "Rakusko")
switzerland <- c("Svajciarsko", "Švajčiarsko")
uae         <- c("United arab emirates", "Spojené arabské emiráty")
usa         <- c("Usa", "Hawaii, usa")

leaf <- leaf %>%
  mutate(S4Q5_other = str_to_sentence(S4Q5_other),
         S4Q5_other = case_when(S4Q5_other %in% england     ~ "Velká Britania",
                                S4Q5_other %in% czechia     ~ "Česká Republika",
                                S4Q5_other %in% austria     ~ "Rakúsko",
                                S4Q5_other %in% switzerland ~ "Švajčiarsko",
                                S4Q5_other %in% uae         ~ "Spojené arabské emiráty",
                                S4Q5_other %in% usa         ~ "Spojené státy Americké",
                                TRUE                        ~ S4Q5_other))

# Cleaning organization names

osf <- c("Osf Slovensko", "Osf, Nzk, Tis", "Osf Slovensko")
missing <- c("Nemám", "Nemám Žiadnu Takúto Organizáciu", "Neviem Si Na Žiadnu Spomenúť",
             "V Tom Čase Ešte Žiadna, Ale Táto Otázka Nie Je Relevantná Pre Mňa, Len Nemôžem Pokračovať Ďalej", "-")

leaf <- leaf %>%
  mutate(across(.cols = starts_with("S2Q2"),
                .fns  = ~str_to_title(.)),
         across(.cols = starts_with("S2Q2"),
                .fns  = ~str_trim(.)),
         across(.cols = starts_with("S2Q2"),
                .fns  = ~case_when(str_detect(., "Leaf") ~ "Leaf",
                                  . == "Aiesec"         ~ "Aisec",
                                  . == "Dofe Slovensko" ~ "Dofe",
                                  . == "Erko - Hnutie Kresťanských Spoločenstiev Detí" ~ "Erko",
                                  . == "Kdn - Klub Detskej Nádeje" ~ "Klub Detskej Nádeje",
                                  . == "Letná Akadémia Discover - Slovenská Debatná Asociácia" ~ "Letná Akadémia Discover",
                                  . == "Open Society Foundation - Katarína Križková" ~ "Open Society Foundation",
                                  . == "Pre Stredoskolakov" ~ "Pre Stredoškolákov",
                                  . == "Slovenský Skauting - 1.Zbor Bratislava A Rozhovory So Staršími Skautmi" ~ "Slovenský Skauting",
                                  . == "Strom O.z." ~ "Strom",
                                  . == "Transparency International Slovensko" ~ "Transparency International",
                                  . %in% osf ~ "Osf",
                                  . %in% missing ~ NA_character_,
                                  . == "" ~ NA_character_,
                                  TRUE ~ .)))



leaf_xlsx <- leaf
names(leaf_xlsx) <-  paste(names(leaf_xlsx), attr(leaf_xlsx, "variable.labels"), sep = "_") # Added labels to var names, so that they can
                                                                                            # can be preserved in .xlsx
# Data export -------------------------------------------------------------

write_rds(leaf, "data-processed/leaf.rds")
write_xlsx(leaf_xlsx, "data-processed/leaf.xlsx") # LEAF wanted an .xlsx format

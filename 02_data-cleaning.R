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

# collapsing S2Q3_a scale

leaf <- leaf %>%
  mutate(S2Q3_coll = case_when(S2Q3_a >= 5 ~ "5-6",
                                S2Q3_a >= 3 ~ "3-4",
                                S2Q3_a >= 1 ~ "1-2"),
         S2Q3_coll = as.factor(S2Q3_coll)) #LEAF wanted to use collapsed scale in the analysis

attr(leaf, which = "variable.labels") <- c(attr(leaf, "variable.labels"), attr(leaf, "variable.labels")[25]) #added the same label as the original scale

var_labs <- attr(leaf, "variable.labels")

# Scoring S1Q1 scale

leaf <- leaf %>%
  mutate(S1Q1_an = case_when(S1Q1_a == "vôbec nesúhlasím" ~ -1,
                             S1Q1_a == "skôr nesúhlasím"  ~ -0.5,
                             S1Q1_a == "skôr súhlasím"    ~  0.5,
                             S1Q1_a == "určite súhlasím"  ~  1,
                             S1Q1_a == "neviem posúdiť"   ~  0),
         S1Q1_bn = case_when(S1Q1_b == "vôbec nesúhlasím" ~  1,
                             S1Q1_b == "skôr nesúhlasím"  ~  0.5,
                             S1Q1_b == "skôr súhlasím"    ~ -0.5,
                             S1Q1_b == "určite súhlasím"  ~ -1,
                             S1Q1_b == "neviem posúdiť"   ~  0),
         S1Q1_dn = case_when(S1Q1_d == "vôbec nesúhlasím" ~ -2,
                             S1Q1_d == "skôr nesúhlasím"  ~ -1,
                             S1Q1_d == "skôr súhlasím"    ~  1,
                             S1Q1_d == "určite súhlasím"  ~  2,
                             S1Q1_d == "neviem posúdiť"   ~  0),
         across(.cols = c("S1Q1_c", "S1Q1_e", "S1Q1_f"),
                .fns  = ~case_when(. == "vôbec nesúhlasím" ~  2,
                                   . == "skôr nesúhlasím"  ~  1,
                                   . == "skôr súhlasím"    ~ -1,
                                   . == "určite súhlasím"  ~ -2,
                                   . == "neviem posúdiť"   ~  0),
                .names = "{.col}n")) %>%
  rowwise() %>%
  mutate(dimension3 = sum(c_across(S1Q1_an:S1Q1_fn))) %>%
  ungroup() %>%
  select(-c(S1Q1_an:S1Q1_fn))

attr(leaf, "variable.labels") <- c(var_labs, "Dimenze 3") # rowwise() strips attributes, including variable labels.
                                                          # Had to readd them and add label add label for dimension3

# Scoring scale S3Q1

var_labs <- attr(leaf, "variable.labels")

labels <- tibble(name  = names(leaf),
                 label = attr(leaf, "variable.labels")) %>%
  filter(str_detect(name, "S3Q1"))

leaf <- leaf %>%
  mutate(across(.cols = c("S3Q1_b", "S3Q1_q"),
                .fns  = ~case_when(. == "áno" ~ 2,
                                         TRUE ~ 0),
                .names = "{.col}n"),
         across(.cols = c("S3Q1_a", "S3Q1_c", "S3Q1_e", "S3Q1_j", "S3Q1_n", "S3Q1_o"),
                .fns  = ~case_when(. == "áno" ~ 1,
                                   TRUE       ~ 0),
                .names = "{.col}n")) %>%
  rowwise() %>%
  mutate(dimension4 = sum(c_across(S3Q1_bn:S3Q1_on))) %>%
  ungroup() %>%
  select(-c(S3Q1_bn:S3Q1_on))

attr(leaf, "variable.labels") <- c(var_labs, "Dimenze 4") # Again, rowwise() strips labels


# Preparing export to Excel -----------------------------------------------

leaf_xlsx <- leaf
names(leaf_xlsx) <-  paste(names(leaf_xlsx), attr(leaf_xlsx, "variable.labels"), sep = "_") # Added labels to var names, so that they can
                                                                                            # can be preserved in .xlsx
# Data export -------------------------------------------------------------

write_rds(leaf, "data-processed/leaf.rds")
write_xlsx(leaf_xlsx, "data-processed/leaf.xlsx") # LEAF wanted an .xlsx format

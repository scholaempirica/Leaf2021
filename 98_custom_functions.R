# Fit flextable to the width of the page ----------------------------------

# shamelessly lifted from https://stackoverflow.com/questions/57175351/flextable-autofit-in-a-rmarkdown-to-word-doc-causes-table-to-go-outside-page-mar
FitFlextableToPage <- function(ft, pgwidth = 6){

  ft_out <- ft %>% autofit()

  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}


# Outputs descriptive table using gtsummary and flextable ----------------
# takes data, selected vars, data frame of labels (with "name" and "label" vars) and optionally grouping variable

desc_table <- function(data, vars, var_labs_df, groupby = NULL) {

  data <- data %>% select({{ vars }})

  var_labs_df          <- var_labs_df %>% filter(name %in% names(data))
  var_labs_list        <- as.list(var_labs_df$label)
  names(var_labs_list) <- var_labs_df$name #variable labels have to be inside a named list for tbl_summary()

  if (!is.null(groupby)) {

    groupby = enquo(groupby)

    tab <- tbl_summary(data = data,
                       label = var_labs_list,
                       missing_text = "přeskočil otázku",
                       by = !!groupby)
  } else {

  tab <- tbl_summary(data = data,
                     label = var_labs_list,
                     missing_text = "přeskočil otázku")
  }

  tab %>%
    modify_header(label = "") %>%
    modify_footnote(update = everything() ~ NA) %>%
    bold_labels() %>%
    as_flex_table() %>%
    theme_booktabs() %>%
    FitFlextableToPage()
}



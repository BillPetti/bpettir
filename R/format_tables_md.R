#' Format Markdown Tables
#'
#' @param df A data frame to view as a table
#' @param caption A caption, if needed
#'
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @return A formatted data frame to display in a markdown document
#' @export
#'
#' @examples \dontrun{format_tables(mtcars)}

format_tables_md <- function(df, caption = NA) {
  if (!is.na(caption)) {
    df_table <- df %>%
      knitr::kable(caption = caption) %>%
      kableExtra::kable_styling(position = "c",
                    bootstrap_options = "striped",
                    full_width = FALSE)
  } else {
    df_table <- df %>%
      knitr::  kable() %>%
      kableExtra::kable_styling(position = "c",
                    bootstrap_options = "striped",
                    full_width = FALSE)
  }

  df_table
}
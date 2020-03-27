#' Variable List
#'
#' @param x A data frame
#'
#' @return Gives variable number, name, first observation, and the variable's class and returns a dataframe
#' @importFrom tibble tibble rownames_to_column
#' @importFrom dplyr select mutate filter distinct pull rename left_join
#' @importFrom purrr map_df
#' @export
#'
#' @examples variable_list(mtcars)

variable_list <- function(x) {
  data <- df[1:5, ]
  data <- t(data)
  vars <- row.names(data)
  num <- 1:length(vars)
  data <- as.data.frame(cbind(num, vars, data))
  names(data) <- c("variable_index_position", "variable_name",
                   "first_observation", "second_observation", "third_observation",
                   "fourth_observation", "fifth_observation")
  row.names(data) <- NULL
  class <- sapply(df, class)
  data$class <- class
  data <- data[, c(2, 1, 8, 3:7)]
  data$variable_name <- as.character(data$variable_name)

  unique_values_function <- function(df) {

    unique_values_count <- map(.x = df,
                               ~length(unique(.x))) %>%
      dplyr::bind_rows(.id = 'vars') %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = 'var') %>%
      dplyr::rename(unique_values = V1) %>%
      dplyr::mutate(unique_values = as.numeric(as.character(unique_values))) %>%
      dplyr::mutate(var = as.character(var))

    vars_to_display_u_values <- unique_values_count %>%
      dplyr::filter(!grepl('date|time|vars', var, ignore.case = T)) %>%
      dplyr::filter(unique_values < 80) %>%
      dplyr::pull(var)

    uniques <- function(df, x) {

      payload <- df %>%
        as.data.frame() %>%
        dplyr::select(x) %>%
        dplyr::distinct() %>%
        dplyr::pull()

      payload_vector <- tibble::tibble(var = x,
                               distinct_values = paste(payload, sep = ", ", collapse = ", "))

      return(payload_vector)

    }

    mapped_uniques <- purrr::map_df(.x = vars_to_display_u_values,
                             ~uniques(df, .x))

    return(mapped_uniques)

  }

  uniques <- unique_values_function(df)

  final_payload <- data %>%
    dplyr::left_join(uniques, by = c('variable_name' = 'var'))

  final_payload <- final_payload %>%
    dplyr::select(variable_name:class, distinct_values, everything())

  return(final_payload)
}

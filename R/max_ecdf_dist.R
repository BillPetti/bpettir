#' Generate the Cumulative Distribution for a given set of values
#'
#' @param df A data frame
#' @param group_var The variable that contains your two groups that you want to
#' compare--think 1 vs. 0
#' @param feature_var The feature or variable whose values you are interested
#' in exploring
#' @param feature_string The quoted name of the feature_var
#' @importFrom rlang enquo
#' @importFrom dplyr filter pull bind_rows mutate select arrange
#' @importFrom tidyr drop_na gather
#' @importFrom purrr set_names map
#' @importFrom tibble tibble
#' @return A tibble with the full cumulative distribution for the different
#' values of the group_var.
#' @export
#'
#' @examples \dontrun{mac_ecdf_dist(df, outcome, key, "key")}

max_ecdf_dist <- function(df, group_var, feature_var, feature_string) {

  var <- rlang::enquo(group_var)
  feature_enquo <- rlang::enquo(feature_var)

  positive_cases <- df %>%
    dplyr::filter((!!var) == 1) %>%
    dplyr::pull(!!feature_enquo)

  negative_cases <- df %>%
    dplyr::filter((!!var) == 0) %>%
    dplyr::pull(!!feature_enquo)

  values <- tibble::tibble(values = c(positive_cases,
                                      negative_cases)) %>%
    tidyr::drop_na() %>%
    dplyr::arrange(values)

  positive_ecdf <- stats::ecdf(positive_cases)

  negative_ecdf <- stats::ecdf(negative_cases)

  pos_ecdf_values <- purrr::map(.x = values$values,
                                ~positive_ecdf(.x)) %>%
    purrr::set_names(values$values) %>%
    dplyr::bind_rows() %>%
    tidyr::gather(key = values, value = cum_dist) %>%
    dplyr::mutate(type = 1)

  neg_ecdf_values <- purrr::map(.x = values$values,
                                ~negative_ecdf(.x)) %>%
    purrr::set_names(values$values) %>%
    dplyr::bind_rows() %>%
    tidyr::gather(key = values, value = cum_dist) %>%
    dplyr::mutate(type = 1)

  comb_ecdf <- pos_ecdf_values %>%
    dplyr::left_join(neg_ecdf_values, by = "values") %>%
    dplyr::select(-type.x, -type.y)

  names(comb_ecdf) <- c("values", "pos_cum_dist", "neg_cum_dist")

  comb_ecdf <- comb_ecdf %>%
    dplyr::mutate(values = as.numeric(values),
                  feature = paste(feature_string)) %>%
    dplyr::select(feature, everything())

  comb_ecdf
}

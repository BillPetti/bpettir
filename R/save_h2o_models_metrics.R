#' Save models and various performance metrics from h2o autoML run
#'
#' @param leaderboard an h2o leaderboard object
#' @param max_top_models The top-n models to save. Defaults to NA, which
#' saves all
#' @param path_slug A path where you want the models to be saved
#'
#' @importFrom dplyr select filter mutate pull
#' @importFrom tidyr gather
#' @importFrom purrr map map_df
#' @importFrom tibble rownames_to_column
#' @return
#' @export
#'
#' @examples \dontrun{save_h2o_models_metrics(leaderbard_a, max_top_models = 5, path_slug = getwd())}

save_h2o_models_metrics <- function(leaderboard,
                            max_top_models = NA,
                            path_slug) {

  loop_save_models <- function(model_from_leaderboard,
                               path_slug) {

    model <- h2o.getModel(model_from_leaderboard)

    h2o.saveModel(object = model,
                  force=TRUE,
                  path = path_slug)
  }

  leaderboard$model_id %>%
    as.data.frame() %>%
    {if (!is.na(max_top_models)) dplyr::slice(.data = ., 1:max_top_models)
      else dplyr::slice(.data = ., 1:length(.$model_id))} %>%
    dplyr::pull(model_id) %>%
    purrr::map(~loop_save_models(., path = path_slug))

  performance_data <- function(model_from_leaderboard) {

    model <- h2o.getModel(model_from_leaderboard)

    var_import <- model@model$variable_importances %>%
      as.data.frame() %>%
      dplyr::mutate(model_id = model@model_id) %>%
      dplyr::select(model_id, everything())

    confusion_marix <- model@model$cross_validation_metrics@metrics$cm$table %>%
      dplyr::rownames_to_column

    performance <- model@model$cross_validation_metrics@metrics$thresholds_and_metric_scores %>%
      dplyr::filter(between(threshold, .5, .509)) %>%
      dplyr::filter(threshold == min(threshold)) %>%
      tidry::gather(key = metric, value = value) %>%
      dplyr::mutate(model_id = model@model_id) %>%
      dplyr::select(model_id, everything())

    model_list <- list(model_object = model,
                       confusion_marix = confusion_marix,
                       variable_importance = var_import,
                       model_performance = performance)

    model_list
  }

  names_to_pull <- leaderboard$model_id %>%
    as.data.frame() %>%
    {if (!is.na(max_top_models)) slice(.data = ., 1:max_top_models)
      else slice(.data = ., 1:length(.$model_id))} %>%
    dplyr::pull(model_id)

  payload <- names_to_pull %>%
    purrr::map(~performance_data(model_from_leaderboard = .)) %>%
    setNames(nm = names_to_pull)

  payload
}

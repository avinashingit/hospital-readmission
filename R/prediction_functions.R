#' Title
#'
#' @param model_file
#' @param test_data_file
#' @param output_dir
#' @param reference_file
#'
#' @return
#' @export
#'
#' @examples
prediction_with_glmnet <-
  function(model_file,
           test_data_file,
           output_dir,
           reference_file) {
    model_output <- readRDS(model_file)
    test_data <- EviveUtil::read_data(data_file = test_data_file)
    ref_file_data <- EviveUtil::read_data(data_file = reference_file)

    test_data_to_consider <-
      test_data[, setdiff(colnames(test_data),
                          c(ref_file_data$remove, ref_file_data$response))]
    test_data_to_consider <-
      BackPain::fill_missing_cols(test_data_to_consider,
                                  model_output)
    prediction_result <- glmnet::predict.cv.glmnet(
      object = model_output,
      newx = data.matrix(test_data_to_consider[, row.names(model_output$glmnet.fit$beta)]),
      s = "lambda.min",
      type = "response"
    )
    prediction_output <-
      get_prediction_output(prediction_result, test_data$upin)
    write.csv(
      prediction_output,
      file.path(output_dir, prediction_output),
      sep = "\t",
      row.names = FALSE
    )
  }

#' Title
#'
#' @param model_file
#' @param test_data_file
#' @param output_dir
#' @param pca_output_file
#' @param number_of_principal_components_file
#' @param reference_file
#'
#' @return
#' @export
#'
#' @examples
prediction_with_pca_glmnet <-
  function(model_file,
           test_data_file,
           output_dir,
           pca_output_file,
           number_of_principal_components_file,
           reference_file) {
    model_output <- readRDS(model_file)
    test_data <- readRDS(test_data_file)
    pca_output <- readRDS(pca_output_file)
    select_pcs <- readRDS(number_of_principal_components_file)
    cols_not_in_test_data <- setdiff(rownames(pca_output$rotation), colnames(test_data))
    test_data[cols_not_in_test_data] <- 0
    test_data_pc <- scale(test_data[, rownames(pca_output$rotation)],
                                 pca_output$center, pca_output$scale) %*% pca_output$rotation
    test_data_to_consider <-
      cbind(test_data[, setdiff(colnames(test_data), rownames(pca_output$rotation))],
            test_data_pc[, 1:select_pcs])
    test_data_to_consider <-
      BackPain::fill_missing_cols(test_data_to_consider,
                                  model_output)
    prediction_result <- glmnet::predict.cv.glmnet(
      object = model_output,
      newx = data.matrix(test_data_to_consider[, row.names(model_output$glmnet.fit$beta)]),
      s = "lambda.min",
      type = "response"
    )
    prediction_output <-
      get_prediction_output(prediction_result, test_data$upin)
    write.csv(
      prediction_output,
      file.path(output_dir, prediction_output),
      sep = "\t",
      row.names = FALSE
    )
  }

#' Title
#'
#' @param prediction_result
#' @param upins
#'
#' @return
#' @export
#'
#' @examples
get_prediction_output <- function(prediction_result, upins) {
  pred <- prediction_result
  pred20 <- rep(0, length(pred))
  pred20[pred > quantile(pred, 0.8, na.rm = T)] <- 1
  output <- cbind(upins, pred20, pred)
  rownames(output) <- NULL
  colnames(output)[1] <- "UPIN"
  colnames(output)[2] <- "result"
  colnames(output)[3] <- "pred"
  output <- data.frame(output)
  output$pred <- as.numeric(as.character(output$pred))
  logging::loginfo("Output has been generated with dimensions %s", dim(output))
  quantiles <- quantile(pred, seq(0.1, 1, by = 0.1), na.rm = T)
  names(quantiles) <- 0:9
  output$quantile <-
    sapply(pred, function(u) {
      if (length(names(quantiles[quantiles > u])) > 0) {
        return(as.integer(min(names(quantiles[quantiles > u]))))
      } else {
        return(9)
      }
    })
  output$quantile[is.na(output$quantile)] <- 0
  return(output)
}
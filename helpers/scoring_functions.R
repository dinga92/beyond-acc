

#' calculate AUC
#'
#' caluclate the area under the receiver operating characteristic curve measure
#' @param predictions predicted values
#' @param labels true values coded as 0 and 1
#'
#' @return AUC score
#' @export
#'
#' @examples get_auc_score(c(0.4, 0.5, 0.6, 0.7, 0.8, 0.8), c(0,0,1,0,1,1))
get_auc_score <- function(predictions, labels) {
  assert_values_only(labels, c(0,1))
  AUC::auc(AUC::roc(predictions, as.factor(labels)))
}


#' calculate accuracy
#'
#' calculate accuracy i.e. proportion of correctly classified samples.
#' @param predictions predicted values (only 0 and 1 or true or false allowed)
#' @param labels true values coded as 0 and 1
#'
#' @return accuracy score
#' @export
#'
#' @examples get_acc_score(c(0,0,1,1), c(0,0,1,1))
get_acc_score <- function(predictions, labels){
  assert_values_only(predictions, c(0,1))
  assert_values_only(labels, c(0,1))
  mean(predictions == labels)
}


#' calculate missclasification error
#'
#' calculate missclassification error, i.e. 0/1 loss or 1-accuracy
#' @param predictions predicted values
#' @param labels true values, coded as 0 and 1
#'
#' @return missclassification error
#' @export
#'
#' @examples get_missclassification_error(c(0,0,1,1), c(0,0,1,1))
get_missclassification_error <- function(predictions, labels){
  1 - get_acc_score(predictions, labels)
}


#' Calculate brier score
#'
#' Calculate brier score i.e. mean squared error between probabilistic
#' predictions and categorical outcomes
#' @param predictions probabilistic predictions between 0 and 1
#' @param labels true values coded as 0 and 1
#'
#' @return brier score
#' @export
#'
#' @examples get_brier_score(c(0.4, 0.5, 0.6, 0.7, 0.8, 0.8), c(0,0,1,0,1,1))
get_brier_score <- function(predictions, labels) {
  assert_values_range(predictions, c(0,1))
  assert_values_only(labels, c(0,1))
  mean((labels-predictions)^2)
}


#' Calculate logarithimic score
#'
#' Calculate logarithmic score between probabilistic predicitons and
#' categorical outcomes
#' @param predictions probabilistic predictions between 0 and 1
#' @param labels true values coded as 0 and 1
#'
#' @return logarithimic score
#' @export
#'
#' @examples get_log_score(c(0.4, 0.5, 0.6, 0.7, 0.8, 0.8), c(0,0,1,0,1,1))
get_log_score <- function(predictions, labels){
  assert_values_range(predictions, c(0,1))
  assert_values_only(labels, c(0,1))
  # library(MLmetrics)
  # We assume predicitons come already in a safe range,
  # so we don't need this here:
  # eps <- 1e-15
  # y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
  -mean(labels * log(predictions) + (1-labels) * log(1-predictions))
}

#' Calculate logarithimic score for each prediction
#'
#' @export
get_individual_log_scores <- function(predictions, labels){
  # library(MLmetrics)
  # We assume predicitons come already in a safe range,
  # so we don't need this here:
  # eps <- 1e-15
  # y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
  -(labels * log(predictions) + (1-labels) * log(1-predictions))
}

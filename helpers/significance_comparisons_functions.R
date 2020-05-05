
#' Title
#'
#' @param predictions1
#' @param predictions2
#' @param labels
#'
#' @return
#' @export
#'
#' @examples
compare_auc <- function(predictions1, predictions2, labels){
  assert_values_only(labels, c(0,1))
  library(pROC)
  roc1 <- pROC::roc(labels, predictions1)
  roc2 <- pROC::roc(labels, predictions2)
  return(pROC::roc.test(roc1, roc2, paired=T, method='delong')$p.value)
}


#' Title
#'
#' @param predictions1
#' @param predictions2
#' @param labels
#'
#' @return
#' @export
#'
#' @examples
compare_brier <- function(predictions1, predictions2, labels){
  assert_values_only(labels, c(0,1))
  assert_values_range(predictions1, c(0, 1))
  assert_values_range(predictions2, c(0, 1))

  brier_errors1 <- (labels-predictions1)^2
  brier_errors2 <- (labels-predictions2)^2
  err_diffs <- brier_errors2 - brier_errors1
  compare_scores_signflip(err_diffs)
}


#' Title
#'
#' @param predictions1
#' @param predictions2
#' @param labels
#'
#' @return
#' @export
#'
#' @examples
compare_logscore <- function(predictions1, predictions2, labels){
  assert_values_only(labels, c(0,1))
  assert_values_range(predictions1, c(0, 1))
  assert_values_range(predictions2, c(0, 1))

  logscore_errors1 <- get_individual_log_scores(predictions1, labels)
  logscore_errors2 <- get_individual_log_scores(predictions2, labels)
  err_diffs <- logscore_errors2 - logscore_errors1
  compare_scores_signflip(err_diffs)
}


#' Title
#'
#' @param err_diffs
#'
#' @return
#' @export
#'
#' @examples
compare_scores_signflip <- function(err_diffs){
  mean_err_diff <- mean(err_diffs)
  err_diff_null <- list()
  nperms <- 999
  for (i in 1:nperms){
    sign_flips <- sign(sample(c(T,F), length(err_diffs), replace=T) - 0.5)
    sign_flipped_err_diff <- err_diffs * sign_flips
    err_diff_null[[i]] <- mean(sign_flipped_err_diff)
  }
  err_diff_null <- unlist(err_diff_null)
  # rank <- sum(mean_err_diff <= err_diff_null) + 1
  #two tailed test
  rank <- sum(abs(mean_err_diff) <= abs(err_diff_null)) + 1
  pval <- rank  / (nperms+1)
  pval
}


#' Title
#'
#' @param predictions1
#' @param predictions2
#' @param labels
#' @param test
#'
#' @return
#' @export
#'
#' @examples
compare_accuracy <- function(predictions1, predictions2, labels, test='binom'){
  assert_values_only(labels, c(0,1))
  assert_values_only(predictions1, c(0,1))
  assert_values_only(predictions2, c(0,1))

  library(stats)
  accuracy_errors1 <- as.numeric(labels != (predictions1))
  accuracy_errors2 <- as.numeric(labels != (predictions2))

  different_predictions <- table(factor(accuracy_errors1, levels=c(0,1)),
                                 factor(accuracy_errors2, levels=c(0,1)))[c(2,3)]
  if (all(different_predictions == c(0,0))){
    p.value <- 1
  } else {
    p.value <- binom.test(different_predictions[1], sum(different_predictions))$p.value
  }
  if (is.nan(p.value)){
    p.value <- 1
  }
  return(p.value)
}
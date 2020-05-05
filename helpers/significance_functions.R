#' calculate p-value for accuracy using binomial test
#'
#' Calcualte p-value for the null accuracy = chance level
#' @param predictions predicted values
#' @param labels true values
#' @param p proportion against which we are testing
#' @keywords p values, statisticall signifficance, accuracy
#' @export
#' @examples
#' p_val_acc(c(0,0,0,1,1,1), c(0, 0, 1, 0,1,1))
p_val_acc <- function(predictions, labels, p=0.5){
  assert_values_only(predictions, c(0, 1))
  assert_values_only(labels, c(0, 1))
  successes <- sum(predictions == labels)
  binom <- stats::binom.test(successes, length(labels),
                             p=p, alternative = 'greater')
  return(binom$p.value)
}


#' calculate p-value for the AUC statistics
#'
#' Calcualte p-value for the AUC statistics corresponding to null hypothesis AUC == 0
#' @param predictions predicted values
#' @param labels true values
#' @keywords p values, statisticall signifficance, auc
#' @export
#' @examples
#' p_val_auc(c(0.3, 0.4, 0.6, 0.7), c(0, 0, 1, 1))
p_val_auc <- function(predictions, labels){
  assert_values_only(labels, c(0,1))
  # if all the predictions are same, just return p-value of 1 directly
  if (all_the_same(predictions)){
    return(1)
  }
  wilcox <- stats::wilcox.test(predictions ~ labels, alternative='less')
  wilcox$p.value
}


#' null hypothesis signifficance permutation test for brier score
#'
#' Caluclates p value for brier score using a permutaiton test. Either with
#' early stopping rules or with fixed number of permutations
#'
#' @param predictions probabilistic predictions between 0 and 1
#' @param labels true values coded as 0 and 1
#' @param method either 'earlystop' or 'fixedn'
#' @param nperms maximum number of permutations performed
#' @param earlystop_threshold p-value to consider for early stopping
#' @param earlystop_error error we are willing to accept in return for early stopping
#'
#' @return p value
#' @export
#'
#' @examples
p_val_brier <- function(predictions, labels, method='earlystop', nperms=2000,
                        earlystop_threshold=0.05, earlystop_error=0.001){
  assert_values_only(labels, c(0,1))
  assert_values_range(predictions, c(0, 1))
  perm_test(predictions, labels, get_brier_score, method,
            nperms, earlystop_threshold, earlystop_error)
}


#' null hypothesis signifficance permutation test for logarithmic score
#'
#' Caluclates p value for logarighmic score using a permutaiton test. Either with
#' early stopping rules or with fixed number of permutations
#'
#' @param predictions probabilistic predictions between 0 and 1
#' @param labels true values coded as 0 and 1
#' @param method either 'earlystop' or 'fixedn'
#' @param nperms maximum number of permutations performed
#' @param earlystop_threshold p-value to consider for early stopping
#' @param earlystop_error error we are willing to accept in return for early stopping
#'
#' @return p value
#' @export
#'
#' @examples
p_val_logscore <- function(predictions, labels, method='earlystop', nperms=2000,
                           earlystop_threshold=0.05, earlystop_error=0.001){
  assert_values_only(labels, c(0,1))
  assert_values_range(predictions, c(0, 1))
  perm_test(predictions, labels, get_log_score,  method,
            nperms, earlystop_threshold, earlystop_error)
}


#' performs permutation test either with early stopping or with a fixed number
#' of permutations
#'
#' @param predictions
#' @param labels
#' @param perf_measure
#' @param method
#' @param nperms
#' @param earlystop_threshold
#' @param earlystop_error
#'
#' @return
#' @export
#'
#' @examples
perm_test <- function(predictions, labels, perf_measure, method, nperms,
                      earlystop_threshold, earlystop_error){
  assert_values_only(method, c('earlystop', 'fixedn'))
  # if all the predictions are same, just return p-value of 1 directly
  if (all_the_same(predictions)){
    return(1)
  }
  if (method=='fixedn'){
    return(perm_test_fixedn(predictions, labels, perf_measure, nperms))
  } else if (method == 'earlystop') {
    return(perm_test_simctest(predictions, labels, perf_measure, nperms,
                              earlystop_threshold, earlystop_error))
  }
}


#' Perform permutation test for fixed number of permutations
#'
#' @param predictions
#' @param labels
#' @param perf_measure
#' @param nperms
#'
#' @return
#' @export
#'
#' @examples
perm_test_fixedn <- function(predictions, labels, perf_measure, nperms){
  observed_measure <- mean(perf_measure(predictions, labels))
  null_distr <- list()
  for (i in 1:nperms){
    null_distr[[i]] <- mean(perf_measure(sample(predictions), labels))
  }
  rank <- sum(observed_measure > null_distr) + 1
  pval <- rank  / (nperms+1)
  return(pval)
}


#' perform permuation test with early stopping
#'
#' @param predictions
#' @param labels
#' @param perf_measure
#' @param nperms
#' @param earlystop_threshold
#' @param earlystop_error
#'
#' @return
#' @export
#'
#' @examples
perm_test_simctest <- function(predictions, labels, perf_measure, nperms,
                               earlystop_threshold,
                               earlystop_error){
  # library(simctest)
  observed <- mean(perf_measure(predictions, labels))
  null_realization_generator <- function(){
    observed >= mean(perf_measure(sample(predictions), labels))
  }
  res <- simctest::simctest(null_realization_generator,
                  level=earlystop_threshold,
                  epsilon=earlystop_error,
                  maxsteps=nperms)
  res@pos / res@steps
  # mctest.simctest(null_realization_generator, J=J)
}


#' get early-stopping limits for permutation test
#'
#' @param level
#' @param epsilon
#' @param maxsteps
#' @param granurality
#'
#' @return
#' @export
#'
#' @examples
get_simctest_rejection_limits <- function(level=0.05, epsilon=1e-3,
                                          maxsteps=1e4, granurality=1000){
  #TODO: make this analytically instead of empirically
  len <- granurality
  p_vals <- numeric(len)
  steps <- numeric(len)

  for (i in  seq_len(len)){
    p <- i/len
    binom_generator <- function(){
      return(sample(c(0,1), size=1, replace = T, prob = c(1-p, p)))
    }
    res <- simctest::simctest(binom_generator, level=level, epsilon=epsilon,
                    maxsteps = maxsteps)
    p_vals[i] <- res@pos / res@steps # to avoid NA when no decision
    steps[i] <- res@steps
  }
  p_vals_steps_limit <- approxfun(p_vals, steps)
}


#' computes mean sequentially
#'
#' @param previous_mean
#' @param current_number
#' @param i_current
#'
#' @return mean
#' @export
#'
#' @examples
#'   x <- cbind(c(1, 0, 1, 1, 0, 0, 1),
#'              c(1, 1, 0, 1, 0, 1, 0))
#'   seq_means <- list()
#'   expected_means <- list()
#'   for(i in seq_len(nrow(x))){
#'     current_mean <- sequential_mean(current_mean, x[i,], i)
#'     seq_means[[i]] <- current_mean
#'     if(i==1){
#'       expected_means[[i]] <- x[1,]
#'       next()
#'     }
#'     expected_means[[i]] <- colMeans(x[1:i,])
#'   }
#'   seq_means <- do.call(rbind, seq_means)
#'   expected_means <- do.call(rbind, expected_means)
#'   seq_means
#'   expected_means
sequential_mean <- function(previous_mean, current_number, i_current){
  if(i_current == 1){
    return(current_number)
  }
  return(previous_mean*(i_current-1)/i_current + current_number*(1/i_current))
}



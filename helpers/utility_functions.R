
assert_values_only <- function(x, allowed_values){
  stopifnot(all(x %in% allowed_values))
}


assert_values_range <- function(x, allowed_range){
  range_x <- range(x)
  stopifnot(range_x[1] >= min(allowed_range) & range_x[2] <= max(allowed_range))
}


all_the_same <- function(x){
  if (length(unique(x)) == 1){
    return(TRUE)
  } else {
    return(FALSE)
  }
}





# from vignette -----------------------------------------------------------

plot_qq <- function(pvals, main){
  plot(sort(pvals), (1:length(pvals)/length(pvals)), main=main)
  abline(0,1, col='red')
}

pvals_stats <- function(pvals_sims, threshold){
  apply(pvals_sims, 2, function(x){
    sucesses <- sum(x < threshold)
    n <- length(x)
    res <- prop.test(sucesses, n)
    out <- c(res$estimate, res$conf.int)
    names(out) <- c('prop', 'CI.low', 'CI.high')
    return(out)})
}

plot_alpha <- function(x, hline){
  plot(x[1,], ylim=c(0,0.1))
  text(1:4, x[3,]+0.01, labels=colnames(x))
  abline(h=hline, col='red')
  segments(1:4, x[2,], 1:4, x[3,])
}

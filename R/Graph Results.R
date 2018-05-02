#' Plot a histogram of the results
#'
#' @param result The result object
#' @return A histogram showing the distribution of each possible score between 0 (no correct answers) and 1 (all correct answers)
#' @examples
#' plot.result(result1)
#' @export
plot.result <- function(result){
  coltotal <- c()
  qs <- nrow(result$Score)
  maxs <- sum(result$Weights)
  outcomes <- 0:maxs
  totals <- rep(0,maxs+1)
  for(i in 1:ncol(result$Score)){
    coltotal[i] <- sum(result$Score[1:qs,i])
    totals[(coltotal[i]+1)] <- totals[(coltotal[i]+1)] +1
  }
  percs <- totals/ncol(result$Score)
  dat <- data.frame(Result = outcomes/maxs, Percent = percs, row.names = outcomes)
  plot(dat, xlab = "Percentage Correct",
       ylab = "Frequency", main = "Results Histogram", xlim = 0:1,type = "h")
}

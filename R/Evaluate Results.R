#' Calculate the mean score of the results
#'
#' @param result The result object
#' @return The mean score
#'
#' @examples
#' mean_result(result1)
#' @export
mean_result <- function(result){
  columns <- ncol(result$Score)
  ts <- ncol(result$Score)
  qs <- nrow(result$Score)
  maxs <- sum(result$Weights)
  sum(result$Score)/(columns*maxs)
}

#' Calculate the median score of the results
#'
#' @param result The result object
#' @return The median score
#' @examples
#' median_result(result1)
#' @export
median_result <- function(result){
  coltotal <- c()
  ts <- ncol(result$Score)
  qs <- nrow(result$Score)
  maxs <- sum(result$Weights)
  for(i in 1:ts){
    coltotal[i] <- sum(result$Score[1:qs,i])
  }
  median(coltotal)/maxs
}
#' Calculate the mode score of the results
#'
#' @param result The result object
#' @return The mode score
#' @examples
#' mode_result(result1)
#' @export
mode_result <- function(result){
  coltotal <- c()
  ts <- ncol(result$Score)
  qs <- nrow(result$Score)
  maxs <- sum(result$Weights)
  for(i in 1:ts){
    coltotal[i] <- sum(result$Score[1:qs,i])
  }
  coltotal <- coltotal/maxs
  uniqv <- unique(coltotal)
  uniqv[which.max(tabulate(match(coltotal, uniqv)))]
}

#' Create a table of the results
#'
#' @param result the result object
#' @return A table showing percentage frequencies for each score
#' @examples
#' table_result(result1)
#' @export
table_result <-  function(result){
  coltotal <- c()
  ts <- ncol(result$Score)
  qs <- nrow(result$Score)
  maxs <- sum(result$Weights)
  outcomes <- 0:maxs
  totals <- rep(0,maxs+1)
  for(i in 1:ts){
    coltotal[i] <- sum(result$Score[1:qs,i])
    totals[(coltotal[i]+1)] <- totals[(coltotal[i]+1)] +1
  }
  percs <- totals/ncol(result$Score)
  data.frame(Result = outcomes/maxs, Percent = percs, row.names = outcomes)
}

#' Calculate the variance of the scores
#'
#' @param result The result object
#' @return The variance of the scores
#' @examples
#' var_result(result1)
#' @export
var_result <- function(result){
  coltotal <- c()
  ts <- ncol(result$Score)
  qs <- nrow(result$Score)
  maxs <- sum(result$Weights)
  for(i in 1:ts){
    coltotal[i] <- sum(result$Score[1:qs,i])
  }
  ctotal <- coltotal/maxs
  var(ctotal)
}

#' Calculate the portion of scores that are higher than a given percentage score
#'
#' @param result The result object
#' @param pass The 'pass' score, i.e. the minimum score for the desired range
#' @param inclusive Logical value, whether scores equal to the pass score should be included
#' @return A percentage score showing the percentage of scores than were greater than the pass score
#' @examples
#' pass_percentage(result1,0.4)
#' pass_percentage(result1,0.5,FALSE)
#' @export
pass_percentage <- function(result, pass, inclusive = TRUE) {
  coltotal <- c()
  ts <- ncol(result$Score)
  qs <- nrow(result$Score)
  maxs <- sum(result$Weights)
  for(i in 1:ts){
    coltotal[i] <- sum(result$Score[1:qs,i])/maxs
  }
  if(inclusive){
    perc <-  length(coltotal[coltotal>=pass])/ts
  } else {
    perc <-  length(coltotal[coltotal>pass])/ts
  }
  perc
}

#' Calculate the portion of scores that are between a given set of percentage scores
#'
#' @param result The result object
#' @param low The 'pass' score, i.e. the minimum score for the desired range
#' @param high The maximum score for the desired range
#' @param inclusive Logical value, whether scores equal to the low and high score should be included
#' @return A percentage score showing the percentage of scores than were between the two provided scores
#' @examples
#' between_percentage(result1,0.4,0.8)
#' between_percentage(result1,0.5,0.7,FALSE)
#' @export
between_percentage <- function(result, low, high, inclusive = TRUE) {
  coltotal <- c()
  ts <- ncol(result$Score)
  qs <- nrow(result$Score)
  maxs <- sum(result$Weights)
  for(i in 1:ts){
    coltotal[i] <- sum(result$Score[1:qs,i])/maxs
  }
  if(inclusive){
    perc1 <-  coltotal[coltotal<=high]
    perc <-  length(perc1[perc1>=low])/ts
  } else {
    perc1 <-  coltotal[coltotal<high]
    perc <-  length(perc1[perc1>low])/ts
  }
  perc
}

#' Calculate the portion of scores that are lower than a given percentage score
#'
#' @param result The result object
#' @param below The maximum score for the desired range
#' @param inclusive Logical value, whether scores equal to the maximum score should be included
#' @return A percentage score showing the percentage of scores than were lower than the pass score
#' @examples
#' below_percentage(result1,0.8)
#' below_percentage(result1,0.7,FALSE)
#' @export
below_percentage <- function(result, below, inclusive = TRUE) {
  coltotal <- c()
  ts <- ncol(result$Score)
  qs <- nrow(result$Score)
  maxs <- sum(result$Weights)
  for(i in 1:ts){
    coltotal[i] <- sum(result$Score[1:qs,i])/maxs
  }
  if(inclusive){
    perc <-  length(coltotal[coltotal<=below])/ts
  } else {
    perc <-  length(coltotal[coltotal<below])/ts
  }
  perc
}


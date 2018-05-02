#' Create a True/False Quiz
#'
#' @param qus The number of questions in the quiz
#' @param wts The weighting score for each question, can be provided as a single integer, or a vector with number of elements equal to qus
#' @param ats The number of attempts the test taker is allowed to answer the question, can be provided as a single integer, or a vector with number of elements equal to \code{qus}. For any value greater than 1 the answer will be correct every time.
#' @return A test object
#' @examples
#' create_tf(10)
#' create_tf(5,1:5)
#' @export
create_tf <- function(qus, wts = rep(1,qus), ats = rep(1,qus)){

  test <- data.frame(Answers = rep(1,qus), PossAnswers = rep(2,qus), Weights = wts, Attempts = ats)

  test
}

#' Create a Multiple Choice Quiz
#'
#' @param ans The number of answers for each question, can be a single integer, or a vector providing the number of answers for multiple questions
#' @param qus The number of questions in the quiz, not neccesary if a vector is provided for \code{ans}
#' @param wts The weighting score for each question, can be provided as a single integer, or a vector with number of elements equal to \code{qus}
#' @param ats The number of attempts the test taker is allowed to answer the question, can be provided as a single integer, or a vector with number of elements equal to \code{qus}
#' @return A test object
#'
#' @examples
#' create_mc(4,10)
#' create_mc(c(2,2,3,3))
#' create_mc(4,5,1:5,2)
#' @export
create_mc <- function(ans, qus = length(ans), wts = rep(1,qus), ats = rep(1,qus)) {

  if(length(ans) == 1) ans <- rep(ans,qus)

  test <- data.frame(Answers = rep(1,qus),PossAnswers = ans,Weights = wts,Attempts = ats)

  test
}

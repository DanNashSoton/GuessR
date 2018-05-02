#' Create a Multiple Choice Quiz with Multiple Answers Required
#'
#' @param ans The number of answers for each question, can be a single integer, or a vector providing the number of answers for multiple questions
#' @param qus The number of questions in the quiz, not neccesary if a vector is provided for \code{ans}
#' @param cor The number of answers that must be given, for questions where, for example, 2 of 4 answers are correct.
#' @param wts The weighting score for each question, can be provided as a single integer, or a vector with number of elements equal to \code{qus}
#' @param ats The number of attempts the test taker is allowed to answer the question, can be provided as a single integer, or a vector with number of elements equal to \code{qus}
#' @return A test object
#'
#' @examples
#' create_maq(4,10,2)
#' create_maq(c(4,4,5,5),3)
#' create_maq(4,5,1:5,3,2)
#' @export
create_maq <- function(ans,qus,cor = rep(1,qus),wts = rep(1,qus),ats = rep(1,qus)) {

   quiz <- rep(cor,qus)

   if(length(ans) == 1) ans <- rep(ans,qus)

  test <- data.frame(Answers = quiz,PossAnswers = ans,Weights = wts,Attempts = ats)

  test

}

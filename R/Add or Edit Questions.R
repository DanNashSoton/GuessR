#' Add Multiple Choice Questions to an Existing Quiz
#'
#' @param test The test object that you would like to add questions too
#' @param ans The number of answers for each new question
#' @param qus The number of questions to be added to the quiz
#' @param wts The weighting score for each question, can be provided as a single integer, or a vector with number of elements equal to \code{qus}
#' @param ats The number of attempts the test taker is allowed to answer the question, can be provided as a single integer, or a vector with number of elements equal to \code{qus}.
#' @return A new test object, with the new questions appended onto the end
#'
#' @examples
#' add_mc(test1,4,10)
#' add_mc(test1,c(2,2,3,3))
#' add_mc(test1,4,5,1:5,2)
#' @export
add_mc <- function(test, ans, qus, wts = rep(1,qus), ats = rep(1,qus)) {

  quiz <- rep(1,qus)
  if(length(ans) == 1) ans <- rep(ans,qus)

  test <- rbind(test,data.frame(Answers = quiz,PossAnswers = ans,Weights = wts),Attempts = ats)

  test
}

#' Add True/False Questions to an Existing Quiz
#'
#' @param test The test object that you would like to add questions too
#' @param qus The number of questions you would like to add
#' @param wts The weighting score for each question, can be provided as a single integer, or a vector with number of elements equal to \code{qus}
#' @param ats The number of attempts the test taker is allowed to answer the question, can be provided as a single integer, or a vector with number of elements equal to \code{qus}. For any value greater than 1 the answer will be correct every time.
#' @return A new test object, with the new questions appended onto the end
#' @examples
#' add_tf(test1,10)
#' add_tf(test1,5,1:5)
#' @export
add_tf <- function(test, qus, wts = rep(1,qus), ats = rep(1,qus)) {

  quiz <- rep(1,qus)

  test <- rbind(test,data.frame(Answers = quiz,PossAnswers = rep(2,qus),Weights = wts,Attempts = ats))

  test
}
#' Add Multiple Choice Questions requiring multiple answers to an Existing Quiz
#'
#' @param test The test object that you would like to add questions too
#' @param ans The number of answers for each new question
#' @param qus The number of questions to be added to the quiz
#' @param cor The number of answers that must be given, for questions where, for example, 2 of 4 answers are correct.
#' @param wts The weighting score for each question, can be provided as a single integer, or a vector with number of elements equal to \code{qus}
#' @param ats The number of attempts the test taker is allowed to answer the question, can be provided as a single integer, or a vector with number of elements equal to \code{qus}.
#' @return A new test object, with the new questions appended onto the end
#'
#' @examples
#' add_maq(test1,4,10,2)
#' add_maq(test1,c(4,4,5,5),3)
#' add_maq(test1,4,5,1:5,3,2)
#' @export
add_maq <- function(test, ans,qus,cor = rep(1,qus),wts = rep(1,qus),ats = rep(1,qus)) {
  
  quiz <- rep(1,qus)
  if(length(ans) == 1) ans <- rep(ans,qus)
  
  test <- rbind(test,data.frame(Answers = quiz,PossAnswers = ans,Weights = wts),Attempts = ats)
  
  test
}
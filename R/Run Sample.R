#' Run a simulation to create a sample of test scores
#'
#' @param test The test object
#' @param trials The desired number of iterations, i.e. the sample size
#' @param know The 'knowledge' of the test taker, i.e. a knowledge of 0.5 indicates that there is 50 percent chance they will know (meaning provide the correct) answer
#'
#' @return A result object
#'
#' @examples
#' sample_test(test1,100)
#' sample_test(test1,5000,0.25)
#' @export


sample_test <- function(test,trials,know = 0){

  guess <- c()
  correct <- c()
  qs <- nrow(test)
  result <- c()
  result$Score <- matrix(,qs,trials)

  for(i in 1:trials){

    for(j in 1:qs){

      possible <- 1:choose(test[j,2],test[j,1])
      correct[j] <- 0

      r1 <- runif(1)

      if(r1 < know){
        correct[j] = 1
      } else{

        for(k in 1:test[j,4]){

          if(length(possible[possible>0.5]) == 1) {
            guess[j] <- possible[possible>0.5]
          } else {
            guess[j] <- sample(possible[possible>0.5],1)
          }
          if(guess[j] == 1){
            correct[j] <- 1
          } else {
            possible[guess[j]] <- 0
          }
        }
      }
      result$Score[j,i] <- test[j,3]*correct[j]
    }
  }
  class(result) <- "result"

  result$Weights <- test[1:qs,3]

  result
}

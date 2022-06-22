errors_fun <- function(data1, data2){
  n <- length(data1)
  er <- 0
  for (i in 1:n) {
    errors <- sum(data1[,i] %in% data2$wrong)
    er <- er + errors
  }
  return(er)
}
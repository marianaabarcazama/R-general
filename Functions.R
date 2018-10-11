ZeroesPerVariable<-function(X1) {
  D1 = (X1 ==0)
  colSums(D1, na.rm = T)
}

VariableInfo<-function(X1, Choice1){
  if (Choice1 == "Zeroes") {D1 = (X1 == 0) }
  if (Choice1 == "NAs") {D1 <-is.na(X1)}
  colSums(D1, na.rm = T)
}

sterr <- function(x) {sqrt(var(x, na.rm = T))/sqrt(sum(!is.na(x)))}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

geomean <- function(x) {exp(mean(log(x)))}

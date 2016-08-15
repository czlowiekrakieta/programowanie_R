#zadanie 1
kwantyl <- function(x,p) {
  h <- (length(x)-1)*p + 1
  x <- sort(x)
  x[floor(h)] + (h-floor(h))*(x[floor(h)+1]-x[floor(h)])
}

#zadanie 2 - potega wektoryzacji
kwantyle <- function(x, p) {
  x <- sort(x)
  h <- (length(x)-1)*p + 1
  x[floor(h)] + (h-floor(h))*(x[floor(h)+1]-x[floor(h)])
}

#zadanie 3
wilcoxon <- function(x,y) {
  gdzie_rowne <- which(x==y)
  x <- x[!gdzie_rowne]
  y <- y[!gdzie_rowne]
  
  r <- rank(abs(y-x))
  W <- sum( sign(y-x)*r )
}

#zadanie 4
hirsch <- function(x) {
  v <- length(x):1
  
  gdzie_m <- which(v<x)
  x[gdzie_m] <- v[gdzie_m]
  floor( max(x) )
}
#zadanie 1
taSamaKlasa <- function(x) {
  z <- lapply(l,class)
  l <- length(unique(z))
  if ( l == 1) {
    return(z[[1]])
  } else {
    return(FALSE)
  }
}

#zadanie 2
zakres <- function(x) {
  mi <- min(x)
  ma <- max(x)
  
  l <- list("max"=ma, "min"=mi, "wartosci"=x)
  class(l) <- "zakres"
  
  l
}

print.zakres <- function(x) {
  z <- zakres(x)
  
  cat(" max = ", z$max, "\n", "min = ", z$min, "\n", "wartosci = ", z$wartosci, "\n")
}

#zadanie 3
textHist <- function(x, z="*") {
  h <- hist(x, plot=FALSE)
  
  n <- length(h$counts)
  m <- max(h$counts)
  for ( i in 1:n) {
    cat(rep(z, h$counts[i]), rep(" ", m-h$counts[i]), "\t", h$breaks[i], "-", h$breaks[i+1], "\n")
  }
  
}

#zadanie 4
combapply <- function(x, f) {
  LF <- length(f)
  N <- length(x)
  
  w <- vector(LF, mode="list")
  for ( i in 1:LF ) {
    w[[i]] <- lapply(x, f[[i]])
  }
  names(w) <- names(f)
  w
}

#zadanie 5
bisekcja <- function(f, a, b, eps=10e-16, maxiter = 100) {
  i <- 0
  while ( i < maxiter & abs(f((a+b)/2)) > eps) {
    i <- i + 1
    x <- (a+b)/2
    if ( f(b) > 0 & f(a) < 0 ) {
      if ( f(x) > 0 ) {
        b <- x
      } else {
        a <- x
      }
    } else {
      if ( f(x) > 0 ) {
        a <- x
      } else {
        b <- x
      }
    }
  }
  list(root=x, f.root=f(x), iter=i, estim.prec=(a+b)/2)
}

#zadanie 6 
newton.raphson <- function(f, fp, x0, eps=10e-16, maxiter=100) {
  i <- 0
  while ( i < maxiter & abs(f(x0)) > eps ) {
    x <- x0 - f(x0)/fp(x0)
    x0 <- x
    i <- i + 1
  }
  list(root = x0, f.root = f(x0), iter = i, estim.prec=NA)
}

#zadanie 7
zloty.podzial <- function(f, a, b, eps=10e-16, maxiter=100) {
  i <- 0
  fi <- (sqrt(5)-1)/2
  while ( i < maxiter & b-a > eps ) {
    l <- b - fi*(b-a)
    p <- a + fi*(b-a)
    if ( f(p) > f(l) ) {
      a <- l
    } else {
      b <- p
    }
    i <- i + 1
  }
  list(par = (a+b)/2, value=f((a+b)/2), counts=i, message=NULL )
}
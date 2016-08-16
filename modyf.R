#zadanie 1

w_cumsum <- function(x) {
  w <- c(x[1])
  for ( i in 2:length(x)) {
    w <- c(w, w[i-1]+x[i])
  }
  w
}

w_diff <- function(x) {
  w <- c()
  for ( i in 2:length(x)) {
    w <- c(w, x[i]-x[i-1])
  }
  w
}

w_which <- function(x, war) {
  w <- c()
  for ( i in 1:length(x)) {
    if (war[i]==TRUE) {
      w <- c(w, i)
    }
    
  }
  w
}

w_whichmin <- function(x) {
  g_min <- 1
  for ( i in 1:length(x)) {
    if ( x[i] < x[g_min]) {
      g_min <- i
    }
    
  }
  g_min
}

w_range <- function(x) {
  mi <- x[1]
  ma <- x[1]
  for ( i in 1:length(x)) {
    if ( x[i] < mi) {
      mi <- x[i]
    }
    if ( x[i] > ma) {
      ma <- x[i]
    }
  }
  c(mi, ma)
}

#zadanie 2
kendall <- function(x, y) {
  c <- 0
  n <- length(x)
  for ( i in 2:n ) {
    l <- length( which( x[1:i-1] < x[i] & y[1:i-1] < y[i]  )  )
    m <- length( which( x[1:i-1] > x[i] & y[1:i-1] > y[i]  )  )
    c <- c + l + m
  }
  4*c/(n*(n-1)) - 1
}

#zadanie 3
ile_k <- function(x, k) {
  if ( length(x[x<1 | x>k]) > 0 | k == 0) {
    return(NA)
  }
  il <- c()
  for ( i in 1:k) {
    il <- c(il, sum(x==i))
  }
  il
}

#zadanie 4
bucketsort <- function(x) {
  if ( sum(x>0) ) {
    ma <- max(x[x>0])
    d <- ile_k(x[x>0], ma)
  } else {
    ma <- 0
    d <- 0
  }
  if ( sum(x<0) ) {
    mi <- min(x[x<0])
    u <- ile_k(abs(x[x<0]), abs(mi))
  } else {
    mi <- 0
    u <- 0
  }
  
  z <- sum(x==0)
  c( rep(mi:-1, rev(u) ), rep(0, z), rep(1:ma, d)  )
}

#zadanie 5
podziel <- function(x, a) {
  a <- sort(a)
  K <- length(a)
  v <- c(sum(x<a[1]), rep(0, K-1), sum(x>a[K]))
  
  k <- 2:K
  for (k in 2:K) {
    v[k] <- sum(x>=a[k-1] & x<a[k])
  }
  v
}

#zadanie 6 - zaimplementowac wersje z diff i sprawdzic szybkosc
liniowa <- function(x, y, z) {
  if (sum(diff(x)<0) > 0) {
    return(NA)
  }
  v <- c()
  for ( i in 1:length(z) ) {
    
    j <- which.min(abs(x-z[i]))
    
    if ( x[j] > z[i] ) {
      j <- j-1
    }
    v <- c(v,  y[j] + (y[j+1]-y[j])*(z[i]-x[j])/(x[j+1]-x[j]))
  }
  v
}

#zadanie 7
kombinuj <- function(l) {
  N <- length(l)
  k <- length(l[[1]])
  c <- TRUE
  for ( i in 1:N) {
    if ( length(l[[i]]) != k) {
      c <- FALSE
      break
      }
  }
  if ( !c ) {
    return(NULL)
  }
  
  split(unlist(l), rep(1:k, N))
  }

#zadanie 8
podziel1 <- function(x) {
  a <- floor(min(x))
  b <- ceiling(max(x))
  
  vect <- rep(0, b-a)
  x <- sort(x)
  for ( i in 1:(b-a)) {
    vect[i] <- sum(x>a+i-1 & x<a+i)
  }
  split(x, rep(1:(b-a), vect))
}

#zadanie 9
mnozskal.trap <- function(x, k) {
  M <- length(k)
  N <- length(x)
  if ( M > N ) {
    gdzie <- which(k<0)
    nowa <- split(unlist(x)*rep(k, each=4), rep(1:M, each=4))
    
    lapply( x[gdzie], rev)
  }
  else {
    u <- rep_len(k, N)
    gdzie <- which( u < 0 )
    lapply( x[gdzie], rev)
    nowa <- split( unlist(x)*rep(u, each=4), rep(1:N, each=4) )
  }
  nowa
}


#zadanie 10
srednia.trap <- function(l) {
  N <- length(l)
  
  kombinuj( lapply(kombinuj(l), mean))
}
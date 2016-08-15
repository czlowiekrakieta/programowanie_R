#zadanie 1

x <- round( rnorm(100, 0, 1), 2)
wyk_zad_1 <- function(x) {
print(x[x>-2 & x<-1 | x > 1 & x < 2])

print(sum(x>0))

print(mean( abs(x) ))

print(x[which.max(abs(x-0))])
print(x[which.min(abs(x-0))])

print(x[which.max(abs(x-2))])
print(x[which.min(abs(x-2))])

print( abs(trunc(x) - x))

print( (x-min(x))/(max(x)-min(x)) )

print( c( rep("ujemna", sum(x<0))), rep("nieujemna", sum(x>=0))[rank(x)] )

print( floor(x) + 1/2 )
}

#zadanie 3
s_los_zm <- function(x) {
  diff(x)^2/2*(length(x)-1)
}

#zadanie 4
pearson <- function(x, y) {
  mX <- mean(x)
  mY <- mean(y)
  r <- 1/((length(x)-1)*sd(x)*sd(y))*sum( (x-mX)*(y-mY) )
}

#zadanie 5
as <- function(x) {
  m <- mean(x)
  a <- (sum(x<m) - sum(x>m))/length(x)
}

#zadanie 6
agr <- function(x) {
  N <- length(x)
  x <- sort(x)
  #moglbym sie co prawda przespacerowac petla - mialbym O(n), ale tak jest wygodniej
  c("min"=x[1], "max"=x[N], "mediana"=x[N/2+1]/2+x[N/2]/2)
}

#zadanie 7
odst <- function(x) {
  q <- as.numeric( quantile(x, c(.25, .5 ,.75) ) )
  iq <- q[3] - q[1]
  sum( x>iq+3/2*q[2] | x<iq-3/2*q[2] )
}

#zadanie 8
sr_k <- function(x, k) {
  N <- length(x)
  if ( (N - 1/2)) {
    r <- rank(x)
    m <- mean(x[r[r>k & r<=N-k]])
    m
  }
}


#zadanie 9
windsor <- function(x, k) {
  if (k <= (length(x)-1)/2) {
    r <- rank(x)
    if ( length(x[r==(k+1)]) & length(x[r==N-k])) {
      x[r<=k] <- x[r==(k+1)]
      x[r>N-k] <- x[r==N-k]
    }
    m <- mean(x)
    m
  }
}

#zadanie 10
owa <- function(x, w) {
  if ( sum(w) == 1) {
    sum(x*w)
  }
}

#zadanie 11
owm <- function(x, c) {
  z <- rev(x)
  p <- which(z>c)
  z[p] <- c[p]
  max(z)
}

#zadanie 12
gdzie0 <- function(x) {
  which(diff(x)==0)
}

#zadanie 13
wypelnij_NA_sr <- function(x) {
  x[which(is.na(x))] <- mean(x[which(!is.na(x))])
}

wypelnij_NA_ob <- function(x) {
  gdzie <- which(is.na(x))
  x[gdzie] <- 1/2(x[g-1] + x[g+1])
}

#zadanie 14

#TODO

#zadanie 15
sr_var_dystr <- function(x, p) {
  m <- sum(x*p)
  v <- sum(x*x*p) - m*m
  s <- sqrt(v)
  c(m,v,s)
}

dystrybuanta <- function(p) {
  cumsum(p)
}
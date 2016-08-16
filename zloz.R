#zadanie 1
w_diag <- function(x) {
  if ( class(x) != "matrix"  ) {
    return(NULL)
  } else{
    if ( dim(x)[1] != dim(x)[2]) {
      return(NULL)
    }
  }
  
  n <- dim(x)[1]
  k <- seq(1,n*n,by=(n+1))
  prod(x[k])
}

#zadanie 2
bmi <- function(waga, wzrost) {
  b <- waga/(wzrost*wzrost)
  et <- rep("c", length(b))
  et[which(b<18.5)] <- "niedowaga"
  et[which(b>=18.5 & b < 25)] <- "norma"
  et[which(b> 25)] <- "nadwaga"

  data.frame(waga=waga, wzrost=wzrost, bmi=b, opis=et)
}

#zadanie 3 - zapewne mozna to zoptymalizowac, ale nie wiem jak
dane_status <- function(ramka) {
  d <- ramka$SES
  d[which(d<18)] <- 1
  d[which(d>=18 & d < 26)] <- 2
  d[which(d>=26 & d < 34)] <- 3
  d[which(d>=34 & d < 42)] <- 4
  d[which(d>=42)] <- 5
  
  l <- ramka$lang
  p <- data.frame(SES = d, lang = l)
  
  sr <- aggregate(lang~SES, p, FUN = function(x) c(srednia=mean(x), mediana=median(x), minimum=min(x), maximum=max(x)) )
  sr
}

#zadanie 4
ptaki <- function(ramka) {
  ramka[names(sort( unlist(lapply(ramka,sum)), decreasing = TRUE ) )]
}

#zadanie 5 
wina <- function(ramka) {
  ze_srednimi <- transform(ramka, sr_temp = s.temp/2 + h.temp/2, sr_deszcz = h.rain/2 + w.rain/2)
  posortowane <- ramka[order(ramka$s.temp, decreasing = TRUE ), ][1:10,]
  row.names(posortowane) <- posortowane$year
  subset(posortowane, select = -year)
  kendall <- cor(ramka$parker, ramka$price, method = "kendall", use = "complete.obs" )

}

#zadanie 6
predkosc <- function(ramka) {
  rok1_lim <- which(ramka$year == 1961 & ramka$limit == "yes")
  rok1_bezlim <- which(ramka$year == 1961 & ramka$limit == "no")
  rok2_lim <- which(ramka$year == 1962 & ramka$limit == "yes")
  rok2_bezlim <- which(ramka$year == 1962 & ramka$limit == "no")
  
  liczba_dni <- matrix(c(length(rok1_lim), length(rok2_lim), length(rok1_bezlim), length(rok2_bezlim)), nrow=2)
  laczna <- matrix(c( sum(ramka[rok1_lim, "y"]), sum(ramka[rok2_lim, "y"]), sum(ramka[rok1_bezlim, "y"]), sum(ramka[rok2_bezlim, "y"])  ), nrow=2)
  sr <- matrix(c( mean(ramka[rok1_lim, "y"]), mean(ramka[rok2_lim, "y"]), mean(ramka[rok1_bezlim, "y"]), mean(ramka[rok2_bezlim, "y"])  ), nrow=2)

}

#zadanie 7
niezaleznosc <- function(ramka) {
  wiersze <- split(ramka, rep(1:4, length.out=16))
  sumy <- as.numeric(unlist(lapply(wiersze, sum)))
  
  m <- matrix(rep(sumy, each=4)*rep(sumy, length.out=16), nrow=4)
  mean( m == ramka ) == 1
}

statystyka <- function(ramka) {
  oceny <- as.numeric(rownames(ramka))
  w <- split(ramka, rep(1:4, each=4))
  p <- as.numeric(unlist(lapply(w, sum)))
  
  sr_x <- sum(p*oceny)
  sr_y <- sr_x
  
  var_x <- sum(p*oceny*oceny) - sr_x*sr_x
  var_y <- var_x
  
  wiersze <- split(ramka, rep(1:4, length.out=16))
  sumy <- as.numeric(unlist(lapply(wiersze, sum)))
  
  m <- rep(sumy, each=4)*rep(sumy, length.out=16)
  
  cov <- sum(m*rep(oceny, each=4)*rep(oceny, length.out=16)) - sr_x^2
  
  r <- cov/sqrt(var_x*var_y)
  
  list("średnia X"=sr_x, "średnia Y"=sr_y, "wariancja X"=var_x, 
       "wariancja Y"=var_y, "kowariancja"=cov, "korelacja"=r)
}

#zadanie 8
mnoz <- function(A, B) {
  if ( dim(A)[2] != dim(B)[1] ) {
    return(NULL)
  }
  
  wiersze <- unlist(rep( split(A, rep(1:dim(A)[1], length.out = length(A))), each = dim(B)[2] ))
  kolumny <- rep( as.vector(B), times = dim(A)[1] )
  
  X <- split( wiersze*kolumny, rep( 1:(dim(A)[1]*dim(B)[2]), each=dim(B)[1] ))
  X <- unlist(lapply(X, sum))
  X <- matrix(X, ncol=dim(B)[2], byrow=TRUE)
  X
}

#zadanie 9
agregacja <- function(x, g, f) {
  
  poziomy <- unique(x$g)
  w <- data.frame(row.names = names(x))
  for ( i in 1:length(poziomy)) {
    z <- subset(x, g == poziomy[i], select = - g)
    w <- rbind(w, apply(z, FUN=f, MARGIN=2))
  }
  w
}

#zadanie 10
srednia_ruchoma <- function(x, k) {
  n <- length(x)
  w <- c()
  for ( i in 1:(n-k)) {
    w <- c(w, mean(x[i:(i+k-1)]))
  }
  w
}

#zadanie 11
mydiag <- function(x) {
  if ( class(x) == "numeric") {
    m <- matrix(rep(0, x*x), nrow=x)
    m[ seq(1, x*x, by=(x+1)) ] <- 1
    return(m)
  }
  
  if ( class(x) == "matrix" & dim(x)[1] == dim(x)[2] ) {
    return( x[seq(1, dim(x)[1]*dim(x)[1], by=(dim(x)[1]+1)) ])
  }
  
  if ( class(x) == "matrix" ) {
    x <- x[ 1:min( dim(x)[1], dim(x)[2]) , 1:min( dim(x)[1], dim(x)[2])]
    return( x[seq(1, dim(x)[1]*dim(x)[1], by=(dim(x)[1]+1)) ])
  }

  if ( class(x) != "matrix" & class(x) != "numeric") {
    return(NULL)
  }
}

#zadanie 14
jas.zachlanny <- function(M) {
  i <- 1
  j <- 1
  p <- M[i,j]
  while ( i < dim(M)[1]) {
    if ( M[i+1, j+1] > M[i+1, j]) {
      i <- i+1
      j <- j+1
    } else {
      i <- i+1
    }
    p <- p + M[i,j]
  }
  p
}

#zadanie 15
jas.dynamiczny <- function(M) {
  B <- M
  for ( i in (dim(M)[1]-1):1 ) {
    for ( j in (1:i) ) {
      B[i,j] <- M[i,j] + max( B[i+1,j+1], B[i+1, j] )
    }
  }
  B[1,1]
}

#zadanie 16
rozwin <- function(M, nazwy) {
  w <- rep(rownames(M), times=dim(M)[2])
  k <- rep(colnames(M), each=dim(M)[1])
  d <- data.frame(as.vector(M), w, k)
  names(d) <- nazwy
  d
}

#zadanie 16
zwin <- function(M) {
  M <- M[order(M[, 2], M[, 3]), ]
  w <- unique(M[,2])
  k <- unique(M[,3])
  
  M <- matrix(M[, 1], nrow=length(k))
  rownames(M) <- k
  colnames(M) <- w
  t(M)
}
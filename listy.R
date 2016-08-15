#zadanie 1

listuj <- function(x) {
  list(ujemne = x[x<0], zero = x[x==0], dodatnie = x[x>0])
}

#zadanie 2
jeden_napis <- function(l) {
  unlist(l, use.names=FALSE)
}

#zadanie 3
ktoryNajdl <- function(l) {
  which.max(lengths(l))
}


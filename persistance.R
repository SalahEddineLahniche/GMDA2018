library(TDA)
x <- seq(-5,5,0.01)
y <- dnorm(x,0,1)
y0 <- 0.5*dnorm(x,4,1)

f <- function(xx, grid){
  return(-(dnorm(xx,0,1)+dnorm(xx,4,1)*0.5))
}
z <- array( c((-y+y0)  , x) , dim = c( length(x),2 ) )
diag.info<- gridDiag(X=coords, FUN=f, lim=c(-5,5), by=0.01, sublevel = TRUE, library = 'PHAT', printProgress=FALSE)
#par(mfrow=c(2,1))
plot(diag.info$diagram)
plot(diag.info$diagram, barcode=TRUE)

coords = read.table("coords.txt")
energies = read.table("C:/Users/Alaa Eddine MAHI/Desktop/hybrid-TRRT-BH-BLN__energies_below_100.txt")
f <- approxfun(unlist(coords), unlist(energies), method = "linear")

g <- function(xx, grid){
  return(f(xx))
}

f <- splinefun(x = unlist(coords), y = unlist(energies),
          method = "fmm")
g <- function(xx, grid){
  return(f(xx))
}
diag.info<- gridDiag(X=coords, FUN=g, lim=c(-5,5), by=0.01, sublevel = TRUE, library = 'PHAT', printProgress=FALSE)
plot(diag.info$diagram)

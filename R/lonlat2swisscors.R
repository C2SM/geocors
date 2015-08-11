`lonlat2swisscors` <-
function(lon, lat) {
# input for lon and lat in degrees

  # rename variables
  lam <- lon
  phi <- lat

  # allow for xy.coord input
  if (class(lam) == "list") { 
     a <- lam[[1]]; b <- lam[[2]]
     lam <- a; phi <- b 
     rm(a,b)
  }

  # allow for matrix input
  if (class(lam) == "matrix") { 
     if (dim(lam)[2] != 2) {
        stop("Matrix lon does not have two columns.")
     }
     a <- lam[,1]; b <- lam[,2]
     lam <- a; phi <- b 
     rm(a,b)
  }

  ## checks
  if (length(lam) != length(phi)) {
    stop(message = 'Unequal length of lamda- & phi-coordinates !')
  }

  if (min(lam,na.rm=TRUE) < 2.0 | max(lam,na.rm=TRUE) > 18.0) {
    stop(message = 'Longitude (lam) seems to be far outside of Switzerland!')
  }

  if (min(phi,na.rm=TRUE) < 42.0 | max(phi,na.rm=TRUE) > 50.0) {
    stop(message = 'Latitude (phi) seems to be far outside of Switzerland!')
  }

  ## convert into seconds and dashed quantities
  lam.d <- (lam * 36 * 100 - 26782.5) / 10000
  phi.d <- (phi * 36 * 100 - 169028.66) / 10000

  ## calculate x and y in m
  y <- 600072.37 + 211455.93 * lam.d - 10938.51 * lam.d * phi.d -
         0.36 * lam.d * phi.d^2 - 44.54 * lam.d^3

  x <- 200147.07 + 308807.95 * phi.d + 3745.25 * lam.d^2 +
         76.63 * phi.d^2 - 194.56 * lam.d^2 * phi.d +
         119.79 * phi.d^3

  # note strange convention for x,y
  if (length(x) > 1) { ll <- cbind(y,x) } else { ll <- c(y,x) }
  ll
}


`lonlat2etrs.austria` <- 
function(lon,lat) {
	
  # geographical coordinates to ETRS89 Austria Lambert
  # http://www.epsg.org/guides/docs/G7-2.pdf, pp. 18-19, version July 2012
  # http://www.spatialreference.org/ref/epsg/3416/
  # http://www.geoland.at/gps_trans.htm
  
  # Lambert Conformal Conic 2SP
  
  # constants
  EF <- 400000                # false easting
  NF <- 400000                # false northing
  phi1d <- 49                 # latitude of standard parallel 1 (degrees)
  phi2d <- 46                 # latitude of standard parallel 2 (degrees)
  a <- 6378137                # ellipsoid
  invf <- 298.257222101       # 1/f, inverse flattening
  phiFd <- 47.5               # latitude of false origin (degrees)
  lamFd <- 13.33333333333333  # longitude of false origin (degrees)
  
  # transformation to radians
  gg <- pi/180
  phi <- lat*gg 
  lam <- lon*gg
  phi1 <- phi1d*gg
  phi2 <- phi2d*gg
  phiF <- phiFd*gg
  lamF <- lamFd*gg

  # derived constants
  f <- 1/invf
  e  <- sqrt(2*f-f^2)    # eccentricity
  m1 <- cos(phi1)/sqrt( 1 - e^2 * (sin(phi1)^2) )
  m2 <- cos(phi2)/sqrt( 1 - e^2 * (sin(phi2)^2) )
  t1 <- tan(pi/4-phi1/2) / (((1-e*sin(phi1))/(1+e*sin(phi1)))^(e/2))
  t2 <- tan(pi/4-phi2/2) / (((1-e*sin(phi2))/(1+e*sin(phi2)))^(e/2))
  tF <- tan(pi/4-phiF/2) / (((1-e*sin(phiF))/(1+e*sin(phiF)))^(e/2))
  n  <- (log(m1)-log(m2))/(log(t1)-log(t2))
  F  <- m1/(n*t1^n)
  rF  <- a*F*tF^n
  
  # calculations
  t <- tan(pi/4-phi/2) / (((1-e*sin(phi))/(1+e*sin(phi)))^(e/2))
  r  <- a*F*t^n
  theta <- n*(lam-lamF)
  X <- EF + r * sin(theta)
  Y <- NF + rF - r * cos(theta)
  
  # output
  if (length(X) > 1) {
        res <- cbind(X, Y)
  } else {
        res <- c(X, Y)
  }
  res
}

`lonlat2etrs.utm32` <- 
function(lon,lat) {
	
  # geographical coordinates to ETRS89 UTM zone 32N
  # http://www.spatialreference.org/ref/epsg/25832/
  # http://www.epsg.org/guides/docs/G7-2.pdf, pp. 47-48, version July 2012
  # JHS formulas
  
  
  # constants
  EF <- 500000              # false easting (meters)
  NF <- 0                   # false northing (meters)
  lam0d <- 9                # longitude of false origin (degrees)
  phi0d <- 0                # latitude of false origin (degrees)
  kO <- 0.9996              # scale factor
  a <- 6378137              # ellipsoid (meters)
  invf <- 298.257222101     # inverse flattening
  
  # transformation to radians
  gg <- pi/180
  lam <- lon*gg
  phi <- lat*gg
  lam0 <- lam0d*gg
  phi0 <- phi0d*gg
  
  # derived constants
  f <- 1 / invf             # flattening
  e  <- sqrt(2*f-f^2)       # eccentricity
    
  n <- f / (2-f)
  B <- (a / (1+n)) * (1 + n^2/4 + n^4/64)
  h1 <- n/2 - (2/3)*n^2 + (5/16)*n^3 + (41/180)*n^4
  h2 <- (13/48)*n^2 - (3/5)*n^3 + (557/1440)*n^4
  h3 <- (61/240)*n^3 - (103/140)*n^4
  h4 <- (49561/161280)*n^4
  
  M0 <- 0                   # meridional arc distance from 
                            # equator to phi0 in case (phi0==0)
  
  # calculations
  Q <- asinh(tan(phi)) - (e*atanh(e*sin(phi)))
  beta <- atan(sinh(Q))
  eta0 <- atanh(cos(beta) * sin(lam - lam0))
  xi0 <- asin(sin(beta) * cosh(eta0))
  
  xi1 <- h1 * sin(2*xi0) * cosh(2*eta0)
  xi2 <- h2 * sin(4*xi0) * cosh(4*eta0)
  xi3 <- h3 * sin(6*xi0) * cosh(6*eta0)
  xi4 <- h4 * sin(8*xi0) * cosh(8*eta0)
  xi <- xi0 + xi1 + xi2 + xi3 + xi4
  
  eta1 <- h1 * cos(2*xi0) * sinh(2*eta0)
  eta2 <- h2 * cos(4*xi0) * sinh(4*eta0)
  eta3 <- h3 * cos(6*xi0) * sinh(6*eta0)
  eta4 <- h4 * cos(8*xi0) * sinh(8*eta0)
  eta <- eta0 + eta1 + eta2 + eta3 + eta4
  
  X <- EF + kO*B*eta        # easting
  Y <- NF + kO*(B*xi - M0)  # northing
  
  
  # output
  if (length(X) > 1) {
        res <- cbind(X, Y)
  } else {
        res <- c(X, Y)
  }
  res
}
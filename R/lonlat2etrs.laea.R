`lonlat2etrs.laea` <- 
function(lon,lat) {
	
  # http://www.epsg.org/guides/docs/G7-2.pdf   p 73-75, version November 2010
	
  #Êconstants
  a <- 6378137             # semi major axis (m)
  invf <- 298.257222101    # inverse flattening
  X0 <- 4321000            # false easting (m)
  Y0 <- 3210000            # false northing (m)
  lon0 <- 10               # longitude of origin (degrees E)
  lat0 <- 52               # latitude of origin (degrees N)

  # derived constants
  f <- 1 / invf            # flattening of ellipsoid
  e2 <- 2*f-f^2            # first eccentricity squared
  e <- sqrt(e2)
  qp <- (1-e2)*( (1-e2)^(-1) - (2*e)^(-1) * log((1-e)/(1+e)) )
  
  # transformation to radians
  gg <- pi/180
  lam0 <- lon0*gg       
  phi0 <- lat0*gg        
  lam <- lon*gg
  phi <- lat*gg
  rm(gg)

  # function
  `qfun` <- function (phi) {
	            (1-e2)*( sin(phi)/(1-e2*sin(phi)^2) - 
                   (2*e)^(-1)*log( (1-e*sin(phi))/(1+e*sin(phi)) ) )
  }                


  # calculations
  q <- qfun(phi)
  q0 <- qfun(phi0)
  beta <- asin(q/qp)
  beta0 <- asin(q0/qp)
  Rq <- a*sqrt(qp/2)
  D <- (a * cos(phi0)) / (sqrt(1-(e*sin(phi0))^2) * Rq * cos(beta0))
  B <- Rq * sqrt( 2 / ( 1 + sin(beta0)*sin(beta) + cos(beta0)*cos(beta)*cos(lam-lam0) ) )
  X <- X0 + (B*D)*(cos(beta)*sin(lam-lam0))
  Y <- Y0 + (B/D)*(cos(beta0)*sin(beta) - sin(beta0)*cos(beta)*cos(lam-lam0))
  
  # output
  if (length(X) > 1) {
        res <- cbind(X, Y)
  } else {
        res <- c(X, Y)
  }
  res
}        

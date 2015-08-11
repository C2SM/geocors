`etrs.laea2lonlat` <- 
function(X,Y) {
	
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
  e4 <- e^4
  e6 <- e^6
  qp <- (1-e2)*( (1-e2)^(-1) - (2*e)^(-1) * log((1-e)/(1+e)) )
  
  # transformation to radians
  gg <- pi/180
  lam0 <- lon0*gg       
  phi0 <- lat0*gg        
  rm(gg)

  # function
  `qfun` <- function (phi) {
	            (1-e2)*( sin(phi)/(1-e2*sin(phi)^2) - 
                   (2*e)^(-1)*log( (1-e*sin(phi))/(1+e*sin(phi)) ) )
  }                

  # calculations
  q0 <- qfun(phi0)
  beta0 <- asin(q0/qp)
  Rq <- a*sqrt(qp/2)
  D <- (a * cos(phi0)) / (sqrt(1-(e*sin(phi0))^2) * Rq * cos(beta0))
  ro <- sqrt( ((X-X0)/D)^2 + (D*(Y-Y0))^2 )
  C <- 2*asin(ro/(2*Rq))
  beta.dash <- asin( cos(C)*sin(beta0) + ( (D*(Y-Y0)*sin(C)*cos(beta0))/ro ))
  lam <- lam0 + atan( (X-X0)*sin(C) / (D*ro*cos(beta0)*cos(C)-D^2*(Y-Y0)*sin(beta0)*sin(C)) )
  phi <- beta.dash + ((e2/3+31*e4/180+517*e6/5040)*sin(2*beta.dash)) + 
                     ((23*e4/360+251*e6/3780)*sin(4*beta.dash)) +
                     ((761*e6/45360)*sin(6*beta.dash))
  
  # transformation to degrees
  gg <- 180/pi
  lon <- lam*gg       
  lat <- phi*gg        
  rm(gg)
  
  # output
  if (length(lon) > 1) {
        res <- cbind(lon, lat)
  } else {
        res <- c(lon, lat)
  }
  res
  
}                

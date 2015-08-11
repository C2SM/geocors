`etrs.utm322lonlat` <- 
function(X,Y){
	
  # ETRS89 UTM zone 32N to geographical coordinates
  # http://www.spatialreference.org/ref/epsg/25832/
  # http://www.epsg.org/guides/docs/G7-2.pdf, pp. 47-48, version July 2012
  # JHS formulas
  
  # internal renaming 
  E <- X; N <- Y
  
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
  lam0 <- lam0d*gg
  phi0 <- phi0d*gg
  
  # derived constants
  f <- 1 / invf             # flattening
  e  <- sqrt(2*f-f^2)       # eccentricity
    
  n <- f / (2-f)
  B <- (a / (1+n)) * (1 + n^2/4 + n^4/64)
  h1 <- n/2 - (2/3)*n^2 + (37/96)*n^3 - (1/360)*n^4
  h2 <- (1/48)*n^2 + (1/15)*n^3 - (437/1440)*n^4
  h3 <- (17/480)*n^3 - (37/840)*n^4
  h4 <- (4397/161280)*n^4
  
  MO <- 0                   # meridional arc distance from 
                            # equator to phi0 in case (phi0==0)
  
  
  # calculations
  eta <- (E-EF) / (B*kO)
  xi <- ((N-NF) + kO*MO) / (B*kO)
    
  xi1 <- h1 * sin(2*xi) * cosh(2*eta)
  xi2 <- h2 * sin(4*xi) * cosh(4*eta)
  xi3 <- h3 * sin(6*xi) * cosh(6*eta)
  xi4 <- h4 * sin(8*xi) * cosh(8*eta)
  xi0 <- xi - (xi1 + xi2 + xi3 + xi4)
  
  eta1 <- h1 * cos(2*xi) * sinh(2*eta)
  eta2 <- h2 * cos(4*xi) * sinh(4*eta)
  eta3 <- h3 * cos(6*xi) * sinh(6*eta)
  eta4 <- h4 * cos(8*xi) * sinh(8*eta)
  eta0 <- eta - (eta1 + eta2 + eta3 + eta4)
  
  beta <- asin(sin(xi0)/cosh(eta0))
  Qp <- asinh(tan(beta))
  Q <- Qp + (e * atanh(e * tanh(Qp)))
  for (i in 1:3){
    Q <- Qp + (e * atanh(e * tanh(Q)))
  }
    
  lam <- lam0 + asin(tanh(eta0)/cos(beta)) # longitude
  phi <- atan(sinh(Q))                     # latitude
  
  
  # transformation to degrees
  lon <- lam/gg
  lat <- phi/gg
  
  
  # output
  if (length(lon) > 1){
    res <- cbind(lon,lat)
  }else{
    res <- c(lon,lat)
  }
  res
}
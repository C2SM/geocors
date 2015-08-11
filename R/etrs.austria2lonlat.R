`etrs.austria2lonlat` <-
function(X,Y){
  
  # ETRS89 Austria Lambert to geographical coordinates
  # http://www.epsg.org/guides/docs/G7-2.pdf, pp. 18-19, version July 2012
  # http://www.spatialreference.org/ref/epsg/3416/
  # http://www.geoland.at/gps_trans.htm
  
  # Lambert Conformal Conic 2SP
  
  # constants
  a <- 6378137               # ellipsoid (m)
  invf <- 298.257222101      # inverse flattening
  phi1d <- 49                # standard parallel 1 (degrees)
  phi2d <- 46                # standard parallel 2 (degrees)
  phiFd <- 47.5              # latitude of origin (degrees)
  lamFd <- 13.33333333333333 # central meridian (degrees)
  EF <- 400000               # false easting (m)
  NF <- 400000               # false northing (m)

  # transformation to radians
  gg <- pi/180
  phi1 <- phi1d*gg
  phi2 <- phi2d*gg
  phiF <- phiFd*gg
  lamF <- lamFd*gg
  
  # derived constants
  f <- 1/invf        # flattening
  e <- sqrt(2*f-f^2) # eccentricity
  
  m1 <- cos(phi1)/sqrt( 1-e^2*(sin(phi1)^2) )
  m2 <- cos(phi2)/sqrt( 1-e^2*(sin(phi2)^2) )
  t1 <- tan(pi/4-phi1/2)/(((1-e*sin(phi1))/(1+e*sin(phi1)))^(e/2))
  t2 <- tan(pi/4-phi2/2)/(((1-e*sin(phi2))/(1+e*sin(phi2)))^(e/2))
  tF <- tan(pi/4-phiF/2)/(((1-e*sin(phiF))/(1+e*sin(phiF)))^(e/2))
  n <- (log(m1)-log(m2))/(log(t1)-log(t2))
  F <- m1/(n*t1^n)
  rF <- a*F*tF^n
  
  if (n < 0){
    rp <- (((X-EF)^2+(rF-(Y-NF))^2)^(1/2))*(-1)
  }else{
    rp <- ((X-EF)^2+(rF-(Y-NF))^2)^(1/2)
  }
  tp <- (rp/(a*F))^(1/n)
  thetap <- atan((X-EF)/(rF-(Y-NF)))
    
  # calculations
  lam <- (thetap/n)+lamF
  
  phip <- (pi/2)-2*(atan(tp)) # preliminary latitude
  phi <- (pi/2)-2*atan((tp*((1-e*sin(phip))/(1+e*sin(phip)))^(e/2)))
  for (i in 1:3){ # iterative convergence to latitude
    phi <- (pi/2)-2*atan((tp*((1-e*sin(phi))/(1+e*sin(phi)))^(e/2)))
  }
  
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
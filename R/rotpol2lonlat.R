`rotpol2lonlat` <-
function (rlon,rlat,plon,plat) {
#  converts lat-lon coordinates in a rotated pole system into normal lat-lon
#  returns a matrix with two columns (lon, lat)
#  implemented after fortran routines in EM/HM (Majewski, Bresch)

   # parameter renaming
   polphi <- plat; pollam <- plon
   
   # allow for xy.coord input
   if (class(rlon) == "list") { 
      a <- rlon[[1]]; b <- rlon[[2]]
      rlon <- a; rlat <- b
      rm(a,b)
   }

   # allow for matrix input
   if (class(rlon) == "matrix") { 
      if (dim(rlon)[2] != 2) {
        stop("Matrix rlon does not have two columns.")
      }
      a <- rlon[,1]; b <- rlon[,2]
      rlon <- a; rlat <- b 
      rm(a,b)
   }

   #  case without rotation
   if (abs(polphi-90.0)<0.001) {
     return(matrix(c(rlon,rlat),ncol=2))
   }

   # initialize working variables
   zrpi18 <- 360./(2*pi)
   zpir18 <- 2*pi/360
   zsinpol <- sin(zpir18*polphi)
   zcospol <- cos(zpir18*polphi)
   zlampol <- zpir18*pollam

   # preparation
   zlat <- zpir18*rlat
   zlon <- rlon
   ii <- (zlon>180)
   zlon[ii] <- zlon[ii] - 360.0
   zlon <- zpir18*zlon

   # first, the conversion of rlat to lat:
   aarg <- zcospol*cos(zlat)*cos(zlon) + zsinpol*sin(zlat)
   lat <- zrpi18*asin(aarg)

   # then, conversion of rlon to lon:
   zarg1 <- sin(zlampol)*(-zsinpol*cos(zlon)*cos(zlat) +
                           zcospol*sin(zlat)          )-
            cos(zlampol)*sin(zlon)*cos(zlat)
   zarg2 <- cos(zlampol)*(-zsinpol*cos(zlon)*cos(zlat) +
                           zcospol*sin(zlat)          )+
            sin(zlampol)*sin(zlon)*cos(zlat)
   ii1 <- ((abs(zarg2) < 1e-30) & (abs(zarg1) < 1e-30)) 
   ii2 <- ((abs(zarg2) < 1e-30) & (abs(zarg1) >= 1e-30) & (zarg1 > 0))
   ii3 <- ((abs(zarg2) < 1e-30) & (abs(zarg1) >= 1e-30) & (zarg1 <= 0))
   ii4 <- (abs(zarg2) >= 1e-30)
   lon <- zlon
   lon[ii1] <- 0.0   
   lon[ii2] <- 90.0   
   lon[ii3] <- -90.0
   lon[ii4] <- zrpi18*atan(zarg1/zarg2)

   # return matrix
   matrix(c(lon,lat),ncol=2)
}


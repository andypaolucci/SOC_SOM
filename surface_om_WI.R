library(aqp)
library(soilDB)


#load lab data by mlra
mlra89 <- fetchKSSL(mlra="89")
mlra90A <- fetchKSSL(mlra="90A")
mlra90B <- fetchKSSL(mlra="90B")
mlra91B <- fetchKSSL(mlra="91B")
mlra92 <- fetchKSSL(mlra="92")
mlra93B <- fetchKSSL(mlra="93B")
mlra94B <- fetchKSSL(mlra="94B")
mlra94D <- fetchKSSL(mlra="94D")
mlra95A <- fetchKSSL(mlra="95A")
mlra95B <- fetchKSSL(mlra="95B")
mlra104 <- fetchKSSL(mlra="104")
mlra105 <- fetchKSSL(mlra="105")
mlra110 <- fetchKSSL(mlra="110")

#combine data
everything <- union(spc= list(mlra89, mlra90A, mlra90B, mlra91B, mlra92, mlra93B, mlra94B, mlra94D, mlra95A, mlra95B, mlra104, mlra105, mlra110), method="all", na.rm=TRUE, drop.spatial=FALSE)

#subset points using state
WI <- subsetProfiles(everything, s="state == 'Wisconsin'")

coordinates(WI) <- ~ x + y
proj4string(WI) <- "+proj=longlat +datum=WGS84"

# use glom to get upper 15cm of mineral soil from each profile
upper15mineral <- function(spc) { 
  glomApply(spc, function(p) {
    top <- getMineralSoilSurfaceDepth(p, hzdesgn=hzdesgnname(p))
    bottom <- top + 15
  }, truncate=TRUE)
}
WI.upper15mineral <- upper15mineral(WI)

install.packages("raster")
install.packages("soilDB")
install.packages("sf")
install.packages("viridis")

library(raster)
library(soilDB)
library(sf)
library(viridis)

# Get state boundaries
data(us_states, package = "spData")

# Subset wisconsin
wisconsin <- subset(us_states, us_states$NAME == "Wisconsin")

# Look up details of ISSR800 data
WCS_details(wcs = 'ISSR800')

?WCS_details()

# Get som data from ISSR800 units are kg SOM per square meter. Depth of whole soil. 
# Can assume 150 cm since that is the maximum depth for most components
total_som <- ISSR800.wcs(wisconsin, "om_kg_sq_m")

# Plot Data
plot(total_som)
# Add State boundary
plot(st_geometry(st_transform(wisconsin, 5070)), add = TRUE)

# check data for determining breaks 
hist(total_som, breaks=20)

# specify breaks for mapping in R 
cuts=c(0,10,20,40,60,80,100,200,400) #set breaks

pal <- viridis(n=9)

plot(total_som, breaks=cuts, col = pal) #plot with defined breaks

# convert to US tons per acre
total_som_tons_acre <- total_som * 4.4609

#rename
names(total_som_tons_acre)<-"newer"

hist(total_som_tons_acre, breaks=20)

# convert to US tons SOC per acre assuming 0.58 conversion factor 
total_soc_tons_acre <- total_som_tons_acre * 0.58

# check data for determining breaks 
hist(total_soc_tons_acre, breaks=20)

# specify breaks for mapping in R 
cuts=c(0,10,20,40,60,80,100,200,400) #set breaks

pal <- viridis(n=9)




plot(total_som, breaks=cuts, col = pal) #plot with defined breaks

# Write data to raster file for symbolizing in arcmap
writeRaster(total_som, filename=file.path("C:/Paolucci/CarbonStocks", "test.tif"), format="GTiff", overwrite=TRUE)

     
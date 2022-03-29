# latest version of AQP
remotes::install_github("ncss-tech/aqp", dependencies=FALSE, upgrade=FALSE, build=FALSE)


library(raster)
library(maps)
library(sf)
library(lattice)
library(soilDB)
library(reshape2)
library(tools)
library(lwgeom)
library(rgdal)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(aqp) 



# Setup
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# fetch lab data
m89 <- fetchKSSL(mlra = "89")
m90A <- fetchKSSL(mlra = "90A")
m90B <- fetchKSSL(mlra = "90B")
m91B <- fetchKSSL(mlra = "91B")
m92 <- fetchKSSL(mlra = "92")
m93B <- fetchKSSL(mlra = "93B")
m94B <- fetchKSSL(mlra = "94B")
m94D <- fetchKSSL(mlra = "94D")
m95A <- fetchKSSL(mlra = "95A")
m95B <- fetchKSSL(mlra = "95B")
m103 <- fetchKSSL(mlra = '103')
m104 <- fetchKSSL(mlra = '104')
m105 <- fetchKSSL(mlra = "105")
m110 <- fetchKSSL(mlra = "110")


# merge data
wi.kssl <-rbind(m89, m90A, m90B, m91B, m92, m93B, m94B, m94D, m95A, m95B, m103, m104, m105, m110)

# Merge data for two MLRAs for comparison 
wi.kssl <-rbind(m89, m90A)

#Convert texture classes to factor. Might need to do this with other attributes for plotting
wi.kssl$lab_texture_class <- factor(toupper(wi.kssl$lab_texture_class))

#### Map sites ####
#define coordinates/system for group 1 bounding box if needed: xlim=c(-95, -85), ylim=c(40, 50), 
coordinates(wi.kssl)<- ~ x + y
proj4string(wi.kssl) <- '+proj=longlat +datum=WGS84'
sites <- as(wi.kssl, "SpatialPointsDataFrame")
sites.df <- as.data.frame(sites)
str(sites.df)

# convert dataframe to sf object CRS code #4326 is WGS84
sites.sf <- st_as_sf(sites.df, coords = c("x","y"), crs = 4326, agr='constant')

# define states
states <- st_as_sf(map("state", plot=FALSE, fill=TRUE))
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)

# create variable to move state labels up and down
states$nudge_y <- 0
states$nudge_y[states$ID == "Illinois"] <- 1
states$nudge_y[states$ID == "Michigan"] <- -1
states$nudge_y[states$ID == "Indiana"] <- 1

# create variable to move state labels left and right 
states$nudge_x <- 0
states$nudge_x[states$ID == "Minnesota"] <- 0.5
states$nudge_x[states$ID == "Michigan"] <- 0.25

# Plot to inspect distribution of lab points in desired MLRAs
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_sf(data = sites.sf, size = 1, shape = 23, fill = "darkred") +
  geom_label(data = states, aes(X, Y, label = ID), size = 3, fontface = "bold", 
             nudge_y = states$nudge_y, nudge_x =states$nudge_x) +
  coord_sf(xlim = c(-95, -84), ylim = c(40, 48), expand = FALSE)


# Define Counties
counties <- st_as_sf(map("county", plot=FALSE, fill=TRUE)) 
counties <- subset(counties, grepl("wisconsin", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)

#plot . Replace with this if dont want area acres: #geom_sf(data = counties, fill = NA, color = gray(0.1)) +
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, aes(fill = area)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  geom_sf(data = sites.sf, size = 0.5, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-93.5, -86.5), ylim = c(42, 47.5), expand = FALSE)

# Read in MLRA shapefile
mlra.shp <- readOGR(dsn='C:/geodata/MLRA_Boundaries/WI_MLRA.shp')
head(mlra.shp)

# convert MLRA symbols to factor, convert to dataframe, and define labels
mlra.shp$MLRARSYM <-as.factor(mlra.shp$MLRARSYM)
mlra.df <- broom::tidy(mlra.shp, region = "MLRARSYM")
lapply(mlra.df, class)

head(mlra.df)
names <- aggregate(cbind(long, lat) ~ id, data=mlra.df, FUN=mean)
map <- ggplot() + geom_polygon(data = mlra.shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
map + geom_text(data = names, aes(x = long, y = lat, label = id), size = 4) + theme_void()

# convert MLRA symbols to factor 
#mlra.shp@data <- mlra.shp@data %>% mutate(MLRARSYM = as.factor(MLRA.shp$MLRARSYM))
str(mlra.shp)
# Plot with MLRA boundaries symbolized by MLRARSYM
#Generate Color Palette
library(RColorBrewer)
n <-14
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
colors <- sample(col_vector, n)


ggplot(data = world) +
  geom_sf() +
  geom_polygon(data = mlra.df, aes(x = long, y = lat, group = group, fill = id), colour = NA) +
  geom_sf(data = counties, fill = NA, color = "black") +
  #geom_sf(data = sites.sf, size = 0.5, shape = 23, fill = "darkred") +
  geom_sf(data = sites.sf, size = 1) +
  coord_sf(xlim = c(-93.5, -86.5), ylim = c(42, 47.5), expand = FALSE)+
  scale_fill_manual(values=colors)

#aes(shape=mlra)
# Add MLRA labels
#geom_text(data = names, aes(x = long, y = lat, label = id), size = 5, fontface="bold") +

#Alternate Plot
ggplot(data = world) +
  geom_sf() +
  geom_polygon(data = mlra.df, aes(x = long, y = lat, group = group, fill = id), colour=NA) +
  geom_sf(data = states, fill = NA) + 
  geom_sf(data = sites.sf, size = 1, shape = 23, fill = "darkred") +
  #geom_label(data = states, aes(X, Y, label = ID), size = 3, fontface = "bold", 
             #nudge_y = states$nudge_y, nudge_x =states$nudge_x) +
  coord_sf(xlim = c(-95, -84), ylim = c(40, 48), expand = FALSE)



#### 0-30 Summary BoxPlot ####
# remove invalid profiles 
kssldat <- filter(wi.kssl, checkHzDepthLogic(wi.kssl)$valid)

# this truncates the whole SPC to [0,30]
kssltrunc <- trunc(kssldat, 0, 30)

# new way to do it: group_by + summarize
ksslgroup <- group_by(kssltrunc, idname(kssltrunc))

# summarize takes one or more expressions that resolve to 1 value per group
#   default grouping is single profiles; based on profile_id (without a group_by call)
data.sum <- summarize(ksslgroup, 
                  oc_mean = mean(oc, na.rm = TRUE), 
                  ph_h2o_mean = mean(ph_h2o, na.rm = TRUE), 
                  oc_sd = sd(oc, na.rm = TRUE), 
                  ph_h2o_sd = sd(ph_h2o, na.rm = TRUE))

data.sum <-merge(data.sum, site(ksslgroup)[,c("pedon_key","mlra","taxonname")], by="pedon_key")  

par(mar=c(0,0,0,0))
ggplot(data.sum, aes(x=mlra, y=oc_mean, fill=mlra))+ 
  geom_boxplot(alpha=0.7, outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE, show.legend=FALSE)+
  scale_x_discrete(name='Major Land Resource Area (MLRA)')+
  scale_y_continuous(name="SOC % 0-30 cm", breaks=seq(0, 8, 1), limits=c(0,8))+
  ggtitle("Soil Organic Carbon: Upper 30 cm") +
  theme_bw()+
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_brewer(palette = "Accent")

#### Slab Method ####

# slab way to do it; using 0-30cm trunc'd SPC
slabres  <- aqp::slab(ksslgroup, mlra ~ oc + estimated_oc + estimated_om + ph_h2o, 
                      slab.fun = mean, na.rm = TRUE)

head(slabres)

boxplot(min_soimoistdept_r~hydgrp, data=slabres, main="HSG Minimum soil moisture depth", xlab="HSG", ylab="minimum soil moisture depth (cm)")

res2 <- do.call('rbind', lapply(split(slabres, f = slabres$mlra), function(mlra) {

  data.frame(mlra = unique(mlra$mlra),
             oc_mean = mean(subset(mlra, variable == "oc")$value, na.rm = TRUE), 
             ph_h2o_mean = mean(subset(mlra, variable == "ph_h2o")$value, na.rm = TRUE),
             oc_sd = sd(subset(mlra, variable == "oc")$value, na.rm = TRUE), 
             ph_h2o_sd = sd(subset(mlra, variable == "ph_h2o")$value, na.rm = TRUE))
}))


#### XY PLOT SUMMARY ####
#plot setup
tps <- list(superpose.line=list(col=c('RoyalBlue', 'DarkRed', 'DarkGreen'), lwd=2))

#tabulate number of pedons
(mlra.tab <- table(wi.kssl$mlra))

str(wi.kssl)

a <- aqp::slab(wi.kssl, fm=mlra ~ caco3 + ph_h2o + co3_cly)

# compute mid-point between slice top and bottom depth for plotting
a$mid <- with(a, (top+bottom)/2)

# append the number of pedons to the taxonname label
a$mlra <- factor(a$mlra, levels=names(mlra.tab), labels=paste(names(mlra.tab), ' (', mlra.tab, ')', sep=''))

#re-name soil property labels
levels(a$variable) <- c('SOC:Soil Organic Carbon (%)', 'Estimated SOC', 'Estimated SOM')

#plot aggregate data
xyplot(top ~ p.q50 | variable, groups=mlra, data=a, ylab='Depth',
       xlab='median bounded by 5th and 95th percentiles',
       lower=a$p.q25, upper=a$p.q75, ylim=c(30,-1),
       panel=panel.depth_function, alpha=0.25, sync.colors=TRUE,
       prepanel=prepanel.depth_function,
       cf=a$contributing_fraction,
       strip=strip.custom(bg=grey(0.85)),
       layout=c(3,1), scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)), par.settings=tps, auto.key=list(columns=2, lines=TRUE, points=FALSE))

# Process soil profile collection to get average SOM for upper 30 cm
slices <- aqp::slice(wi.kssl, 0:30 ~ oc + estimated_oc + estimated_om + ph_h2o, strict = FALSE, just.the.data = TRUE)

head(slices)

## again, this time for a user-defined slab from 40-60 cm

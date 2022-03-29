# load packages
library(soilDB)
library(aqp)
library(sharpshootR)
library(cluster)
library(RColorBrewer)
library(classInt)
library(rgdal)

# load pedons from NAIS
x <- fetchNASIS()

# subset by taxonname 
x.sub <- x[grep(pattern='Swissranch', x$taxonname, ignore.case=TRUE), ]

# subset by current taxon kind
x.sub.notax <- x.sub[grep(pattern='series', x.sub$taxonkind, ignore.case=TRUE), ] 
plot(x.sub.notax)
str(x.sub)

# slice color data at select depths
s <- slice(x.sub, c(5, 10, 15, 20, 25, 30) ~ soil_color, strict = FALSE)
head(s)

# make horizon labels based on slice depth
s$slice <- paste0(s$hzdept, " cm")
s$slice <- factor(s$slice, levels = guessGenHzLevels(s, "slice")$levels)

par(mar = c(4.5, 2.5, 4.5, 0))
aggregateColorPlot(aggregateColor(s, "slice"), label.cex = 0.65, main = "Shawsflat Moist Colors Depth Slices", print.n.hz = TRUE)

str(pedons)

aggregateColorPlot(aggregateColor(x.sub, 'genhz', col="moist_soil_color"), label.font=2, label.cex=1)
title("dry color")
aggregateColorPlot(aggregateColor(x.sub, 'genhz', col="dry_soil_color"), label.font=2, label.cex=1)

str(x.sub)

# Calculate Horizon Thickness
(x.sub$hzdepb-x.sub$hzdept)-> thickness
x.sub$thickness <-thickness 

# Calculate min and max for generalized horizons 
min(x.sub$thickness[x.sub$genhz=="Bw"], na.rm=TRUE)
max(x.sub$thickness[x.sub$genhz=="Bw"], na.rm=TRUE)

# get dry color slices
q <- slice(x.sub, c(10) ~ moist_soil_color, strict = FALSE)
s <- slice(x.sub, c(20) ~ moist_soil_color, strict = FALSE)
t <- slice(x.sub, c(30) ~ moist_soil_color, strict = FALSE)

# make horizon labels based on slice depth
q$slice <- paste0(q$hzdept, " cm")
q$slice <- factor(q$slice, levels = guessGenHzLevels(q, "slice")$levels)
s$slice <- paste0(s$hzdept, " cm")
s$slice <- factor(s$slice, levels = guessGenHzLevels(s, "slice")$levels)
t$slice <- paste0(t$hzdept, " cm")
t$slice <- factor(t$slice, levels = guessGenHzLevels(t, "slice")$levels)

# plot dry color proportions (all slices included)
par(mar=c(5,5,5,5))
aggregateColorPlot(aggregateColor(q, 'taxonname'), main='Moist color: 10 cm')
aggregateColorPlot(aggregateColor(s, 'taxonname'), main='Moist color: 20 cm')
aggregateColorPlot(aggregateColor(t, 'taxonname'), main='Moist color: 30 cm')

# Diagnostic Property Plot
v <- c('argillic.horizon','shallow', 'mod.deep', 'mollic.epipedon', 'loamy', 'loamy-skeletal', 'lithic.contact', 'paralithic.contact')
diagnosticPropertyPlot(x.sub, v, k=3, dend.label = 'pedon_id', grid.label='tax_suborder')
str(x)


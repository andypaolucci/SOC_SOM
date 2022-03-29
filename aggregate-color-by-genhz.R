library(soilDB)
library(aqp)
library(sharpshootR)

x <-fetchNASIS()

data(loafercreek, package = "soilDB")

# make a new column to store generalized taxonname, and fill with NA
x$generalized_taxname <- rep(NA, times=length(x))

# Sort pedons with intrusive parent materials
x$generalized_taxname[x$taxonname %in% c('Schaad', 'SCHAAD', 'schaad warm', 'Schaad warm', 'Schaad Warm', 'SCHAAD WARM')] <- 'Schaad'
x$generalized_taxname[x$taxonname %in% c('Nedsgulch', 'NEDSGULCH')] <- 'Nedsgulch'


# subset only those pedons with generalized bedrock
x.sub <- subsetProfiles(x, s="!is.na(generalized_taxname)")
plot(x.sub)
groupedProfilePlot(x.sub, groups = 'generalized_taxname')

# generalize horizon names using REGEX rules
n <- c('Oi', 'A', 'BA','Bt1','Bt2','Bt3','Cr','R')
p <- c('O', '^A$|Ad|Ap|AB','BA$|Bw', 'Bt1$|^B$','^Bt$|^Bt2$','^Bt3|^Bt4|CBt$|BCt$|2Bt|2CB$|^C$','Cr','R')
x.sub$genhz <- generalize.hz(x.sub$hzname, n, p)

# slice color data at select depths
s <- slice(x, c(5, 10, 15, 25, 50, 75) ~ soil_color, strict = FALSE)

# make horizon labels based on slice depth
s$slice <- paste0(s$hzdept, " cm")
s$slice <- factor(s$slice, levels = guessGenHzLevels(s, "slice")$levels)

# remove non-matching generalized horizon names
x.sub$genhz[x.sub$genhz == 'not-used'] <- NA
x.sub$genhz <- factor(x.sub$genhz)

par(mar=c(4.5,1.5,4.5,0))
aggregateColorPlot(aggregateColor(x.sub, 'genhz'))

par(mar=c(4.5,4,4.5,0))
aggregateColorPlot(aggregateColor(loafercreek, 'hillslope_pos'))
par(mar=c(4.5,5,4.5,0))
aggregateColorPlot(aggregateColor(loafercreek, 'bedrock_kind'))


a.1 <- aggregateColor(loafercreek, 'genhz')


# plot results with helper function
# pdf(file='aggregate-color-testing.pdf', width=10, height=6)
# png(file='aggregate-color-testing.png', width=900, height=600, res = 120, pointsize = 8, antialias = 'cleartype')
png(file='aggregate-color-testing.png', width=800, height=600, res = 120, pointsize = 8, type='cairo', antialias = 'subpixel')
par(mar=c(4.5,1.5,4.5,0))
aggregateColorPlot(a.1,  main='Loafercreek Series, Dry Colors', print.n.hz = TRUE, label.cex=1)
dev.off()

# aggregate results are the same using both scaling methods
a.1$aggregate.data
a.2$aggregate.data

# load some example data
data(sp1, package='aqp')

# upgrade to SoilProfileCollection and convert Munsell colors
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

# generalize horizon names
n <- c('O', 'A', 'B', 'C')
p <- c('O', 'A', 'B', 'C')
sp1$genhz <- generalize.hz(sp1$name, n, p)

# aggregate colors by generalized horizon
aggregateColorPlot(aggregateColor(sp1, 'genhz'))
aggregateColorPlot(aggregateColor(sp1, 'group'))


library(cluster)
data(sp3)
depths(sp3) <- id ~ top + bottom

pam1 <- function(x,k) list(cluster = pam(x, k, cluster.only=TRUE, stand=TRUE))
d <- horizons(sp3)[, c('clay', 'cec', 'ln_tc')]

g <- clusGap(d, FUN=pam1, K.max=10)
plot(g)

sp3.clara <- clara(d, k=6, stand=TRUE)
plot(silhouette(sp3.clara))

sp3$genhz <- sp3.clara$clustering

plot(sp3, name='genhz')

aggregateColorPlot(aggregateColor(sp3, 'genhz'))


data(sp2)
depths(sp2) <- id ~ top + bottom

# transfer site-level data
site(sp2) <- ~ surface

# generate a new plotting order, based on the dated surface each soil was described on
p.order <- order(sp2$surface)

# plot
par(mar=c(1,0,3,0))
plot(sp2, plot.order=p.order)

# generalize horizon names
n <- c('A', 'B', 'C', 'BC')
p <- c('A', 'B', 'C', 'BC')
sp2$genhz <- generalize.hz(sp2$name, n, p)



png(file='aggregate-color-testing.png', width=800, height=600, res = 120, pointsize = 8, antialias = 'cleartype')
par(mar=c(2,2,3,0), mfrow=c(4,1))
aggregateColorPlot(a.1,  main='Loafercreek Series, Dry Colors', print.n.hz = TRUE, label.cex=1)
aggregateColorPlot(aggregateColor(sp1, 'genhz'), main='Sample Dataset 1, Dry Colors', print.n.hz = TRUE, label.cex=1)
aggregateColorPlot(aggregateColor(sp2, 'genhz'), print.n.hz = TRUE, label.cex=1)
aggregateColorPlot(aggregateColor(sp3, 'genhz'), print.n.hz = TRUE, label.cex=1)
dev.off()



f <- fetchNASIS(rmHzErrors = FALSE)

f.sub <- f[grep('pardee', f$taxonname, ignore.case = TRUE), ]
f.sub$genhz <- factor(f.sub$genhz, levels=guessGenHzLevels(f.sub)$levels)

aggregateColorPlot(aggregateColor(f.sub, 'genhz'), print.n.hz = TRUE, label.cex=1)


# bedrock
f.sub <- f[grep('granite|metavolcanic|marble', f$bedrock_kind, ignore.case = TRUE), ]
f.sub <- slice(f.sub, c(25,35,55) ~ soil_color)

aggregateColorPlot(aggregateColor(f.sub, 'bedrock_kind'), label.cex=1)

# depth slices + single taxon
# f.sub <- f[grep('canisrocks', f$taxonname, ignore.case = TRUE), ]
f.sub <- f[grep('musick', f$taxonname, ignore.case = TRUE), ]
f.sub <- slice(f.sub, c(5, 10, 15, 25, 50, 100, 150) ~ soil_color)

# make fake label
f.sub$slice <- paste0(f.sub$hzdept, ' cm')
f.sub$slice <- factor(f.sub$slice, levels=guessGenHzLevels(f.sub, 'slice')$levels)

aggregateColorPlot(aggregateColor(f.sub, 'slice'), label.cex=0.65, main='Musick', print.n.hz = TRUE)



# geology + depth slices
f.sub <- f[grep('diorite', f$bedrock_kind, ignore.case = TRUE), ]
f.sub <- slice(f.sub, c(5, 10, 15, 25, 50, 100, 150) ~ soil_color)

# make fake label
f.sub$slice <- paste0(f.sub$hzdept, ' cm')
f.sub$slice <- factor(f.sub$slice, levels=guessGenHzLevels(f.sub, 'slice')$levels)

aggregateColorPlot(aggregateColor(f.sub, 'slice'), label.cex=0.65, main='Soils Formed on "Diorite"')




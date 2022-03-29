library(soilDB)
library(aqp)
library(rgdal)
library(plyr)
library(raster)
library(reshape2)
library(RODBC)
library(latticeExtra)

# is this all of the data?
x <- fetchKSSL(mlra='18')
str(x)

# get NASIS data
y <- fetchNASIS(rmHzErrors = FALSE)

# keep the stuff we need
y.site <- site(y)
y.site.sub <- y.site[, c('peiid','pedlabsampnum', 'temp_class', 'tax_order')]
# Remove rows with NA
y.sub <- na.omit(y.site.sub)
# Add NASIS data to lab data
site(x) <- y.sub
str(x)

# Subset CA630 pedons from lab data
x.sub <- subsetProfiles(x, s="!is.na(peiid)")

# classify horizons
h <- c('O', 'A', 'B', 'C')
p <- c('O', 'A', 'B', 'C')

#Apply genhz rules and check matching
x.sub$genhz <- generalize.hz(x.sub$hzn_desgn, new = h, pat = p)
table(x.sub$genhz, x.sub$hzn_desgn)

## calculate OM from OC
x.sub$estimated_om <- ifelse(is.na(x.sub$oc), x.sub$c_tot * 1.724, x.sub$oc * 1.724)

# convert to DF
d <- as(x.sub, 'data.frame')

# select wanted variables
myvars <- c("pedlabsampnum", "pedon_id", "hzn_desgn", "genhz", "hzn_top", "hzn_bot", "sand", "clay", "estimated_oc", "estimated_om")
d.new <- d[myvars]

# melt data
melted <- melt(d.new, id.vars=c("pedlabsampnum", "hzn_desgn", "genhz"), measure.vars=c("estimated_om"))
head(melted)
options(digits=10)
# define summarizing function
summary.function <- function(i, p=c(0.05, 0.25, 0.5, 0.75, 0.95), digits=10) 
{
   # remove NA
   v <- na.omit(i$value)
   # compute quantiles
   q <- quantile(v, probs=p)
   res <- data.frame(t(q))
  
}
 
 # apply summary function to samples collected from each raster
ddply(melted, c('genhz', 'variable'), summary.function) 


               
             
bwplot(genhz ~ estimated_om, data=d.new, xscale.components=xscale.components.log10.3, scales=list(alternating=3, x=list(log=10)), xlab='Estimated Organic Matter (%)')

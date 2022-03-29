
library(raster)
library(rgdal)
library(sp)
library(dplyr)

#read files
soc <-read.csv(file = 'c:/Users/Andrew.Paolucci/Desktop/CA630_SOC.csv')
CA630 <-shapefile("S:/NRCS/Archive_Andy_Paolucci/CA630/CA630_GIS/CA630_a_1_30_2018.shp")

#capitalize musym column name for merge
colnames(soc)[colnames(soc)=="musym"]<-"MUSYM"

#simplify column names containing SOC values
colnames(soc)[colnames(soc)=="oc_10cm_no_duff"]<-"no10"
colnames(soc)[colnames(soc)=="oc_10cm_duff"]<-"yes10"
colnames(soc)[colnames(soc)=="oc_25cm_no_duff"]<-"no25"
colnames(soc)[colnames(soc)=="oc_25cm_duff"]<-"yes25"
colnames(soc)[colnames(soc)=="oc_50cm_no_duff"]<-"no50"
colnames(soc)[colnames(soc)=="oc_50cm_duff"]<-"yes50"
colnames(soc)[colnames(soc)=="oc_100cm_no_duff"]<-"no100"
colnames(soc)[colnames(soc)=="oc_100cm_duff"]<-"yes100"
colnames(soc)[colnames(soc)=="oc_150cm_no_duff"]<-"no150"
colnames(soc)[colnames(soc)=="oc_150cm_duff"]<-"yes150"
colnames(soc)[colnames(soc)=="oc_200cm_no_duff"]<-"no200"
colnames(soc)[colnames(soc)=="oc_200cm_duff"]<-"yes200"

#remove columns that arent needed
soc2 <- soc[, c('MUSYM','compname', 'comppct_r', 'no10', 'yes10', 'no25', 'yes25', 'no50', 'yes50', 'no100', 'yes100','no150', 'yes150', 'no200', 'yes200')]

#check
head(soc2)

#Calculate weighted mean for each mapunit and save to new dataframe
soc.no.duff<-as.data.frame(soc2 %>% group_by(MUSYM) %>% summarise(no10 = weighted.mean(no10, comppct_r),
                                                        no25 = weighted.mean(no25, comppct_r),
                                                        no50 = weighted.mean(no50, comppct_r),
                                                        no100 = weighted.mean(no100, comppct_r),
                                                        no150 = weighted.mean(no150, comppct_r),
                                                        no200 = weighted.mean(no200, comppct_r)))

soc.w.duff<-as.data.frame(soc2 %>% group_by(MUSYM) %>% summarise(yes10 = weighted.mean(yes10, comppct_r),
                                                                  yes25 = weighted.mean(yes25, comppct_r),
                                                                  yes50 = weighted.mean(yes50, comppct_r),
                                                                  yes100 = weighted.mean(yes100, comppct_r),
                                                                  yes150 = weighted.mean(yes150, comppct_r),
                                                                  yes200 = weighted.mean(yes200, comppct_r)))

#merge
soc.no.duff.sp <- merge(CA630, soc.no.duff, by='MUSYM', duplicateGeoms=TRUE, incomparables=NULL, all.x=FALSE)
soc.w.duff.sp <- merge(CA630, soc.w.duff, by='MUSYM', duplicateGeoms=TRUE, incomparables=NULL, all.x=FALSE)

head(x)

#save as new shapefile
shapefile(soc.no.duff.sp, "S:/NRCS/Archive_Andy_Paolucci/CA630/CA630_GIS/CA630_a_1_30_2018SOC_noduff.shp", overwrite=TRUE)
shapefile(soc.w.duff.sp, "S:/NRCS/Archive_Andy_Paolucci/CA630/CA630_GIS/CA630_a_1_30_2018SOC_duff.shp", overwrite=TRUE)


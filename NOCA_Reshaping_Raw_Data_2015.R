#### IMPORTANT NOTE, Jan. 7 2015: The data frame you will ultimately create using this code WILL CONTAIN MISTAKES outlined in word document "List of suspicious things" as well as double % covers in plots that had supplementaries. Make sure you only ever write over the Understory_all file in dropbox if you have good reason to do so because this is the ONLY VERSION with up to date corrections. If you absolutely need to write over the Understory_all file, view "List of suspicious things.docx" to see list of funky things and how I corrected them. ###

setwd("/Users/rachelwilson/Dropbox/Cascades resurveys/NCCO/2015/Data_entry")

############ Reshaping the legacy data into a useable format ##############

legacy.und.raw<-read.csv("Legacy_Understory_raw.csv", header=TRUE, na.strings="")

# Melting dataset into long form

library(reshape2)
legacy.und<-melt(legacy.und.raw, id.vars=c("PLOT", "ELEV", "ASP"))

# A shitload of god-awful, nightmarish nonsense w data frames trying to get 1 column for species and 1 column for cover. This could possibly have been accomplished in 3 lines of code but hey, it works!
## EDIT LATER: If you ever need to do this again; look into tidyr or similar programs

frame.1<-data.frame(legacy.und[legacy.und$variable=="SPEC1",])
frame.1<-data.frame(frame.1, legacy.und[legacy.und$variable=="COV1",])
frame.1<-data.frame(frame.1$PLOT, frame.1$ELEV, frame.1$ASP, frame.1$value, frame.1$value.1)
new.names<-c("PLOT", "ELEV", "ASP", "SPEC", "COV")
names(frame.1)<-new.names

frame.2<-data.frame(legacy.und[legacy.und$variable=="SPEC2",])
frame.2<-data.frame(frame.2, legacy.und[legacy.und$variable=="COV2",])
frame.2<-data.frame(frame.2$PLOT, frame.2$ELEV, frame.2$ASP, frame.2$value, frame.2$value.1)
new.names<-c("PLOT", "ELEV", "ASP", "SPEC", "COV")
names(frame.2)<-new.names
frame.2<-frame.2[complete.cases(frame.2),]

frame.3<-data.frame(legacy.und[legacy.und$variable=="SPEC3",])
frame.3<-data.frame(frame.3, legacy.und[legacy.und$variable=="COV3",])
frame.3<-data.frame(frame.3$PLOT, frame.3$ELEV, frame.3$ASP, frame.3$value, frame.3$value.1)
new.names<-c("PLOT", "ELEV", "ASP", "SPEC", "COV")
names(frame.3)<-new.names
frame.3<-frame.3[complete.cases(frame.3),]

frame.4<-data.frame(legacy.und[legacy.und$variable=="SPEC4",])
frame.4<-data.frame(frame.4, legacy.und[legacy.und$variable=="COV4",])
frame.4<-data.frame(frame.4$PLOT, frame.4$ELEV, frame.4$ASP, frame.4$value, frame.4$value.1)
new.names<-c("PLOT", "ELEV", "ASP", "SPEC", "COV")
names(frame.4)<-new.names
frame.4<-frame.4[complete.cases(frame.4),]

frame.5<-data.frame(legacy.und[legacy.und$variable=="SPEC5",])
frame.5<-data.frame(frame.5, legacy.und[legacy.und$variable=="COV5",])
frame.5<-data.frame(frame.5$PLOT, frame.5$ELEV, frame.5$ASP, frame.5$value, frame.5$value.1)
new.names<-c("PLOT", "ELEV", "ASP", "SPEC", "COV")
names(frame.5)<-new.names
frame.5<-frame.5[complete.cases(frame.5),]

frame.6<-data.frame(legacy.und[legacy.und$variable=="SPEC6",])
frame.6<-data.frame(frame.6, legacy.und[legacy.und$variable=="COV6",])
frame.6<-data.frame(frame.6$PLOT, frame.6$ELEV, frame.6$ASP, frame.6$value, frame.6$value.1)
new.names<-c("PLOT", "ELEV", "ASP", "SPEC", "COV")
names(frame.6)<-new.names
frame.6<-frame.6[complete.cases(frame.6),]

frame.7<-data.frame(legacy.und[legacy.und$variable=="SPEC7",])
frame.7<-data.frame(frame.7, legacy.und[legacy.und$variable=="COV7",])
frame.7<-data.frame(frame.7$PLOT, frame.7$ELEV, frame.7$ASP, frame.7$value, frame.7$value.1)
new.names<-c("PLOT", "ELEV", "ASP", "SPEC", "COV")
names(frame.7)<-new.names
frame.7<-frame.7[complete.cases(frame.7),]

frame.8<-data.frame(legacy.und[legacy.und$variable=="SPEC8",])
frame.8<-data.frame(frame.8, legacy.und[legacy.und$variable=="COV8",])
frame.8<-data.frame(frame.8$PLOT, frame.8$ELEV, frame.8$ASP, frame.8$value, frame.8$value.1)
new.names<-c("PLOT", "ELEV", "ASP", "SPEC", "COV")
names(frame.8)<-new.names
frame.8<-frame.8[complete.cases(frame.8),]

# Next, I will stack these data frames on top of one another 

legacy.und.full<-rbind(frame.1, frame.2, frame.3, frame.4, frame.5, frame.6, frame.7, frame.8)
dim(legacy.und.full)
head(legacy.und.full)

#Finally, write to csv!

getwd()
write.csv(legacy.und.full, file="Legacy_Understory_Reshaped.csv", row.names=F)



### MORE FIDDLY STUFF ####

# Adding a column of cover class to resurvey data and averaging at plot level. Here, "rep" indicates that there is replication of species' % covers at the plot level

resurvey.und.rep<-read.csv("/Users/rachelwilson/Dropbox/Cascades resurveys/NCCO/2015/Data_entry/Understory_2015.csv", na.strings = c("", " "))
resurvey.und.rep<-resurvey.und.rep[,1:5]
resurvey.und.rep[1:25,]
resurvey.und.rep$Cover.Class<-resurvey.und.rep$Percent.Cover
resurvey.und.rep$Cover.Class <-as.numeric(as.character(resurvey.und.rep$Cover.Class))

# Now, I will average % cover to the plot level
resurvey.und.rep<-resurvey.und.rep[complete.cases(resurvey.und.rep),]
resurvey.und<-aggregate(resurvey.und.rep["Cover.Class"], (resurvey.und.rep[c("Plot", "Species")]), mean)
#Use ACCI as an example species to make sure a couple plots were averaged correctly

# <5%
for(i in 3)resurvey.und[,i]<-ifelse(resurvey.und[,i]<=5, 1, resurvey.und[,i])
#5-25%
for(i in 3)resurvey.und[,i]<-ifelse(resurvey.und[,i]>=5 & resurvey.und[,i]<=25, 2, resurvey.und[,i])
#25-50%
for(i in 3)resurvey.und[,i]<-ifelse(resurvey.und[,i]>=25 & resurvey.und[,i]<=50, 3, resurvey.und[,i])
#50-75%
for(i in 3)resurvey.und[,i]<-ifelse(resurvey.und[,i]>=50 & resurvey.und[,i]<=75, 4, resurvey.und[,i])
#75-90%
for(i in 3)resurvey.und[,i]<-ifelse(resurvey.und[,i]>=75 & resurvey.und[,i]<=90, 5, resurvey.und[,i])
#95-100%
for(i in 3)resurvey.und[,i]<-ifelse(resurvey.und[,i]>=95, 6, resurvey.und[,i])
#Again, this is a good spot to use ACCI to compare between datasets

getwd()
write.csv(resurvey.und, file="2015_Understory_w_Cover_Class.csv", row.names=F)

# Adding elevations to 2015 understory data

resurvey.und<-read.csv("/Users/rachelwilson/Dropbox/Cascades resurveys/NCCO/2015/Data_entry/2015_Understory_w_Cover_Class.csv")
elevations<-read.csv("/Users/rachelwilson/Dropbox/Cascades resurveys/NCCO/2015/Data_entry/Lat.Long.csv") #Note: X2015.Plot.Name includes plots from 2014 and 2015
elevations.2015<-elevations[elevations$Resurvey.Year =="2015",]
elevations.2015<-data.frame(elevations.2015$X2015.Plot.Name, elevations.2015$X2015.Elevation.m)
names(elevations.2015)<-c("Plot", "Elevation.m")
resurvey.elev<-merge(resurvey.und, elevations.2015, by="Plot")
getwd()
write.csv(resurvey.elev, file="2015_w_Elevation.csv", row.names=F)

# Substituting 2015/2014 elevations for legacy data

elevations<-read.csv("/Users/rachelwilson/Dropbox/Cascades resurveys/NCCO/2015/Data_entry/Lat.Long.csv")
understory.all<-read.csv(file.choose()) # Understory_All in Data_entry
elevations.1980<-data.frame(elevations$X1980.Plot.Name, elevations$X2015.Elevation.m)
names(elevations.1980)<-c("Plot", "Elevation.New")
understory.1980<-understory.all[understory.all$Year=="1980",]
understory.new.elevations<-merge(understory.1980, elevations.1980, by="Plot")
getwd()
write.csv(understory.new.elevations, file="1980_w_New_Elevation.csv", row.names=F)

# Creating understory_all file with correct elevations and averaged plot values
und.2015<-read.csv("/Users/rachelwilson/Dropbox/Cascades resurveys/NCCO/2015/Data_entry/2015_w_Elevation.csv")
und.2014<-read.csv("/Users/rachelwilson/Dropbox/Cascades resurveys/NCCO/2015/Data_entry/2014_Understory.csv")
und.2014<-und.2014[und.2014$Year=="2014", c(2, 4, 8, 9)]
names(und.2014)<-c("Plot", "Elevation.m", "Species", "Cover.Class")
und.1980<-read.csv("/Users/rachelwilson/Dropbox/Cascades resurveys/NCCO/2015/Data_entry/1980_w_New_Elevation.csv")
und.1980<-und.1980[,c(3,4,5,7)]
names(und.1980)<-c("Plot", "Species", "Cover.Class", "Elevation.m")
und.1980$Plot<-factor(und.1980$Plot)
und.all<-rbind(und.2015, und.2014, und.1980)
#Adding years
und.all$Year<-c(rep("2015", nrow(und.2015)), rep("2014", nrow(und.2014)), rep("1980", nrow(und.1980)))
#Adding legacy vs. resurvey
und.all$Data.Type<-ifelse(und.all$Year=="1980", "Legacy", "Resurvey")
#### IMPORTANT NOTE, Jan. 7 2015: This data frame you have just created CONTAINS MISTAKES outlined in word document "List of suspicious things" as well as double % covers in plots that had supplementaries. Make sure you only ever write over the Understory_all file in dropbox if you have good reason to do so because this is the ONLY VERSION with up to date corrections. ###
# write.csv(und.all, file="Understory_All.csv", row.names=F)






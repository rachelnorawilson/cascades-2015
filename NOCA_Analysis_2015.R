################## THE ANALYSIS!!!! #########################

######## IMPORTANT NOTE: unless otherwise indicated, always use Understory_All.csv for these analyses as it is the ONLY file with up-to-date corrections. The original data (2015_data_Oct23.xml) was ONLY corrected once upon initial data entry for mistakes/to-do's on the hard copy data sheets as well as a few obvious typos.

##################################################################################

### Species cover ###

#### Always do this first #####
setwd("/Users/rachelwilson/Dropbox/Cascades resurveys/NCCO/2015/Data_entry")
und.cover<-read.csv("Understory_All.csv") # Note that L = Legacy and R = Resurvey

#select the species that you want to analyze
#Nov 6 list of species relevant to me: ACCI, ACMI, AMAL, ARUV, CAME, CHUM, CLUN, COCA, EPAN, GAOV, GASH, GOOB, HODI, LIBO, LUPE, MEFE, OPHO, PAMY, PHEM, POMU, RHAL, RULA, RUPA, RUPE, SARA, SOSI, SPBE, TABR, TITR, VAAL, VADE, VAME, VAPA?
#LINEAR: POMU
levels(und.cover$Species.Code)
und.cover.SPEC=subset(und.cover, Species.Code=="VAPA")
######################## JAN 13 WHERE I LEFT OFF ################################
#Trying to scale
und.cover$Cover.Class.Scaled<-

model.glm<-glm(Cover.Class~Elevation.m*Data.Type+I(Elevation.m^2), data= und.cover.SPEC)
summary(model.glm)

# Model selection
model.glm.linear<-glm(Cover.Class~Elevation.m*Data.Type, data= und.cover.SPEC)
summary(model.glm.linear)
model.glm.poly.w.int<-glm(Cover.Class~Elevation.m*Data.Type+I(Elevation.m^2)+I(Elevation.m^2):Data.Type, data=und.cover.SPEC)
summary(model.glm.poly.w.int)
library(MuMIn)
AICc(model.glm.linear)
AICc(model.glm.poly.w.int)
AICc(model.glm)

# Creating graphs of species' % cover
par(oma=c(0, 2.0, 0, 0))
plot(Cover.Class~Elevation.m, data= und.cover.SPEC, type="n", yaxt="n", xaxt="n", xlab="", ylab="", mgp=c(3,0.7,0))
axis(side=2, las=1, mgp=c(3,0.7,0), cex.axis=1.3, at=seq(1, 5, 1), labels=c("0-5", "5-25", "25-50", "50-75", "75-95"))
mtext(side=2, cex=1.5, line=4, "Cover Class (%)")
axis(side=1, cex.axis=1.4)
mtext(side=1, cex=1.5, "Elevation (m)", line=3)
points(Cover.Class~Elevation.m, data= und.cover.SPEC[und.cover.SPEC $Data.Type=="Legacy",], col="Blue", pch=2)
points(Cover.Class~Elevation.m, data= und.cover.SPEC[und.cover.SPEC $Data.Type=="Resurvey",], col="Red")
und.cover.SPEC.L<-und.cover.SPEC[und.cover.SPEC$Data.Type=="Legacy",]
lm.L<-lm(Cover.Class~I(Elevation.m^2)+Elevation.m, data=und.cover.SPEC.L)
x.L<-seq(from=min(und.cover.SPEC.L$Elevation.m, na.rm=TRUE), to=max(und.cover.SPEC.L$Elevation.m, na.rm=TRUE), by=0.01)
y.L<-predict(lm.L, list(Elevation.m=x.L, data=und.cover.SPEC.L))
lines(y.L~x.L, col="Blue")
und.cover.SPEC.R<-und.cover.SPEC[und.cover.SPEC$Data.Type=="Resurvey",]
lm.R<-lm(Cover.Class~I(Elevation.m^2)+Elevation.m, data=und.cover.SPEC.R)
x.R<-seq(from=min(und.cover.SPEC.R$Elevation.m, na.rm=TRUE), to=max(und.cover.SPEC.R$Elevation.m, na.rm=TRUE), by=0.01)
y.R<-predict(lm.R, list(Elevation.m=x.R, data=und.cover.SPEC.R))
lines(y.R~x.R, col="Red")
legend(400, 4.8, legend=c("1980", "2015"), col=c("Blue", "Red"), pch=c(17,16), cex=1.2)
#Linear line: sub this in instead lm.R<-lm(Cover.Class~Elevation.m, data=und.cover.SPEC.R)
#if you need to add text:
text(900, 4.5, expression(P[int] <" 0.05"), cex=1.4)



#Trying out GAMs - this might be more useful as a visualization tool 
library(mgcv)
mod.gam<-gam(Cover.Class~Data.Type + s(Elevation.m, by=Data.Type), data= und.cover.SPEC)
summary(mod.gam)
quartz()
plot(mod.gam, pages=1) #make new data frame - see notes from Matt

##################################################################################

### Lumping groups of species together for cover analysis - refer to word doccument "List of suspicious things" for rationale

#Pulling out an entire genus: I was really excited about this but I realized it creates a dataframe full of species you don't want; e.g. Vaccinium AND Valeriana
lumped.data<-subset(und.cover, grepl("VA", und.cover$Species.Code))
#Pulling out only desired species: probably the way to go
#ALL THE VACCINIUMS: DO NOT DELETE
lumped.data<-und.cover[und.cover$Species.Code=="VAAL" | und.cover$Species.Code=="VACA"| und.cover$Species.Code=="VADE"| und.cover$Species.Code=="VAME"| und.cover$Species.Code=="VAME?"| und.cover$Species.Code=="VAMExDE?"| und.cover$Species.Code=="VAMExPA"| und.cover$Species.Code=="VAOV"| und.cover$Species.Code=="VAOV?"| und.cover$Species.Code=="VAOVxAL"| und.cover$Species.Code=="VAPA"| und.cover$Species.Code=="VAPA?"| und.cover$Species.Code=="VAPAxME?"| und.cover$Species.Code=="VAPAXOV"| und.cover$Species.Code=="VASC"| und.cover$Species.Code=="VASC?"| und.cover$Species.Code=="VAUL"| und.cover$Species.Code=="VAXX",]

model.glm<-glm(Cover.Class~Elevation.m*Data.Type+I(Elevation.m^2), data= lumped.data)
summary(model.glm)

# Model selection
model.glm.linear<-glm(Cover.Class~Elevation.m*Data.Type, data= lumped.data)
summary(model.glm.linear)
model.glm.poly.w.int<-glm(Cover.Class~Elevation.m*Data.Type+I(Elevation.m^2)+I(Elevation.m^2):Data.Type, data= lumped.data)
summary(model.glm.poly.w.int)
library(MuMIn)
AICc(model.glm.linear)
AICc(model.glm.poly.w.int)
AICc(model.glm)

# Creating graphs of species' % cover
par(oma=c(0, 2.0, 0, 0))
plot(Cover.Class~Elevation.m, data= lumped.data, type="n", yaxt="n", xaxt="n", xlab="", ylab="", mgp=c(3,0.7,0))
axis(side=2, las=1, mgp=c(3,0.7,0), cex.axis=1.3, at=seq(1, 100, 20), labels=c("0-5", "5-25", "25-50", "50-75", "75-95"))
mtext(side=2, cex=1.5, line=4, "Cover Class (%)")
axis(side=1, cex.axis=1.4)
mtext(side=1, cex=1.5, "Elevation (m)", line=3)
points(Cover.Class~Elevation.m, data= lumped.data[lumped.data $Data.Type=="Legacy",], col="Blue", pch=2)
points(Cover.Class~Elevation.m, data= lumped.data[lumped.data $Data.Type=="Resurvey",], col="Red")
lumped.data.L<-lumped.data[lumped.data $Data.Type=="Legacy",]
lm.L<-lm(Cover.Class~I(Elevation.m^2)+Elevation.m, data= lumped.data.L)
x.L<-seq(from=min(lumped.data.L$Elevation.m, na.rm=TRUE), to=max(lumped.data.L$Elevation.m, na.rm=TRUE), by=0.01)
y.L<-predict(lm.L, list(Elevation.m=x.L, data= lumped.data.L))
lines(y.L~x.L, col="Blue")
lumped.data.R<-lumped.data[lumped.data $Data.Type=="Resurvey",]
lm.R<-lm(Cover.Class~Elevation.m, data= lumped.data.R)
x.R<-seq(from=min(lumped.data.R$Elevation.m, na.rm=TRUE), to=max(lumped.data.R$Elevation.m, na.rm=TRUE), by=0.01)
y.R<-predict(lm.R, list(Elevation.m=x.R, data= lumped.data.R))
lines(y.R~x.R, col="Red")
legend(400, 4.8, legend=c("1980", "2015"), col=c("Blue", "Red"), pch=c(17,16), cex=1.2)
#Linear line: sub this in instead lm.R<-lm(Cover.Class~Elevation.m, data=und.cover.SPEC.R)
#if you need to add text:
# text(900, 4.5, expression(P[int] <" 0.05"), cex=1.4)

library(mgcv)
mod.gam<-gam(Cover.Class~Data.Type + s(Elevation.m, by=Data.Type), data= lumped.data)
summary(mod.gam)
quartz()
plot(mod.gam, pages=1) #make new data frame - see notes from Matt






##################################################################################



#### Creating a histogram of frequency of elevational differences for min and max elevation. There may be a faster way to do this but ¯\_(ツ)_/¯
und.cover.L<-und.cover[und.cover$Data.Type=="L",]
und.cover.R<-und.cover[und.cover$Data.Type=="R",]

### LEGACY MAX!
und.cover.L<-und.cover.L[complete.cases(und.cover.L),]
small.L<-data.frame(und.cover.L$Species.Code, und.cover.L$Elevation.m)
small.L$und.cover.L.Species.Code<-factor(small.L$und.cover.L.Species.Code)
Species.Code.L<-levels(small.L$und.cover.L.Species.Code)
max.elev.L<-vector()
for(i in 1:219){
	max.elev.L[i]<-max(c(und.cover.L$Elevation.m[und.cover.L$Species.Code==Species.Code.L[i]], na.rm=TRUE))
	}
max.elev.L
max.elev.L<-data.frame(max.elev.L, Species.Code.L)
names(max.elev.L)<-c("Elevation.m.L", "Species.Code")
max(und.cover.L$Elevation.m[und.cover.L$Species.Code=="VAME"], na.rm=TRUE) #double check with whatever species

### RESURVEY MAX!!!
und.cover.R<-und.cover.R[complete.cases(und.cover.R),]
small.R<-data.frame(und.cover.R$Species.Code, und.cover.R$Elevation.m)
small.R$und.cover.R.Species.Code<-factor(small.R$und.cover.R.Species.Code)
Species.Code.R<-levels(small.R$und.cover.R.Species.Code)
max.elev.R<-vector()
for(i in 1:559){
	max.elev.R[i]<-max(c(und.cover.R$Elevation.m[und.cover.R$Species.Code==Species.Code.R[i]], na.rm=TRUE))
	}
max.elev.R
max.elev.R<-data.frame(max.elev.R, Species.Code.R)
names(max.elev.R)<-c("Elevation.m.R", "Species.Code")
max(und.cover.L$Elevation.m[und.cover.L$Species.Code=="VAME"], na.rm=TRUE) #double check with whatever species

### COMBINING EM AND TAKING THE DIFFERENCE!
max.elev<-merge(max.elev.L, max.elev.R, by="Species.Code")
max.elev$Elevation.dif<-max.elev$Elevation.m.R-max.elev$Elevation.m.L

### LEGACY MIN! (Note - have to have created und.cover.L etc. as above)
min.elev.L<-vector()
for(i in 1:219){
	min.elev.L[i]<-min(und.cover.L$Elevation.m[und.cover.L$Species.Code==Species.Code.L[i]], na.rm=TRUE)
	}
min.elev.L
min.elev.L<-data.frame(min.elev.L, Species.Code.L)
names(min.elev.L)<-c("Elevation.m.L", "Species.Code")
min(und.cover.L$Elevation.m[und.cover.L$Species.Code=="RHAL"], na.rm=TRUE) #double check with whatever species

### RESURVEY MAX!!!
min.elev.R<-vector()
for(i in 1:559){
	min.elev.R[i]<-min(und.cover.R$Elevation.m[und.cover.R$Species.Code==Species.Code.R[i]], na.rm=TRUE)
	}
min.elev.R
min.elev.R<-data.frame(min.elev.R, Species.Code.R)
names(min.elev.R)<-c("Elevation.m.R", "Species.Code")
min(und.cover.R$Elevation.m[und.cover.R$Species.Code=="VIXX"], na.rm=TRUE) #double check with whatever species

### COMBINING EM AND TAKING THE DIFFERENCE!
min.elev<-merge(min.elev.L, min.elev.R, by="Species.Code")
min.elev$Elevation.dif<-min.elev$Elevation.m.L-min.elev$Elevation.m.R

### GRAPHS!!!!!!!! :O
hist(min.elev$Elevation.dif, breaks=15, xlab="Change in lower range limit (m)", xlim=c(-1600, 1600), ylim=c(0,50), main="")
abline(v=mean(min.elev$Elevation.dif), col="Red")

hist(max.elev$Elevation.dif, breaks=15, xlab="Change in upper range limit (m)", xlim=c(-1500, 1500), ylim=c(0,40), main="")
abline(v=mean(max.elev$Elevation.dif), col="Red")

mean(min.elev$Elevation.dif)
t.test(min.elev$Elevation.dif, y=NULL)

mean(max.elev$Elevation.dif)
t.test(max.elev$Elevation.dif, y=NULL)

## Taking subset: to come
















##################################################################################


### Species presence/absence ####

### Next order of business: figure out why some of these values are turning into 2s and 3s
####### I just realized it's because I didn't aggregate by transect. Next on my to do list

und.cover<-read.csv("Understory_All.csv") # Note that L = Legacy and R = Resurvey

# Creating a new data frame from Understory_All of 1s/0s presence/absence

pres.abs<-table(und.cover$Plot, und.cover$Species.Code)
library(reshape2)
und.presence.small<-melt(pres.abs, id.vars=c("Plot", "Species.Code"))
names(und.presence.small)<-c("Plot", "Species.Code", "Pres.Abs")

und.cover.small<-aggregate(und.cover[c("Year","Elevation.m")],und.cover[c("Plot")],mean)
und.cover.small$Data.Type<-ifelse(und.cover.small$Year==1980,"L","R")

und.presence<-merge(und.presence.small, und.cover.small, by="Plot")
und.presence$Data.Type<-as.factor(und.presence$Data.Type)
# I just realized that I forgot to average to the plot level so I'm going to do a quick fix for this here and make all values >1 equal to 1
und.presence$Pres.Abs<-ifelse(und.presence$Pres.Abs>=1, 1, 0)

levels(und.presence$Species.Code)
und.presence.SPEC=subset(und.presence, Species.Code=="ARUV")
und.presence.SPEC$Elevation.m2<-(und.presence.SPEC$Elevation)^2

SPEC.lm<-glm(Pres.Abs~Elevation.m*Data.Type, data=und.presence.SPEC, family="binomial")
summary(SPEC.lm)

#Trying out GAMs
library(gam)
library(mgcv)
mod.gam<-gam(Pres.Abs~Elevation.m*Data.Type, data= und.presence.SPEC, family=binomiaL)
summary(mod.gam)
gam.null<-gam(Pres.Abs~1, data=und.presence.SPEC, family=binomial)
summary(gam.null)

par(oma=c(2, 3, 0, 0))
plot(Pres.Abs~Elevation.m, data= und.presence.SPEC, type="n", xaxt="n", yaxt="n", ylab="", xlab="")
axis(side=2, las=1, at=seq(0,1,1), labels=c("Absence", "Presence"), cex.axis=1.5)
axis(side=1, cex.axis=1.2)
mtext(side=1, line=3, "Elevation (m)", cex=1.4)
points(Pres.Abs~Elevation.m, data= und.presence.SPEC[und.presence.SPEC $Data.Type=="L",], col="Blue", pch=2)
points(Pres.Abs~Elevation.m, data= und.presence.SPEC[und.presence.SPEC $Data.Type=="R",], col="Red", pch=1)
legend(300, 0.6, legend=c("1980", "2015"), col=c("Blue", "Red"), pch=c(17,16), cex=1.2)

# und.presence.SPEC.L<-und.presence.SPEC[und.presence.SPEC$Data.Type=="L",]
# lm.L<-glm(Pres.Abs~Elevation.m, data=und.presence.SPEC.L, family=binomial(link="logit"))
# x.L<-seq(from=min(und.presence.SPEC.L$Elevation.m, na.rm=TRUE), to=max(und.presence.SPEC.L$Elevation.m, na.rm=TRUE), by=0.01)
# y.L<-predict(lm.L, list(Elevation.m=x.L, data=und.presence.SPEC.L), type="response")
# lines(y.L~x.L, col="Blue")
# und.presence.SPEC.R<-und.presence.SPEC[und.presence.SPEC$Data.Type=="R",]
# lm.R<-glm(Pres.Abs~Elevation.m, data=und.presence.SPEC.R, family=binomial)
# x.R<-seq(from=min(und.presence.SPEC.R$Elevation.m, na.rm=TRUE), to=max(und.presence.SPEC.R$Elevation.m, na.rm=TRUE), by=0.01)
# y.R<-predict(lm.R, list(Elevation.m=x.R, data=und.presence.SPEC.R), type="response")
# lines(y.R~x.R, col="Red")







#######################################################################

#species richness
Rich<-read.csv("richness_Nov7.csv")

model_lm<-glm(Richness~Elevation_m*Year, data=Rich)
summary(model_lm)

#plot richness by elevation
scatterplot(Richness~Elevation_m | Year, data=Rich, smoother=F, xlab="Year", ylab="Species (n)", 
            main="Species Richness", cex.lab=1.5, x.leg=1.2, cex.axis=1.3, las=1, labels=row.names(Rich))






#######################################################################

#lennon dissimilarity analysis. I calculated the values in Excel
Lennon<-read.csv("Lennon1.csv")

model_lm<-lm(Lennon~Elevation_m, data=Lennon)
summary(model_lm)

#plot Lennon value by elevation
scatterplot(Lennon~Elevation_m, data=Lennon, smoother=F, xlab="Elevation (m)", ylab="Lennon dissimilarity value", 
            main="Lennon dissimilarity", cex.lab=1.5, x.leg=1.2, cex.axis=1.3, las=1, ylim=c(0.5,1), boxplots=F, col="Black", grid=F,
            labels=row.names(Lennon))


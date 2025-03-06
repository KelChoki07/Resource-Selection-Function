
 #       _                                            _____     _____   ______ 
 #      | |                                          |  __ \   / ____| |  ____|
 #      | |   __ _    __ _   _   _    __ _   _ __    | |__) | | (___   | |__   
 #  _   | |  / _` |  / _` | | | | |  / _` | | '__|   |  _  /   \___ \  |  __|  
 # | |__| | | (_| | | (_| | | |_| | | (_| | | |      | | \ \   ____) | | |     
 #  \____/   \__,_|  \__, |  \__,_|  \__,_| |_|      |_|  \_\ |_____/  |_|     
 #                    __/ |                                                    
 #                   |___/                                                     


# Resource selection function using gps collar data
# Home range estimation using adehabitatHR packages
# Probability of selection and mapping

setwd("D:\\Fall 2023\\Movement Ecology WL 792\\Project\\Jaguar_Movement_RSF_2023")
getwd()

# Load packages
library("raster"); library("sf"); library("ggplot2"); library("visreg"); 
library("viridis"); library("adehabitatHR"); library("mapview"); library("sjPlot");
library("tidyverse")


###################################Getting movebank-data##########################
loginStored <- movebankLogin(username="XXXX", password="XXXX") #you need to add your username and password and have "move" package
wholestudy<-getMovebankData(study="Movement ecology of the jaguar in the largest floodplain of the world, the Brazilian Pantanal",
                            login=loginStored, removeDuplicatedTimestamps=TRUE)#download everything
wholestudy.df <- as(wholestudy, "data.frame") #change the movestack to a dataframe for easier manipulation
str(wholestudy.df)
head(wholestudy.df)

# write.csv(wholestudy.df, "wholestudy.csv")
wholestudy2.df <- with(wholestudy.df, data.frame(id = trackId, lon = location_long, 
                lat = location_lat, time = timestamp)) #if you have elevation add elevation = height_above_ellipsoid
ggplot(wholestudy2.df, aes(x = lon, y = lat, col = id))+
       xlab("Longitude")+ ylab("Latitude") + labs(col="Individual Jaguar")+ geom_path()


#____________Adding UTM and removing NAs in the dataset
#First add UTM to our dataframe
ll <- with(wholestudy.df, data.frame(X = location_long, Y = location_lat)) #is LL lat long?yup
attributes(ll)$projection <- "LL"
xy.km <- convUL(ll, km=TRUE) #km = true when you want km #Convert coordinates between UTM and Lon/Lat #PBSmapping package
wholestudyUTM <- data.frame(wholestudy.df, x=xy.km$X*1000, y=xy.km$Y*1000)
head(wholestudyUTM)

#Next we'll remove any NAs
wholestudyUTM <- wholestudyUTM[!is.na(wholestudyUTM$x) & !is.na(wholestudyUTM$y),] #removing NAs
head(wholestudyUTM)
unique(wholestudyUTM$trackId) #can check which individuals are still in the dataset
wholestudyUTM$tag_id[]  

write.csv(wholestudyUTM, "wholestudyUTM.csv")



################################################################################
# Load movement data
jagr <- read.csv("wholestudyUTM.csv")
str(jagr)
head(jagr)

#Check the min and max period of collaring for each individual #tidyverse package
Indv<-group_by(jagr,trackId)
head(Indv)

#groub_by(.data=df, ...=group_by
summary.tab<-summarise(Indv,
                       max.date=max(timestamp),
                       min.date=min(timestamp),
                       Tag_id =unique(tag_id),
                       SEX= unique(sex),
                       
)
summary.tab 

# Convert coordinates into SpatialPoints object #library(sp)
datapointsjagr<- SpatialPoints(cbind(jagr$x,jagr$y))
plot(datapointsjagr, axes=T)

#check crs and assign
sf::st_crs(datapointsjagr)$proj4string
proj4string(datapointsjagr) <- CRS( "+proj=utm +zone=21 +south +ellps=aust_SA +towgs84=-67.35,3.88,-38.22,0,0,0,0 +units=m +no_defs" ) #add projection information
crs(datapointsjagr)


# Load raster layers
# Better to have rasters clipped to study area with same extent and resolution 
elevation <- raster("Pantanal_DEM_final_mcp.tif")
wetland <- raster("Pantanal_wetland_final_mcp.tif")
distance_river <- raster("Pantanal_Dist_hydro_mcp.tif")
distance_road <- raster("Pantanal_Dist_road_mcp.tif")
human_footprint <- raster("Pantanal_humanfootprint_mcp.tif")

#check the crs
sf::st_crs(elevation)$proj4string
sf::st_crs(wetland)$proj4string
sf::st_crs(distance_river)$proj4string
sf::st_crs(distance_road)$proj4string
sf::st_crs(human_footprint)$proj4string

#check the extension #raster package
extent(elevation)
extent(wetland)
extent(distance_river)
extent(distance_road)
extent(human_footprint)

#check cell size
res(elevation)
res(wetland)
res(distance_river)
res(distance_road)
res(human_footprint)

# Make all rasters same size and extent
elevation.matched <- resample(elevation, distance_river) # , method="bilinear"
wetland.matched <- resample(wetland, distance_river)
# dist_road.matched <- resample(dist_road, distance_river)
# human_ft.matched <- resample(human_footprint, distance_river)



# Creating a raster brick (all raster layers should have the same extent and resolution)
RSF.brick <- brick(elevation.matched, wetland.matched, distance_road, distance_river, human_footprint)
str(RSF.brick)

nlayers(RSF.brick)
names(RSF.brick) <- c("Elevation", "Wetland", "Distance_Roads", "Distance_Rivers", "Human_Footprint")

plot(RSF.brick)

plot(RSF.brick[[2]]) #second layer -wetland
# plot(RSF.brick[[2]], ylim= c(range(jagr$y)[1]+5000,range(jagr$y)[2]+6000), xlim=c(range(jagr$x)[1]+40000,range(jagr$x)[2]+30000))

plot(datapointsjagr, col="dodgerblue", pch=19, cex=0.5, add=TRUE)

###############################################################################

# Compute mcp
jagr.mcp <- mcp(datapointsjagr, 100) #percent=100-uses all points (can use 95% too)
# jagr.mcp1 <- mcp(datapointsjagr,percent=100, unout="km2") #to check HR in km2
lines(jagr.mcp, col="red", lwd=2)
str(jagr.mcp)

#creator a vector of mcp and save the shape file to clip other raster file to it
V1 <- terra::vect(jagr.mcp)
plot(V1, axes= F)
writeVector(V1, "jagr.mcp.shp")


# Random points within MCP 
#we use GPS location as response
#we are creating a binary response, 0 for un-used and 1 for used,  are drawing absence location
#here we are drawing sample of used site from observed points and
xy.obs <- jagr[sample(1:nrow(jagr), 5000), c("x", "y")] # select 5000 random points from observation data
str(xy.obs)
head(xy.obs)
ncol(xy.obs)

xy.random <- spsample(jagr.mcp, 5000, "random")@coords # create 5000 random points but from within the mcp
str(xy.random)
head(xy.random)

# Plot to check the random points and observation points
plot(xy.random, asp=1, col="darkblue", pch=19, cex=0.5)
points(xy.obs, pch=19, col="orange", cex=0.5) # observation points #actual used
legend("topleft", legend=c("Used", "Available"),
       col=c("orange","darkblue"), pch = 19, cex=0.9)

# Stack used (observed) and available points (unused) and label them
data.rsf <- data.frame(X=c(xy.obs[, 1], xy.random[, 1]), Y=c(xy.obs[, 2], xy.random[, 2]),
                       Used=c(rep(TRUE, nrow(xy.obs)), rep(FALSE, nrow(xy.random))))

head(data.rsf)
str(data.rsf)

# Extract variables for used and available points from each raster
data.rsf$elevation <- extract(RSF.brick[[1]], data.rsf[, 1:2]) #1:2, X:Y coordinates
data.rsf$wetland <- extract(RSF.brick[[2]], data.rsf[, 1:2])
data.rsf$dist_road <- extract(RSF.brick[[3]], data.rsf[, 1:2])
data.rsf$dist_river <- extract(RSF.brick[[4]], data.rsf[, 1:2])
data.rsf$human_footprint <- extract(RSF.brick[[5]], data.rsf[, 1:2])
head(data.rsf)

#Check if there is NA 
any(is.na(data.rsf))
which(is.na(data.rsf))
data.rsf[is.na(data.rsf)] <- 0
any(is.na(data.rsf))

#Checking the distribution of variable
str(data.rsf)
hist(data.rsf$elevation)
hist(data.rsf$wetland)
hist(data.rsf$dist_road)
hist(data.rsf$dist_river)
hist(data.rsf$human_footprint)


# Create a copy
data.rsf2 <- data.rsf

# scale() changes the class of your columns
# Simply them to numeric vectors
# Use the dimension-stripping properties of c()
data.rsf2$elevation <- c(scale(data.rsf2$elevation))
class(data.rsf2$elevation)
data.rsf2$wetland <- c(scale(data.rsf2$wetland)) #wetland is categorical, and we have scaled it? shit, Oh okay it still is categorical
class(data.rsf2$wetland)
data.rsf2$dist_road <- c(scale(data.rsf2$dist_road))
data.rsf2$dist_river <- c(scale(data.rsf2$dist_river))
data.rsf2$human_footprint <- c(scale(data.rsf2$human_footprint))

head(data.rsf2)
str(data.rsf2)

rsf.fit3 <- glm(Used ~ elevation + wetland + dist_road + dist_river + human_footprint, data=data.rsf2, family="binomial")
rsf.fit4 <- glm(Used ~ elevation + wetland + dist_river + human_footprint, data=data.rsf2, family="binomial")
rsf.fit5 <- glm(Used ~ elevation + wetland + human_footprint, data=data.rsf2, family="binomial")
rsf.fit6 <- glm(Used ~ elevation + wetland + dist_road + human_footprint, data=data.rsf2, family="binomial")
rsf.fit7 <- glm(Used ~ wetland + dist_river + human_footprint, data=data.rsf2, family="binomial")
rsf.fit8 <- glm(Used ~ dist_road+dist_river + human_footprint, data=data.rsf2, family="binomial")
rsf.fit9 <- glm(Used ~ elevation +I(elevation^2) + wetland + dist_road+ dist_river
                 + human_footprint+ I(human_footprint^2), data=data.rsf2, family="binomial")

#we use quadratic because relation are not always linear
library(AICcmodavg)
Cand.mods <- list("model1" = rsf.fit3, "model2" = rsf.fit4, "model3"= rsf.fit5, "model4"=rsf.fit6, "model5"=rsf.fit7, "model6"=rsf.fit8, "model7"=rsf.fit9)  
model_list <- aictab(cand.set = Cand.mods, second.ord = TRUE)
model_list

#Plot the top model - whisker plot
library(sjPlot) #its ratio of event/ not event log(p/1-p)
plot_model(rsf.fit9, type="est", show.intercept=F, show.p=T, show.values=T, vline.color="grey20") # plots odds ratio
plot_model(rsf.fit9, type="eff", grid=T) # produces relationship plots
summary(rsf.fit9) #top model


# Make some plots
par(mfrow=c(2,3))
visreg(fit=rsf.fit9, xvar="elevation", scale="response", 
       xlab="Elevation (standardised)", ylab="Prob of use", ylim=c(0, 1))
       
visreg(fit=rsf.fit9, xvar="wetland", scale="response", 
       xlab="Wetland (standardised)", ylab="Prob of use", ylim=c(0, 1)) 

visreg(fit=rsf.fit9, xvar="dist_river", scale="response", 
       xlab="Distance to river (standardised)", ylab="Prob of use", ylim=c(0, 1))

visreg(fit=rsf.fit9, xvar="dist_road", scale="response", 
       xlab="Distance to road (standardised)", ylab="Prob of use", ylim=c(0, 1)) 

visreg(fit=rsf.fit9, xvar="human_footprint", scale="response", 
       xlab="Human Footprint (standardised)", ylab="Prob of use", ylim=c(0, 1))

par(mfrow = c(1, 1))

# No y limits
png("Pantanal_Jaguar_RSF_covariate_relationship.png", units="cm", res=400, height=15, width=17)
par(mfrow=c(2,3))
visreg(fit=rsf.fit9, xvar="elevation", scale="response", 
       xlab="Elevation (standardised)", ylab="Prob of use")
       
visreg(fit=rsf.fit9, xvar="wetland", scale="response", 
       xlab="Wetland (standardised)", ylab="Prob of use") 

visreg(fit=rsf.fit9, xvar="dist_river", scale="response", 
       xlab="Distance to river (standardised)", ylab="Prob of use")

visreg(fit=rsf.fit9, xvar="dist_road", scale="response", 
       xlab="Distance to road (standardised)", ylab="Prob of use") 

visreg(fit=rsf.fit9, xvar="human_footprint", scale="response", 
       xlab="Human Footprint (standardised)", ylab="Prob of use")
par(mfrow = c(1, 1))
dev.off()


#############################################################################################

# RSF prediction map
#raster minus data from covariates
elevation.s <- (elevation.matched-mean(data.rsf$elevation))/sd(data.rsf$elevation)
wetland.s <- (wetland.matched-mean(data.rsf$wetland))/sd(data.rsf$wetland)
dist_road.s <- (distance_road-mean(data.rsf$dist_road))/sd(data.rsf$dist_road)
dist_river.s <- (distance_river-mean(data.rsf$dist_river))/sd(data.rsf$dist_river)
human_footprint.s <- (human_footprint-mean(data.rsf$human_footprint, na.rm=T))/sd(data.rsf$human_footprint, na.rm=T)

plot(elevation.s)
plot(wetland.s)
plot(dist_road.s)
plot(dist_river.s)
plot(human_footprint.s)

rsf.fit9 <- glm(Used ~ elevation +I(elevation^2) + wetland + dist_road+ dist_river
                + human_footprint+ I(human_footprint^2), data=data.rsf2, family="binomial")
summary(rsf.fit9)

beta.glm <- coef(rsf.fit9)

# Linear model for prediction
logit.glm <- beta.glm[1] + (beta.glm[2]*elevation.s) + (beta.glm[3]*elevation.s^2) + 
  (beta.glm[4]*wetland.s) + (beta.glm[5]*dist_road.s) + (beta.glm[6]*dist_river.s) + 
  (beta.glm[7]*human_footprint.s) +(beta.glm[7]*human_footprint.s^2)

class(logit.glm)

# Backtransform (convert logit scale to probability scale)
rsf.prob <- exp(logit.glm)/(1+exp(logit.glm))

png("Pantanal_Jaguar_RSF.png", units="cm", res=400, height=18, width=18)

plot(rsf.prob, main="Pantanal Jaguar RSF", col=turbo(100), 
     legend.args=list(text='Selection probability', side=2, font=2, line=0.3, cex=1.25),
     xlab="Eastings", ylab="Northings")
dev.off()

library(mapview)
# Overlay on the map
mapview(rsf.prob, layer.name="Selection probability", 
        na.color="transparent", lwd=5, at=seq(0,1,0.2))

?mapview

################################ END OF SCRIPT:) ################################

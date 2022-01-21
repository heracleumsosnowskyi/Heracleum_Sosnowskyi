setwd(getwd())
par(mar = rep(2, 4))
## load the required packages
library(biomod2)
library(ggplot2)
library(gridExtra)
library(raster)
library(rasterVis)
library(maptools)
library(terra)
library(SDMtune)
library(leaflet)
library(maps)
library(spThin)
library(rgdal)
library(zeallot)
## read data ----

HS_occ <- read.csv('./input_data/dataset4.csv')
total <- HS_occ
summary(HS_occ)

HS_clean <- total[!duplicated(total),]
HS_occ_ready <- total[!is.na(total$lon),]
HS_occ_ready

summary(HS_occ_ready)

plot(HS_occ_ready$lon, HS_occ_ready$lat)

HS_occ_ready <- HS_occ_ready[HS_occ_ready$long < 73,]
HS_occ_ready <- HS_occ_ready[HS_occ_ready$long > 18,]


# visualization occurrence points on the map:
long <- HS_occ_ready$long
lat <- HS_occ_ready$lat
coords <- data.frame(long, lat)
library(dplyr)

ID <- 1:10

pal <- colorNumeric(palette = "Blues", domain = quakes$mag)

leaflet(data=coords) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addCircleMarkers(~long, ~lat,
                   #label=as.character(ID), 
                   radius = 1,
                   color = "navy",
                   fillColor = 'blue',
                   stroke = TRUE, fillOpacity = 0.3, weight = 1)



## load environmental data
bioclim <- 
  raster::stack(
    c(
      bio_1  = './input_data/_bio_1_i.asc',
      bio_2  = './input_data/_bio_1_i.asc'
    )
  )



soil <- 
  raster::stack(
    './input_data/_soc_5-15.asc',
    './input_data/_cec_5-15.asc'
  )


soil <- soil/10

#change resolution
a <- resample(soil,bioclim)
res(a)

#create stack
env <- stack(a,bioclim)
denpasar  <- readOGR('./input_data/RUS_adm0.shp')
masked <- mask(x = env, mask = denpasar)
cropped_env <- crop(x = masked, y = extent(denpasar))
plot(cropped_env)

cropped_env <- stack(cropped_env)
names(cropped_env)

##RESample10 min using old data
bioclim_old <- 
  raster::stack(
    c(
      bio_1  = './input_data/_bio_1_i.asc',
      bio_2  = './input_data/_bio_2_i.asc'
    )
  )

cropped_env <- resample(cropped_env, bioclim_old)
cropped_env <- stack(cropped_env)


#visualization
ggplot(data = world) + 
  geom_sf() +
  geom_jitter(data = thin_full , aes(x = long1, y = lat1), color = "blue",
              alpha = 0.4, size = 0.1) +
  labs(x = "longitude", y = "latitude") +
  coord_sf(xlim = c(19, 72), ylim = c(40, 68), expand = FALSE)


leaflet(data=coords1) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addCircleMarkers(~long1, ~lat1,
                   #label=as.character(ID), 
                   radius = 1,
                   color = "navy",
                   fillColor = 'blue',
                   stroke = TRUE, fillOpacity = 0.3, weight = 1)



library(tidyverse)
HS_occ_ready <- HS_occ_ready %>% 
  rename(
    long = clim_long1,
    lat = clim_lat1
  )

##generation of first part of  PA
library(biomod2)
HS_data <- 
  BIOMOD_FormatingData(
    resp.var = HS_occ_ready['Heracleum.sosnowskyi'],
    resp.xy = HS_occ_ready[, c('long', 'lat')],
    expl.var = cropped_env,
    resp.name = "Heracleum.sosnowskyi",
    PA.nb.rep = 1,
    PA.nb.absences = 1000,
    PA.dist.max = 25000,
    PA.strategy = 'disk'
  )

plot(HS_data)


#Export of coords from first PA
library(dplyr)
## function to get PA dataset
get_PAtab <- function(bfd){
  dplyr::bind_cols(
    x = bfd@coord[, 1],
    y = bfd@coord[, 2],
    status = bfd@data.species,
    bfd@PA
  )
}

## function to get background mask
get_mask <- function(bfd){
  bfd@data.mask
}

(pa.all.xy <- get_PAtab(HS_data) %>% 
    filter(is.na(status)) %>%
    select(x, y)) %>%
  distinct()

pa.all.xy

(pres.xy <- get_PAtab(HS_data) %>% 
    filter(status == 1) %>%
    select(x, y)) %>%
  distinct()

pres.xy
plot(HS_data)

##Second PA tundra
HS_data2 <- 
  BIOMOD_FormatingData(
    resp.var = HS_occ_ready['Heracleum.sosnowskyi'],
    resp.xy = HS_occ_ready[, c('long', 'lat')],
    expl.var = cropped_env,
    resp.name = "Heracleum.sosnowskyi",
    PA.nb.rep = 1,
    PA.nb.absences = 3000,
    PA.strategy = 'random'
  )

plot(HS_data2)


(pa.all_2.xy <- get_PAtab(HS_data2) %>% 
    filter(is.na(status)) %>%
    select(x, y)) %>%
  distinct()

pa.all_2.xy

pa2 <- pa.all_2.xy[pa.all_2.xy$x >40 & pa.all_2.xy$y > 64,]
pa_data_all = rbind(pa2,pa.all.xy)


library(tmap)
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) + 
  geom_sf() +
  geom_jitter(data = pres.xy, aes(x = x, y = y), color = "blue",
              alpha = 0.4, size = 0.1) +
  labs(x = "longitude", y = "latitude") +
  coord_sf(xlim = c(19, 72), ylim = c(40, 68), expand = FALSE)

ggplot(map_data("world"), aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "grey95", color = "gray40", size = 0.2) +
  geom_jitter(data = pres.xy, aes(x = x, y = y), color = "blue",
              alpha = 0.4, size = 1) +
  labs(x = "longitude", y = "latitude") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_fixed() +
  scale_x_continuous(limits = c(20, 70)) +
  scale_y_continuous(limits = c(40, 70))

ggplot(data = world) + 
  geom_sf() +
  geom_jitter(data = pa_data_all, aes(x = x, y = y), color = "red",
              alpha = 0.4, size = 0.1) +
  labs(x = "longitude", y = "latitude") +
  coord_sf(xlim = c(19, 72), ylim = c(40, 68), expand = FALSE)

ggplot(map_data("world"), aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "grey95", color = "gray40", size = 0.2) +
  geom_jitter(data = pa_data_all, aes(x = x, y = y), color = "red",
              alpha = 0.4, size = 0.1) +
  labs(x = "longitude", y = "latitude") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_fixed() +
  scale_x_continuous(limits = c(20, 70)) +
  scale_y_continuous(limits = c(40, 70))

##Spatial cross validation
library(blockCV)
library(raster)
library(sf)

pa_data_all$Species <- 0
pres.xy$Species <- 1

pa<- rbind(pa_data_all, pres.xy,by=c('x', 'y'))
pa<- na.omit(pa)

# import raster data
awt <- cropped_env
awt
crs(awt) <- '+proj=longlat +datum=WGS84'

pa <- head(pa,-3)
tail(pa)

pa <- pa[, c( 3,1,2)]
# make a SpatialPointsDataFrame object from data.frame
pa_data <- st_as_sf(pa, coords = c("x", "y"), crs = crs(awt))
# see the first few rows
pa_data

plot(awt$bio_1)

par(mfrow = c(2,3))
plot(awt[[3]], labs(x = "longitude", y = "latitude"))# plot raster data
plot(pa_data[which(pa_data$Species==1), ], pch = 1, col="red", add=TRUE) # add presence points
plot(pa_data[which(pa_data$Species==0), ], pch = 1, col="blue", add=TRUE) # add absence points
legend(x=500000, y=8250000, legend=c("Presence","Absence"), col=c(2, 4), pch=c(2,2), bty="n")



set.seed(1)
sb <- spatialBlock(speciesData = pa_data,
                   species = "Species",
                   rasterLayer = awt,
                   theRange = 100000, # size of the blocks
                   k = 13,
                   selection = "random",
                   iteration = 20, # find evenly dispersed folds
                   biomod2Format = TRUE,
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)


foldExplorer(blocks = sb, 
             rasterLayer = awt, 
             speciesData = pa_data)


# loading the libraries
library(randomForest)
library(precrec)

# extract the raster values for the species points as a dataframe
mydata <- raster::extract(awt, pa_data, df = TRUE)
# adding species column to the dataframe
mydata$Species <- as.factor(pa_data$Species)
# remove extra column (ID)
mydata <- mydata[,-1]

# extract the foldIDs in SpatialBlock object 
# created in the previous section
# the folds (list) works for all three blocking strategies
folds <- sb$folds

# create a data.frame to store the prediction of each fold (record)
testTable <- pa_data
testTable$pred <- NA

set.seed(1)
cross <- for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(Species~., mydata[trainSet, ], ntree = 900, importance=TRUE) # model fitting on training set
  testTable$pred[testSet] <- predict(rf, mydata[testSet, ], type = "prob")[,2] # predict the test set
}



# calculate Area Under the ROC and PR curves and plot the result
precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Species)


autoplot(precrec_obj)
autoplot(sspoints)
#saveRDS(rf, "./final_final_model.rds")
par(mfrow = c(2,4))

varImpPlot(rf, scale=TRUE)
importance(rf)

saveRDS(rf, "./models/model_rf.rds")
my_model <- readRDS("./models/model_rf.rds")


##Make map prediction
library(mecofun)
bio_curr_df <- data.frame(rasterToPoints(awt))
bio_curr_df$pred_rf <- mecofun::predictSDM(rf, bio_curr_df)

r_pred_curr <- rasterFromXYZ(bio_curr_df[,-c(3:10)])
plot(r_pred_curr)

library(sna)


map_c <- gplot(r_pred_curr) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                       na.value = "transparent",
                       name = "Probability") +
  labs(title = "The projection of habitat suitability 
       for H.S. in 2000-2018",
       x = "longitude",
       y = "latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

library(raster)
crs(r_pred_curr) <- CRS("+proj=longlat +datum=WGS84")
r <- ratify(r_pred_curr)
levels(r)[[1]]$NAME <- letters[1:nrow(levels(r)[[1]])]
writeRaster(r,'cur_7km_final.tif',overwrite=TRUE)

r_pred_curr

jpeg(
  filename="cur_map_7km",
  width=8,
  height=5,
  units="in",
  res=300)
map_c
dev.off()

summary(map_c)


library(knitr)    # For knitting document and include_graphics function
library(ggplot2)  # For plotting
library(png)
attr(map_c, "info")



## future 
##CNRM
bioclim_future <- 
  raster::stack(
    c(
      bio_1  = './input_data/bio1_mean_1.asc',
      bio_8  = './input_data/bio1_mean_8.asc',
      bio_10  = './input_data/bio1_mean_10.asc',
      bio_13  = './input_data/bio1_mean_13.asc',
      bio_15  = './input_data/bio1_mean_15.asc',
      bio_19  = './input_data/bio1_mean_19.asc'
    )
  )

##ssp585
bioclim_future <- 
  raster::stack(
    c(
      bio_1  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp585/bio1_a.asc',
      #bio_2  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp585/bio2_a.asc',
      #bio_3  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp585/bio3_a.asc',
      #bio_7  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp585/bio7_a.asc',
      bio_8  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp585/bio8_a.asc',
      bio_10  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp585/_bio10_a.asc',
      bio_13  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp585/_bio13_a.asc',
      bio_15  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp585/_bio15_a.asc',
      bio_19  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp585/_bio19_a.asc'
    )
  )



##ssp126
bioclim_future <- 
  raster::stack(
    c(
      bio_1  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp126/_band1_a.asc',
      #bio_2  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp585/bio2_a.asc',
      #bio_3  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp585/bio3_a.asc',
      #bio_7  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp585/bio7_a.asc',
      bio_8  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp126/_band8_a.asc',
      bio_10  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp126/_band10_a.asc',
      bio_13  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp126/_band13_a.asc',
      bio_15  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp126/_band15_a.asc',
      bio_19  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp126/_band19_a.asc'
    )
  )



##old future126
bioclim_future <- 
  raster::stack(
    c(
      bio_1  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/cmip5/10m/bio1.asc',
      bio_8  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/cmip5/10m/bio8.asc',
      bio_10  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/cmip5/10m/bio10.asc',
      bio_13  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/cmip5/10m/bio13.asc',
      bio_15  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/cmip5/10m/bio15.asc',
      bio_19  = 'C:/Users/lk311/OneDrive/project/biomod2_video_single_species_modelling/cmip5/10m/bio19.asc'
    )
  )

##Old BCC 585
bioclim_future <- 
  raster::stack(
    c(
      bio_1  = 'C:/Users/lk311/Downloads/Biomod/CNM/share/spatial03/worldclim/cmip6/7_fut/10m/BCC-CSM2-MR/ssp585/bio1_a.asc',
      #bio_2  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp126/_band2_a.asc',
      #bio_3  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp126/_band3_a.asc',
      #bio_7  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp126/_band7_a.asc',
      bio_8  = 'C:/Users/lk311/Downloads/Biomod/CNM/share/spatial03/worldclim/cmip6/7_fut/10m/BCC-CSM2-MR/ssp585/bio8_a.asc',
      bio_10  = 'C:/Users/lk311/Downloads/Biomod/CNM/share/spatial03/worldclim/cmip6/7_fut/10m/BCC-CSM2-MR/ssp585/_bio10_a.asc',
      bio_13  = 'C:/Users/lk311/Downloads/Biomod/CNM/share/spatial03/worldclim/cmip6/7_fut/10m/BCC-CSM2-MR/ssp585/_bio13_a.asc',
      bio_15  = 'C:/Users/lk311/Downloads/Biomod/CNM/share/spatial03/worldclim/cmip6/7_fut/10m/BCC-CSM2-MR/ssp585/_bio15_a.asc',
      bio_19  = 'C:/Users/lk311/Downloads/Biomod/CNM/share/spatial03/worldclim/cmip6/7_fut/10m/BCC-CSM2-MR/ssp585/_bio19_a.asc'
    )
  )

##old BCC 126
bioclim_future <- 
  raster::stack(
    c(
      bio_1  = 'C:/Users/lk311/Downloads/Biomod/CNM/share/spatial03/worldclim/cmip6/7_fut/10m/BCC-CSM2-MR/ssp126/bio1_a.asc',
      #bio_2  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp126/_band2_a.asc',
      #bio_3  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp126/_band3_a.asc',
      #bio_7  = 'C:/Users/lk311/Downloads/Biomod/CanESM5/share/spatial03/worldclim/cmip6/7_fut/10m/CanESM5/ssp126/_band7_a.asc',
      bio_8  = 'C:/Users/lk311/Downloads/Biomod/CNM/share/spatial03/worldclim/cmip6/7_fut/10m/BCC-CSM2-MR/ssp126/bio8_a.asc',
      bio_10  = 'C:/Users/lk311/Downloads/Biomod/CNM/share/spatial03/worldclim/cmip6/7_fut/10m/BCC-CSM2-MR/ssp126/_bio10_a.asc',
      bio_13  = 'C:/Users/lk311/Downloads/Biomod/CNM/share/spatial03/worldclim/cmip6/7_fut/10m/BCC-CSM2-MR/ssp126/_bio13_a.asc',
      bio_15  = 'C:/Users/lk311/Downloads/Biomod/CNM/share/spatial03/worldclim/cmip6/7_fut/10m/BCC-CSM2-MR/ssp126/_bio15_a.asc',
      bio_19  = 'C:/Users/lk311/Downloads/Biomod/CNM/share/spatial03/worldclim/cmip6/7_fut/10m/BCC-CSM2-MR/ssp126/_bio19_a.asc'
    )
  )






bioclim_future <- resample(bioclim_future,bioclim)

env_future <- stack(bioclim_future, a)
plot(env_future)

masked_f <- mask(x = env_future, mask = denpasar)
plot(masked_f)
cropped_env_f <- crop(x = masked_f, y = extent(denpasar))
plot(cropped_env_f)


library(rgdal)
denpasar  <- readOGR('./input_data/RUS_adm0.shp')
#denpasar <- spTransform(x = denpasar, CRSobj = '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
#plot(denpasar)
masked <- mask(x = env_future , mask = denpasar)
plot(masked)
cropped_env_f <- crop(x = masked, y = extent(denpasar))
plot(cropped_env_f)
plot(awt)

awt.df <- as.data.frame(awt)

#all_data <- rbing(pa, )

plot(cropped_env_f$bio_1)
plot(cropped_env$bio_7)
cropped_env_f$bio_1 <- cropped_env_f$bio_1/10
cropped_env_f$bio_8 <- cropped_env_f$bio_8/10
cropped_env_f$bio_10 <- cropped_env_f$bio_10/10


plot(cropped_env_f$bio_8)



bio_fut_df <- data.frame(rasterToPoints(cropped_env_f))
bio_fut_df$pred_rf <- mecofun::predictSDM(rf, bio_fut_df)


r_pred_fut <- rasterFromXYZ(bio_fut_df[,-c(3:10)])
plot(r_pred_fut)



library(raster)
crs(r_pred_fut) <- CRS("+proj=longlat +datum=WGS84")
r <- ratify(r_pred_fut)
levels(r)[[1]]$NAME <- letters[1:nrow(levels(r)[[1]])]
writeRaster(r,'fut_7km_BCC-CSM2-MR_126.tif',overwrite=TRUE)





gplot(r_pred_fut) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                       na.value = "transparent",
                       name = "Probability") +
  labs(title = "Future projection of habitat suitability
       for H.S. in 2040-2060",
       x = "longitude",
       y = "latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())



map_c <- gplot(r_pred_curr) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                       na.value = "transparent",
                       name = "Probability") +
  labs(title = "Current projection for habitat suitability 
       of H.S. on the territory of Russia",
       x = "longitude",
       y = "latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())



jpeg(
  filename="map_future_25disk1_last.jpeg",
  width=8,
  height=5,
  units="in",
  res=500)
map_f
dev.off()

awt
plot(awt$bio_1)
plot(cropped_$bio_1)

knitr::opts_chunk$set(fig.width=12, fig.height=8) 


summary(bio_fut_df)



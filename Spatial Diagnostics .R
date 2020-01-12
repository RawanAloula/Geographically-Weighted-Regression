# Spatial Diagnostics 


library(spdep)
library(maptools)
library(rgdal)


# import shapefile 

Man.poly <- readOGR("CensusAndAirbnb.shp")
#Man.poly <- readShapePoly('CensusAndAirbnb.shp')  #it dosent always work

class(Man.poly)


# subset shapefile
myvars <- c("zcta", "bcode","DP0130010","perc_oou","Count_NUMP")
Man.poly <- Man.poly[myvars]

head(Man.poly@data)
class(Man.poly)

# Remove NA from data (317 before / 251 after)  = 66 rows removed 
Man.poly <- Man.poly[!is.na(Man.poly@data$perc_oou) ,]


#check the data type
sapply(Man.poly@data, class)


#change columns to numeric 
Man.poly@data$perc_oou <- as.numeric(Man.poly@data$perc_oou)
Man.poly@data$DP0130010 <- as.numeric(Man.poly@data$DP0130010)


#NOTE: the data accessed as following (@data$variable)

summary(Man.poly@data$Count_NUMP)

head(Man.poly@data)

plot(Man.poly)


# set CRS 
Man.poly <- spTransform(Man.poly, CRS("+init=epsg:4326"))

library(leaflet)
leaflet(Man.poly) %>%
  addPolygons(stroke = FALSE, fillOpacity = 0.5, smoothFactor = .5) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)  #the default is OpenStreetMap



require(RColorBrewer)
qpal<-colorQuantile("OrRd", Man.poly@data$Count_NUMP, n=10)    # n = number of breaks 

leaflet(Man.poly) %>%
  addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal(Count_NUMP)
  ) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)




############### Spatial Regression ###################

# X = "DP0130010","perc_oou"

#start by running a linear regression
Man.ols<-lm(Count_NUMP~DP0130010+perc_oou, 
            data=Man.poly@data)

#print results
summary(Man.ols)


#Create queens contiguous weights matrix

list.queen <- poly2nb(Man.poly, queen=TRUE)   #creates a list of neighbors

W <- nb2listw(list.queen, style="W", zero.policy=TRUE)  #convert list into a weights matrix 


# Moran's I (spatial dependence)
moran.lm <- lm.morantest(Man.ols, W, alternative="two.sided", zero.policy=TRUE)
print(moran.lm)


# Other dependence diagnostics (same as Geoda)
LM <- lm.LMtests(Man.ols, W, test="all", zero.policy=TRUE)
print(LM)

# Spatial lag model
sar.Man <- lagsarlm(Count_NUMP~DP0130010+perc_oou, data=Man.poly@data, W, zero.policy=TRUE)
summary(sar.Man) #NOTE: rho is listed below other model coefs


# Spatial error model
errorsalm.Man<-errorsarlm(Count_NUMP~DP0130010+perc_oou, data=Man.poly@data, W, zero.policy=TRUE)
summary(errorsalm.Man)


# Extract residuals 
Man.poly@data$Man.ols.res <- resid(Man.ols) #residuals OLS
Man.poly@data$Man.sar.res <- resid(sar.Man) #residual lag
Man.poly@data$Man.errorsalm.res <- resid(errorsalm.Man) #residual error

# explore 
hist(abs(Man.poly@data$Man.ols.res))

plot(Man.poly@data$Count_NUMP,Man.poly@data$Man.ols.res, xlab = "Numer of listings", ylab="Residuals")

plot(Man.poly@data$Count_NUMP,abs(Man.poly@data$Man.ols.res), xlab = "Numer of listings", ylab="|Residuals|")


# Map OLS residuals 
require(RColorBrewer)
qpal<-colorQuantile("Blues", Man.poly@data$Man.ols.res, n=9) 

leaflet(Man.poly) %>%
  addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal(Man.ols.res)
  ) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)


# Map lag residuals
qpal<-colorQuantile("Blues", Man.poly@data$Man.sar.res, n=9) 

leaflet(Man.poly) %>%
  addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal(Man.sar.res)
  ) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)


# Map error residuals
qpal<-colorQuantile("Blues", Man.poly@data$Man.errorsalm.res, n=9) 

leaflet(Man.poly) %>%
  addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal(Man.errorsalm.res)
  ) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)


# scatterplot matrix
pairs(~Count_NUMP+DP0130010+perc_oou,data=Man.poly@data,
      pch = 20, cex = 0.7, col = "red",
      lower.panel=NULL)


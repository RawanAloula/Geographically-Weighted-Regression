
#Geographically weighted regression 

# OLS
model1 <- lm(Count_NUMP~DP0130010+perc_oou, 
           data=Man.polyOGR2@data)

summary(model1)

# Extract residuals
Man.polyOGR2@data$man.ols.res<-resid(model1) 

# Map residuals 
library(RColorBrewer)
qpal<-colorQuantile("OrRd", Man.polyOGR2@data$man.ols.res, n=9) 

leaflet(Man.polyOGR2) %>%
  addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal(man.ols.res)
  ) %>%
  addTiles()


# Residuals cluster? Maybe GWR can help 

# Create center point for polygons (i.e. generate centroids)
library(rgeos)
centr <- gCentroid(Man.polyOGR2, byid = TRUE)  #byid tells function to add centroids per block group

# Add data from Man.polyOGR2@data to centr@data
centr <- SpatialPointsDataFrame(centr, data= Man.polyOGR2@data) 


#Begin GWR prep: First, calculate bandwidth

# Find optimal bandwidth 
#"uses cross validation to find kernel bandwidth that generates best model...e.g.-kernel that minimizes RSS"

GWRbandwidth <- gwr.sel(Count_NUMP~DP0130010+perc_oou, data=centr, 
                        coords=coordinates(centr),adapt=T)# coordinates() extracts x, y coordinates , adapt = adaptive bandwidth


# checkout result, use it below 
GWRbandwidth 


# GWR
gwr.model = gwr(Count_NUMP~DP0130010+perc_oou, data=centr, coords=coordinates, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 


#Warning message:
#  In gwr(Count_NUMP ~ DP0130010 + perc_oou, data = centr, coords = coordinates,  :
#           data is Spatial* object, ignoring coords argument


# Print results 
gwr.model   #"note summaries of coefficients for independent variables. More variation is evidence that local context matters"


#extract results for each variable
results<-as.data.frame(gwr.model$SDF)
head(results)


#attach coefficients to original dataframe
centr$coef_perc_oou <- results$perc_oou
centr$coef_DP0130010 <- results$DP0130010


summary(results$perc_oou) 
class(centr2)
head(centr2)



# maybe needed 
centr2 <- spTransform(centr, CRS("+init=epsg:4326"))
proj4string(centr2) <- CRS("+proj=longlat +datum=WGS84")



# somthing wrong with the map, invistigate why points are not projected 
head(centr2@coords[,1])
centr2@coords[,1]
str(centr2@data)


# clean coordinates and create new Spatial Data Fram 
centr3 <-  centr2[ which(centr2@coords[,2] < 41 & centr2@coords[,2] > 39 ) ,]
head(centr3)



#now plot the various GWR coefficients

#Does the homeownership rate influence short term rental activity uniformly across Manhattan?

qpal<-colorQuantile("OrRd", centr3@data$coef_perc_oou, n=9) 

leaflet(centr3) %>%
  addCircleMarkers(radius=4,color = ~qpal(centr3@data$coef_perc_oou)
  ) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)


#Does the nonfamily houshold rate influence short term rental activity uniformly across Manhattan?

qpal<-colorQuantile("OrRd", centr3@data$coef_DP0130010, n=9) 

leaflet(centr3) %>%
  addCircleMarkers(radius=4,color = ~qpal(centr3@data$coef_DP0130010)
  ) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)





# Alternative plots to leaflet quantile plot 

# First clean coordinates 

# create a new compy of the data
gwr.model2 <- gwr.model

gwr.model2$SDF@

gwr.model2$SDF <- gwr.model2$SDF[ which(gwr.model2$SDF@coords[,2] < 41 & gwr.model2$SDF@coords[,2] > 39 ) ,]


head(gwr.model$SDF$DP0130010)
head(centr3@data$DP0130010)

colorpalette<-c("dark blue","blue", "red", "dark red")

spplot(gwr.model$SDF, "DP0130010", cuts=quantile(gwr.model$SDF$DP0130010),
       col.regions=colorpalette, cex=.5) #cex changes size of plotted points


#or

library(RColorBrewer)
colorpalette<-brewer.pal(5, "RdBu") #get red to blue palette

colorpalette<-rev(colorpalette) #reverse order to get blue to red palette

quantiles<-quantile(gwr.model$SDF$DP0130010, c(0,.2,.4,.6,.8,1))

spplot(gwr.model$SDF, "DP0130010", cuts=quantiles,
       col.regions=colorpalette, cex=.5)


class(gwr.model)
head(gwr.model$SDF)
str(gwr.model)


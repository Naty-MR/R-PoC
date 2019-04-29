###### Common Libraries ######
library(rpart)
library(ggplot2)

###### Initail recover data ######
csv <- read.csv("crimes.csv")
attach(csv)
summary(csv)


####### Heatmap Libraries #######
library(reshape2)

####### Heatmap Functions #######

# Get lower triangle of the correlation matrix
get_lower_tri <- function(corrMat){
  corrMat[lower.tri(corrMat)] <- NA
  return(corrMat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(corrMat) {
  corrMat[uper.tri(corrMat)]<- NA
  return(corrMat)
}

# Use correlation between variables as distance
reorder_cormat <- function(corrMat) {
  dataDistance <- as.dist((1-corrMat)/2)
  cluster <- hclust(dataDistance)
  corrMat <-corrMat[cluster$order, cluster$order]
}

#Heatmap data preparation
drawSimpleHeatMap <- function () {
  csvAux <- csv
  dataSet <- sapply(csvAux, as.numeric) #transform data as numeric interpretation
  rm(csvAux)
  
  corrMat <- round(cor(dataSet), 2) #2 decimals round
  lower_tri <- get_lower_tri(corrMat)
  print(lower_tri)
  
  melted_corrMat <- melt(lower_tri, na.rm = TRUE)
  
  ggheatmap <- ggplot(melted_corrMat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "black")+
    scale_fill_gradient2(low = "white", high = "black", mid = "brown",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Crimen\nCorrelation") +
    labs(title = "", 
         subtitle = "", 
         y = "", 
         x = "",
         caption="MR") +
    theme_gray()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     size = 12, hjust = 1))+
    coord_fixed() +
    geom_text(aes(Var2, Var1, label = value), color = "grey", size = 2)
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
  
  print(ggheatmap)
  rm(corrMat)
  rm(dataSet)    
}


###### Map Libraries ######
library(OpenStreetMap)
library(rgdal)
library(mapview)
library(raster)

###### Map functions ######
drawGeoMap <- function() {
  data <- csv
  data <- subset(data, data$crimedescr=="10851 VC AUTO THEFT LOCATE")
  coords <- data.frame("latitude" = data$latitude, "longitude" = data$longitude)
  #convert longitud/latitudes to Mercator coordinates
  coords.mc <- cbind(coords, projectMercator(coords$latitude, coords$longitude))

  map <- openmap(c(38.44, -121.4), c(38.68, -121.6), zoom = 9, type= "osm")
  autoplot(map) + geom_point(data = coords.mc,
                  aes(x, y, size = 1, color = "red"),
                  alpha = I(8/10))
  #rm(data)
  #rm(coords)
}

drawSimpleHeatMap()
drawGeoMap()



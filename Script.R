
###################################################################
###   Análisis del espacio ambiental de Phaethornis mexicanus   ###
###################################################################    
## Autor: Jorge A. Valle, adaptado del script de Vicente J. Castillo

#The code takes bioclimatic data, crops it to the areas of study, extracts the environmental values 
#within those regions, and then creates a scatter plot of two bioclimatic 
#variables, showing the environmental space occupied by each area.  The ellipses help visualize the 
#spread and overlap of the environmental niches of the regions. This allows for a comparison of the 
#environmental conditions that characterize each area. 

#NOTA: Es importante ajustar los directorios de los archivos al directorio que se está usando

# # 1. Loading Libraries and Setting Working Directory # #

# Load libraries
library(terra)

# Primero lo primero
setwd ("Proyecto_final")

# # 2. Reading shapefiles  # #

#Shape del mapa base
shp_mex <- ("data/Mex_basemap/Mex.shp")
shp_mex <- vect(shp_mex)
plot(shp_mex)

# Read and assign name to the distributions shape files.

#Demostrativa para visualizar toda la distribución de P. mexicanus
P_mexicanus <- ("data/P_mexicanus_dist/P_mexicanus.shp") 
P_mexicanus <- vect(P_mexicanus)
plot(P_mexicanus)

#Distribución solo de P. mexicanus mexicanus
P_mexicanus_mexicanus <- ("data/P_mexicanus_dist/P_mexicanus_mexicanus.shp")
P_mexicanus_mexicanus <- vect(P_mexicanus_mexicanus)
plot(P_mexicanus_mexicanus)

#Distribución solo de P. mexicanus griseoventer
P_mexicanus_griseoventer <- ("data/P_mexicanus_dist/P_mexicanus_griseoventer.shp")
P_mexicanus_griseoventer <- vect(P_mexicanus_griseoventer)
plot(P_mexicanus_griseoventer)

# # 3. Plotting P. mexicanus distributions on Mexico # #

# Plot in the Geographic space
plot(shp_mex, col = "azure3")
plot(P_mexicanus, col = "burlywood3", add=T)
plot(P_mexicanus_griseoventer, col = "chartreuse4", add=T)
plot(P_mexicanus_mexicanus, col = "deeppink3", add=T)



# # 4. Loading and Preparing Bioclimatic Data # #

#Las variables bioclimáticas usadas para este análisis se obtuvieron de CHELSA,
#las variables seleccionadas se basaron en aquellas usadas
#para P. mexicanus en: Remolina-Figueroa et al. (2022) 
#https://doi.org/10.1007/s10584-022-03447-3 se cambio la Bio 2 por la Bio 1
#debido a
#Bio 1, Bio 3, Bio 5, Bio 12, Bio 15, Bio 17

# List the directory files

var_chelsa <- list.files(path="data/var_chelsa_sub", pattern = ".tif", full.names=TRUE)

# Hacer un stack de capas raster 
var_chelsa <- rast(var_chelsa)
var_chelsa
plot(var_chelsa)

#Definir el CRS

CRS.new <- crs("epsg:4326") 

# # 5. Cropping and Masking Bioclimatic Data to Ecoregions # #

# Recortar las variables a cada capa

#Recorte del area 1 P. mexicanus mexicanus

# Initialize an empty SpatRaster *before* the loop
p_mexicanus_mexicanus_stack <- rast()

for (i in seq_along(1:6)) {
  pres.stack.cut.ecos <- crop(var_chelsa[[i]], P_mexicanus_mexicanus)
  pres.stack.cut.ecos_mask <- mask(pres.stack.cut.ecos, P_mexicanus_mexicanus)
  crs(pres.stack.cut.ecos_mask) <- CRS.new
  p_mexicanus_mexicanus_stack <- c(p_mexicanus_mexicanus_stack, pres.stack.cut.ecos_mask)
  print(paste("Este es el area para p_mexicanus_mexicanus", i))
  plot(p_mexicanus_mexicanus_stack[[i]])
}

p_mexicanus_mexicanus_stack
plot(p_mexicanus_mexicanus_stack)

#Recorte del area 2 P. mexicanus griseoventer

p_mexicanus_griseoventer_stack <- rast()

for (i in seq_along(1:6)) {
  pres.stack.cut.ecos <- crop(var_chelsa[[i]], P_mexicanus_griseoventer)
  pres.stack.cut.ecos_mask <- mask(pres.stack.cut.ecos, P_mexicanus_griseoventer)
  crs(pres.stack.cut.ecos_mask) <- CRS.new
  p_mexicanus_griseoventer_stack <- c(p_mexicanus_griseoventer_stack, pres.stack.cut.ecos_mask)
  print(paste("Este es el area para p_mexicanus_griseoventer", i))
  plot(p_mexicanus_griseoventer_stack[[i]])
}

p_mexicanus_griseoventer_stack
plot(p_mexicanus_griseoventer_stack)


# # 6. Extracting Environmental Data # #

# Transfer to the environmental space
# Extract values
raster_names <- c("p_mexicanus_mexicanus","p_mexicanus_griseoventer")
extract.area_table <- c()

for (i in seq_along(raster_names)) { 
  
  # Stack for each area
  area_stack <-  paste0(raster_names[i], "_stack")
  print(paste0(raster_names[i], "_stack"))
  area_stack_get <- get(area_stack)
  
  # Extract COORDINATES
  coordinates_area <- xyFromCell(area_stack_get, 1:ncell(area_stack_get))
  coordinates_area_df <- as.data.frame(coordinates_area)
  # make object a spatial points
  sp.geog.area <- vect(coordinates_area_df, geom = c("x", "y"))
  crs(sp.geog.area) <- CRS.new
  #Extract values
  extract.vals.area <- extract(area_stack_get, sp.geog.area)
  extract.area_table <- cbind(coordinates_area_df, extract.vals.area)
  # paste the extracted values to dataframe 
  assign(paste0(raster_names[i], "_extract"), extract.area_table)
}

extract.area_table
summary(extract.area_table)
names(extract.area_table)


# # 7. Subsampling data # #
#Merge tables

for (i in seq_along(raster_names)) { 
  coordinates_area_extract <- paste0(raster_names[i], "_extract")
  coordinates_area_extract_get <- get(coordinates_area_extract)
  
  #Select 10 thousand at random
  extract.vals.area_val <- na.omit(coordinates_area_extract_get)
  if (nrow(extract.vals.area_val) < 10000) { 
    print("cool")
    print(nrow(extract.vals.area_val))
    assign(paste0(raster_names[i], "_extract_table"), extract.vals.area_val)
  } else { 
    random_coordinates <- extract.vals.area_val[sample(nrow(extract.vals.area_val), 10000), ]
    print(nrow(extract.vals.area_val))
    assign(paste0(raster_names[i], "_extract_table"), random_coordinates)
  }}

p_mexicanus_mexicanus_extract_table
p_mexicanus_griseoventer_extract_table


# # 8. Merging Data Tables # #

# Merge all tables
table_ecos <- c()
# Final table 
for (i in seq_along(raster_names)) { 
  extract_table <- get(paste0(raster_names[i], "_extract_table"))
  extract_table <- extract_table[1:8]
  extract_table$id <- NA
  extract_table$id <- paste0(raster_names[i])
  table_ecos <- rbind(table_ecos, extract_table)
}

# Table master
table_ecos

# # 9. Creating a Plot # #

# FIGURE plot
library(factoextra)
library(ggfortify)
library(ggplot2)

names_species <- c(
  "P. mexicanus mexicanus",
  "P. mexicanus griseoventer")

colors<-setNames(c("deeppink3",
                       "chartreuse4"), levels(table_ecos$id))

ee <- ggplot(table_ecos) +
  aes(Bio_1, Bio_12, color = as.factor(table_ecos$id)) + 
  geom_point(size = 0.1, alpha = 0.2) +
  xlab("Temperatura media anual")+
  ylab("Precipitacion media anual") +
  stat_ellipse(geom="polygon", level=0.80, alpha=0.2, lwd = 1.4) +
  scale_colour_manual(name="Area de distribucion", labels = names_species, values = colors) 

ee


##################
# Ahora en PCA 

pca_areas <- prcomp(table_ecos[ ,c(4:8)], scale = TRUE)

summary(pca_areas)

# Show the percentage of variances explained by each principal component.
fviz_eig(pca_areas)

eepca <- ggplot(pca_areas$x) +
  aes(PC1, PC2, color = as.factor(table_ecos$id)) + 
  geom_point(size = 0.1, alpha = 0.2) +
  ylim(-2.5, 5) +
  xlim(-5, 5) +
  xlab("PC1")+
  ylab("PC2") +
  stat_ellipse(geom="polygon", level=0.80, alpha=0.2, lwd = 1.4) +
  scale_colour_manual(name="Area de distribucion", labels = names_species, values = colors) 

eepca 


###################################################################################
# Análisis incluyendo el área en la que P. mexicanus no se distribuye (Michoacán)
###################################################################################

# #  Reading shapefiles  # #

mich <- ("data/P_mexicanus_dist/dis_mich.shp")
mich <- vect(mich)
plot(mich)

# # Plot in the Geographic space # #

plot(shp_mex, col = "azure3")
plot(mich, col = "#CD6600", add=T)

# # Cropping and Masking Bioclimatic Data # #

# Initialize an empty SpatRaster *before* the loop
mich_stack <- rast()

for (i in seq_along(1:6)) {
  pres.stack.cut.ecos <- crop(var_chelsa[[i]], mich)
  pres.stack.cut.ecos_mask <- mask(pres.stack.cut.ecos, mich)
  crs(pres.stack.cut.ecos_mask) <- CRS.new
  mich_stack <- c(mich_stack, pres.stack.cut.ecos_mask)
  print(paste("Este es el area para Michoacán", i))
  plot(mich_stack[[i]])
}

mich_stack
plot(mich_stack)


# #  Extracting Environmental Data # #

# Transfer to the environmental space
# Extract values
raster_name <- c("mich")
extract.area_table2 <- c()

for (i in seq_along(raster_name)) { 
  
  # Stack for each area
  area_stack2 <-  paste0(raster_name[i], "_stack")
  print(paste0(raster_name[i], "_stack"))
  area_stack_get2 <- get(area_stack2)
  
  # Extract COORDINATES
  coordinates_area2 <- xyFromCell(area_stack_get2, 1:ncell(area_stack_get2))
  coordinates_area_df2 <- as.data.frame(coordinates_area2)
  # make object a spatial points
  sp.geog.area2 <- vect(coordinates_area_df2, geom = c("x", "y"))
  crs(sp.geog.area2) <- CRS.new
  #Extract values
  extract.vals.area2 <- extract(area_stack_get2, sp.geog.area2)
  extract.area_table2 <- cbind(coordinates_area_df2, extract.vals.area2)
  # paste the extracted values to dataframe 
  assign(paste0(raster_name[i], "_extract"), extract.area_table2)
}

extract.area_table2
summary(extract.area_table2)
names(extract.area_table2)


# #  Subsampling data # #
#Merge tables

for (i in seq_along(raster_name)) { 
  coordinates_area_extract2 <- paste0(raster_name[i], "_extract")
  coordinates_area_extract_get2 <- get(coordinates_area_extract2)
  
  #Select 10 thousand at random
  extract.vals.area_val2 <- na.omit(coordinates_area_extract_get2)
  if (nrow(extract.vals.area_val2) < 10000) { 
    print("cool")
    print(nrow(extract.vals.area_val2))
    assign(paste0(raster_name[i], "_extract_table"), extract.vals.area_val2)
  } else { 
    random_coordinates2 <- extract.vals.area_val2[sample(nrow(extract.vals.area_val2), 10000), ]
    print(nrow(extract.vals.area_val2))
    assign(paste0(raster_name[i], "_extract_table"), random_coordinates2)
  }}

# Objects
mich_extract_table


# # Merging Data Tables # #

# Merge all tables
table_ecos2 <- c()
raster_names2 <- c("mich", "p_mexicanus_mexicanus", "p_mexicanus_griseoventer")

# Final table 
for (i in seq_along(raster_names2)) { 
  extract_table2 <- get(paste0(raster_names2[i], "_extract_table"))
  extract_table2 <- extract_table2[1:8]
  extract_table2$id <- NA
  extract_table2$id <- paste0(raster_names2[i])
  table_ecos2 <- rbind(table_ecos2, extract_table2)
}

# Table master
table_ecos2

# # 9. Creating a Plot # #

names_species <- c(
  "Michoacán",
  "P. mexicanus mexicanus",
  "P. mexicanus griseoventer")

colors<-setNames(c("#CD6600", "deeppink3",
                   "chartreuse4"), levels(table_ecos2$id))

ee2 <- ggplot(table_ecos2) +
  aes(Bio_1, Bio_12, color = as.factor(table_ecos2$id)) + 
  geom_point(size = 0.1, alpha = 0.2) +
  xlab("Temperatura media anual")+
  ylab("Precipitacion media anual") +
  stat_ellipse(geom="polygon", level=0.80, alpha=0.2, lwd = 1.4) +
  scale_colour_manual(name="Area de distribucion", labels = names_species, values = colors) 

ee2


##################
# Ahora en PCA 

pca_areas2 <- prcomp(table_ecos2[ ,c(4:8)], scale = TRUE)

summary(pca_areas2)

# Show the percentage of variances explained by each principal component.
fviz_eig(pca_areas2)

eepca2 <- ggplot(pca_areas2$x) +
  aes(PC1, PC2, color = as.factor(table_ecos2$id)) + 
  geom_point(size = 0.1, alpha = 0.2) +
  ylim(-2.5, 5) +
  xlim(-5, 5) +
  xlab("PC1")+
  ylab("PC2") +
  stat_ellipse(geom="polygon", level=0.80, alpha=0.2, lwd = 1.4) +
  scale_colour_manual(name="Area de distribucion", labels = names_species, values = colors) 

eepca2 




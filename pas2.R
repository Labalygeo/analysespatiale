# Librairies utilisées
library(sf)
library(mapview)
library(tidygeocoder)
library(osrm)
library(maptiles)
#Lire les couches qui sont dans le geopackage 
st_layers("sen.gpkg")

# Ouvrir une couche 
reg <- st_read("sen.gpkg", layer = "region")
dep <- st_read("sen.gpkg", layer = "departement")
mapview(dep) + mapview(reg)

st_crs(reg)
region <-st_transform(reg, crs = 4326)
 
# Créer des centroides 
 centro <- st_centroid(reg)
mapview(centro)
View(centro)
#Créer un buffer 
 buf <- st_buffer(centro, 20000)
 mapview(buf)
 
 # Prise en main du géocodage 
 
 Dk <- geo(city = "Dakar")
 kl <- geo(city = "Kaolack")
View(Dk) 

# Création du point  
lat<- 14.69342
long <- -17.44794
nom <-"Dakar"
Dk <-data.frame(nom,lat, long)
dks <- st_as_sf(Dk, coords=c("long","lat"), crs= 4326)

#Modifier la projection 
dks <-st_transform(dks, crs= 32628)
# Calcul de la distance entre Dk  et les centroides des regions 
test<-st_distance(x=dks, y=centro)
View(test)
#création d'une colonne  qui contiendra les données de la distance euclidienne  en km
centro$dist <- as.numeric(test) / 1000
View(centro)
View(reg)
centrosp <- write_sf(centro,"centrosp.geojson")
# Calcul de distance et de temps entre un point et plusieurs autres points 
library(osrm)
distance1 <- osrmTable(src = dks, 
                  dst = centro,
                  measure = c("distance", "duration"),
                  osrm.server = getOption("osrm.server"))
te <- st_read("exo.geojson")


distance2 <- osrmTable(src = dks, 
                       dst = te,
                       measure = c("distance", "duration"),
                       osrm.server = getOption("osrm.server"))
 View(distance2)
 te$route <- as.numeric(distance2$distances)/1000
View(te)

route2 <- osrmRoute(src = dks, dst = te)
mapview(route2)
 View(dist)
#création d'une colonne qui contiendra les données de la distance par la route   en km
centro$dist_route <- as.numeric(distance1$distance) / 1000
#création d'une colonne  qui contiendra les données de la durée en heure
centro$duree <- as.numeric(distance1$duration) / 60

#Calcul de la moyenne , max de la distance euclidienne
mean(centro$dist)
max(centro$dist)

#Calcul de la moyenne , max de la distance par la route
mean(centro$dist_route)
max(centro$dist_route)

#Calcul de la moyenne , max de la durée, min
mean(centro$duree)
max(centro$duree)
min(centro$duree)


#Import des tuiles OSM sur la base de la couche des centroides des regions 
#osm_tiles <- get_tiles(x = st_buffer(centro, 10000), zoom = 8, crop = TRUE)
zone <- get_tiles(reg, zoom = 8, crop = TRUE)
#Lancer les tuiles OSM
plot_tiles(zone)

plot(st_geometry(dks ), border = NA, col="red" , cex = 1.5, pch = 20, add = TRUE)
plot(st_geometry(centro), border = NA, col="black" , cex = 1, pch = 20, add = TRUE)
mtext(side = 0.2, line = -1, text = get_credit("OpenStreetMap"), col="tomato")

# Calcul des indices 
centro$ind_sinuo <- round(centro$dist_route/ centro$dist, 2)

centro$ind_speed <- round(centro$dist/ centro$duree, 1)

centro$ind_perf <- round(centro$ind_speed / centro$ind_sinuo, 1)

# Calcul  du point le plus loin 
city_max_perf <- centro[centro$ind_perf == max(centro$ind_perf),]
View(city_max_perf)
# Creation de la route 
route <- osrmRoute(src = dks, dst = city_max_perf)
st_write(route,"route1.geojson")
 #Affichage des couches
plot_tiles(zone)
plot(st_geometry(route), col = "red", lwd = 6, add = TRUE)
plot(st_geometry(route), col = "grey90", lwd = 1, add = TRUE)
plot(st_geometry(dks), border = NA, col="black", pch = 20, cex = 3, add = TRUE)
plot(st_geometry(city_max_perf), border = NA, col="green", pch = 20, cex = 3, add = TRUE)
mtext(side = 1, line = -1, text = get_credit("OpenStreetMap"), col="tomato")
??osrm
``
dm <-st_read("dmdsp.geojson")
dm2 <- st_read("dmndsp.geojson")
test <- st_read("centrosp2.geojson")
View(test)
dest <- st_read("centro3.geojson")
mapview(dest)
View(dest)

dest <- dest["THEME",]
library(osrm)
distancedm <- osrmTable(src = dm2, 
                       dst = dest,
                       measure = c("distance", "duration"))



View(dm2)
View(distancedm)
                       

# Download raw data

# download world basemap, ocean and graticules
download.file(url = "https://raw.githubusercontent.com/riatelab/basemaps/master/World/countries.geojson",
              destfile = "data/country.geojson")
# Graticules layer
download.file(url = "https://raw.githubusercontent.com/riatelab/basemaps/master/World/graticule30.geojson",
              destfile = "data/graticule.geojson")
download.file(url="http://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockByOriginAndDestination_2015.xlsx",
              destfile="data/mig.xlsx")


##############################################################################


# Prepare data

# libraries
library(sf)
library(countrycode)
library(readxl)

# import world countries and graticules
countries <- st_read(dsn = "data/country.geojson", stringsAsFactors = F)
mig <- data.frame(read_excel("data/mig.xlsx", skip = 15, sheet = "Table 16"))

# import migration data
mig$ISO3 <- countrycode(sourcevar = mig$X__4, 
                        origin = "iso3n", 
                        destination = "iso3c")
# merge countries geometries and mig data
countries <- merge(countries[,c("ISO3", "ISO2","name")], mig[,-(1:5)], 
                   by.x="ISO3", by.y="ISO3", all.x=TRUE)

# modify col names with ISO3 codes
coln <- countrycode(sourcevar = names(countries)[7:238], 
                    origin = "country.name",destination = "iso3c")
names(countries)[7:238] <- coln
names(countries)[48] <- "GGY"

# export the layer
st_write(countries, "data/countrriesmig.geojson")



##############################################################################


# import
graticules <- st_read(dsn = "data/graticule.geojson", quiet = TRUE)
countries <-  st_read(dsn = "data/countrriesmig.geojson", quiet = TRUE)

# plot the layers
par(mar = c(0,0,1.2,0))
# affichage des différentes couches
plot(st_geometry(graticules), col = "lightblue", lwd = 0.2)
plot(st_geometry(countries), col = "ivory4", border ="ivory3", 
     lwd = 0.5, add=TRUE)


# transformation de la projection WGS84 => Robinson
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
countries <- st_transform(x = countries, crs = crs)
graticules <- st_transform(x = graticules, crs = crs)

# plot the layers
plot(st_geometry(graticules), col = "lightblue", lwd = 0.2)
plot(st_geometry(countries), col = "ivory4", border ="ivory3", 
     lwd = 0.5, add=TRUE)


library(cartography)
# first map of syrian migrants in UE
par(mar = c(0,0,1.2,0))
UEcode <- c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", 
            "FI", "FR", "GR", "HR", "HU", "IE", "IS", "IT", "LI", "LT", "LU", 
            "LV", "MK", "MT", "NL", "NO", "PL", "PT", "RO", "SE", "SI", "SK", 
            "GB")
UE2015 <- countries[countries$ISO2 %in% UEcode,]

lim <- st_bbox(countries[countries$ISO3 %in% c("PRT", "FIN"),])
plot(st_geometry(graticules), col = "white", lwd = 0.2, bg="lightblue", 
     xlim = lim[c(1,3)], ylim = lim[c(2,4)]) 
plot(st_geometry(countries), col = "ivory4", border ="ivory3", 
     lwd = 0.5, add=TRUE)
plot(UE2015$geometry, col="black", add=T, border="white")
propSymbolsLayer(x = UE2015, var = "SYR",
                 inches=0.5, col="red",legend.frame = T, 
                 legend.pos = "topright")
layoutLayer("Syrian migrant in UE", sources = "",author = "", tabtitle = T)


# 2nd map, syrian migrant in Middle East
lim <- st_bbox(countries[countries$ISO3 %in% c("PRT", "FIN", "SAU"),])
plot(st_geometry(graticules), col = "white", lwd = 0.2, bg="lightblue", 
     xlim = lim[c(1,3)], ylim = lim[c(2,4)]) 
plot(st_geometry(countries), col = "ivory4", border ="ivory3", 
     lwd = 0.5, add=TRUE)
propSymbolsLayer(x = countries, var = "SYR", 
                 inches=1.2, col="red", legend.pos = "n")
x <- c(3180000, 3072000, 3202000, 3992000, 3851000, 2693000)
y <- c(5050000, 4465000, 3827000, 2625000, 3480000, 2765000)
options(scipen=3)
fl <- countries[order(countries$SYR, decreasing = T),
                    c("ISO3","SYR")][1:6,]
labs <- paste0(fl$ISO3,"\n",round(fl$SYR, -3)) 
text(x, y, labels = labs, font=4, col="white", cex=0.8)
layoutLayer("Ouf!", sources = "",author = "")









## small multiples
st_precision(countries) <- 1
sc <- st_simplify(st_union(countries ), dTolerance = 400000, preserveTopology = T)
sc <- st_cast(sc, "POLYGON")
sc <- sc[as.numeric(st_area(sc))>=100000000000]

par(mfrow=c(6,4),mar = c(0,0,1.2,0))
for(i in 1:24){
  plot(st_geometry(graticules), col = "lightblue",border=NA, bg="white")
  plot(st_geometry(sc), col = "ivory4", border ="ivory4",add=TRUE)
  propSymbolsLayer(x = countries, var = names(countries)[i+6],
                   fixmax=1000000, 
                   inches=0.1, col="red", legend.pos = "n", lwd=0.2)
  mtext(names(countries)[i+6], 3,  adj = c(0.25))
}





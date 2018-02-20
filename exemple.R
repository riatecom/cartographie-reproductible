library("rnaturalearth")
# download world basemap, ocean and graticules
download.file(url = "https://raw.githubusercontent.com/riatelab/basemaps/master/World/countries.geojson",
              destfile = "data/country.geojson")
# Graticules layer
download.file(url = "https://raw.githubusercontent.com/riatelab/basemaps/master/World/graticule30.geojson",
              destfile = "data/graticule.geojson")
download.file(url="http://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockByOriginAndDestination_2015.xlsx",
              destfile="data/mig.xlsx")


# libraries
library(sf)
# import
countries <- st_read(dsn = "data/country.geojson", quiet = TRUE, stringsAsFactors = F)
graticules <- st_read(dsn = "data/graticule.geojson", quiet = TRUE)

# transformation de la projection WGS84 => Robinson
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
countries <- st_transform(x = countries, crs = crs)
graticules <- st_transform(x = graticules, crs = crs)

par(mar = c(0,0,1.2,0))
# affichage des différentes couches
plot(st_geometry(graticules), col = "white", lwd = 0.2, bg="lightblue")
plot(st_geometry(countries), col = "ivory4", border ="ivory3", 
     lwd = 0.5, add=TRUE)



library(countrycode)
library(readxl)
mig <- data.frame(read_excel("data/mig.xlsx", skip = 15, sheet = "Table 16", ))
mig$ISO3 <- countrycode(sourcevar = mig$X__4, 
                        origin = "iso3n", 
                        destination = "iso3c")
countries2015 <- merge(countries[,c("ISO3", "ISO2","name")], mig[,-(1:5)], 
                   by.x="ISO3", by.y="ISO3", all.x=TRUE)


mig <- data.frame(read_excel("data/mig.xlsx", skip = 15, sheet = "Table 13", ))
mig$ISO3 <- countrycode(sourcevar = mig$X__4, 
                        origin = "iso3n", 
                        destination = "iso3c")
countries2010 <- merge(countries[,c("ISO3", "ISO2","name")], mig[,-(1:5)], 
                       by.x="ISO3", by.y="ISO3", all.x=TRUE)


library(cartography)




par(mar = c(0,0,1.2,0))
# affichage des différentes couches
plot(st_geometry(graticules), col = "white", lwd = 0.2, bg="lightblue")
plot(st_geometry(countries), col = "ivory4", border ="ivory3", 
     lwd = 0.5, add=TRUE)
propSymbolsLayer(x = countries2015, var = "Syrian.Arab.Republic")

lim <- st_bbox(countries[countries$ISO3 %in% c("PRT", "FIN"),])

UEcode <- c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", 
            "FI", "FR", "GR", "HR", "HU", "IE", "IS", "IT", "LI", "LT", "LU", 
            "LV", "MK", "MT", "NL", "NO", "PL", "PT", "RO", "SE", "SI", "SK", 
            "GB")

UE2015 <- countries2015[countries2015$ISO2 %in% UEcode,]


lim <- st_bbox(countries[countries$ISO3 %in% c("PRT", "FIN"),])

plot(st_geometry(graticules), col = "white", lwd = 0.2, bg="lightblue", xlim = lim[c(1,3)], ylim = lim[c(2,4)]) 
plot(st_geometry(countries), col = "ivory4", border ="ivory3", 
     lwd = 0.5, add=TRUE)
plot(UE2015$geometry, col="black", add=T, border="white")
propSymbolsLayer(x = UE2015, var = "Syrian.Arab.Republic", inches=0.5, col="red", legend.pos = "left")
layoutLayer("Danger!")



lim <- st_bbox(countries[countries$ISO3 %in% c("PRT", "FIN", "SAU"),])
plot(st_geometry(graticules), col = "white", lwd = 0.2, bg="lightblue", xlim = lim[c(1,3)], ylim = lim[c(2,4)]) 
plot(st_geometry(countries), col = "ivory4", border ="ivory3", 
     lwd = 0.5, add=TRUE)
propSymbolsLayer(x = countries2015, var = "Syrian.Arab.Republic", inches=1.2, col="red", legend.pos = "n")
x <- c(3180000, 3072000, 3202000, 3992000, 3851000, 2693000)
y <- c(5050000, 4465000, 3827000, 2625000, 3480000, 2765000)
text(x, y, labels = c("TUR", "LBN", "JOR", "SAU", "IRQ", "EGY"), font=2, col="white")
layoutLayer("Ouf!")




################## Install packages #####################################
# install packages
# install.packages("sf")
# install.packages("cartography")
# install.packages("readxl")







################## Import geo layer #####################################

# load libraries
library(sf)

# import communes of Auvergne-Rhone-Alpes region
reg84 <- st_read(dsn = "data/communes_reg_84.geojson", stringsAsFactors = F)

# view information on the geo layer com
head(reg84)

# display the layer
plot(st_geometry(reg84))

# Extract the Rhone Departement 
dep69 <- reg84[reg84$CODE_DEPT==69,]
plot(st_geometry(dep69))








################## Import data #####################################

library(readxl)
# import data from an INSEE excel file 
csp <- read_excel("data/pop-act2554-csp-cd-6814.xls", 
                  sheet = "COM_2014", skip = 15)
head(csp)

# select data for Rhone
csp69 <- data.frame(csp[csp$DR %in% 69,])
head(csp69)

# create INSEE_COM code
csp69$INSEE_COM <- paste0(csp69$DR, csp69$CR)

# suppress unused fields (the first 5 fields)
csp69 <- csp69[,-c(1:5)]

## ATTENTION ##
dep69[substr(dep69$NOM_COM, 1,4) =="LYON",  ] 
csp69[csp69$LIBELLE=="Lyon", ]

# aggregate Lyon arrondissements in dep69 into 1 commune
dep69[substr(dep69$NOM_COM, 1,4) =="LYON", "INSEE_COM" ] <- "69123"
dep69 <- aggregate(dep69, by=list(dep69$INSEE_COM), FUN = head, 1)
st_geometry(dep69) <- st_cast(dep69$geometry, "MULTIPOLYGON")
dep69 <- dep69[,"INSEE_COM"]
plot(st_geometry(dep69))

# merge com layer and data
dep69 <- merge(dep69, csp69, by="INSEE_COM", all.x=TRUE)
head(dep69)

# use friendly names
cs <- c("agr1", "agr0", "art1", "art0", "cad1", "cad0", "int1","int0", 
        "emp1", "emp0","ouv1", "ouv0")
names(dep69)[3:14] <- cs

dep69
for (i in 3:14){
  dep69[,i] <- round(dep69[,i, drop=T],0)
}


# agregate field (employed + unemployed)
dep69$agr <- dep69$agr0 + dep69$agr1
dep69$art <- dep69$art0 + dep69$art1
dep69$cad <- dep69$cad0 + dep69$cad1
dep69$int <- dep69$int0 + dep69$int1
dep69$emp <- dep69$emp0 + dep69$emp1
dep69$ouv <- dep69$ouv0 + dep69$ouv1
dep69$act <- dep69$agr + dep69$art + dep69$cad + dep69$int + dep69$emp + dep69$ouv

head(dep69)

# export in geojson format (===> Magrit)
st_write(dep69, "output/dep69.geojson", delete_dsn = T)



################## Cartography #####################################
library(cartography)


# change projection (lambert93)
dep69 <- st_transform(dep69, 2154)
plot(st_geometry(dep69))


# Map of active population
plot(st_geometry(dep69), col="lightblue", border="ivory")
propSymbolsLayer(dep69, var = "act", col="red")
title("Population active")


# Custom map of active population
par(mar=c(0.2,0.2,1.4,0.2), bg="grey95")
plot(st_geometry(dep69), col="lightblue", border="ivory")
propSymbolsLayer(dep69, var = "act", col="darkblue", inches = 0.6, 
                 border = "white", lwd=0.7, symbols = "square",
                 legend.style = "e", legend.pos="topright",
                 legend.title.txt = "Nombre d'actifs\n(2014)", 
                 legend.values.rnd = -1)
barscale(size = 10)
north(pos = "topleft", col = "darkblue")
layoutLayer(title = "Population active dans le département du Rhône", 
            sources = "Insee, 2018", author = "cartography 2.0.2", 
            col = "darkblue", coltitle = "white", 
            frame = TRUE, scale = NULL, north = FALSE)

# get ideal output ratio for the dep69 layer
exp_dim <- getFigDim(x = dep69, width = 600, 
                     mar = c(0.2,0.2,1.4,0.2), res = 100)

png(filename = "output/pop_act.png",
    width = exp_dim[1], height = exp_dim[2], res = 100)
# Custom map of active population
par(mar=c(0.2,0.2,1.4,0.2), bg="grey95")
plot(st_geometry(dep69), col="lightblue", border="ivory")
propSymbolsLayer(dep69, var = "act", col="darkblue", inches = 0.6, 
                 border = "white", lwd=0.7, symbols = "square",
                 legend.style = "e", legend.pos="topright",
                 legend.title.txt = "Nombre d'actifs\n(2014)", 
                 legend.values.rnd = -1)
barscale(size = 10)
north(pos = "topleft", col = "darkblue")
layoutLayer(title = "Population active dans le département du Rhône", 
            sources = "Insee, 2018", author = "cartography 2.0.2", 
            col = "darkblue", coltitle = "white", 
            frame = TRUE, scale = NULL, north = FALSE)
dev.off()



## Compare two maps
# use fixmax param
par(mar=c(0,0,1.2,0), bg="grey95", mfrow=c(1,2))
plot(st_geometry(dep69), col="lightblue", border="ivory")
propSymbolsLayer(dep69, var = "cad", col="darkblue", 
                 inches = 0.4, fixmax=63991,
                 border = "white", lwd=0.5,
                 legend.pos="n")
north(pos = "topleft", col = "darkblue")
layoutLayer(title = "Cadres", 
            sources = "Insee, 2018", author = "cartography 2.0.2", 
            col = "darkblue", coltitle = "white", postitle = "center",
            frame = F, scale = NULL, north = FALSE)
plot(st_geometry(dep69), col="lightblue", border="ivory")
propSymbolsLayer(dep69, var = "ouv", col="darkred", 
                 inches = 0.4, fixmax=63991,
                 border = "white", lwd=0.7, 
                 legend.pos="n")
barscale(size = 10)
layoutLayer(title = "Ouvriers", 
            sources = "", author = "", 
            col = "darkred", coltitle = "white",  postitle = "center",
            frame = F, scale = NULL, north = FALSE)
# use custom legend
legendCirclesSymbols(pos = "topright", title.txt = "Nombre d'actifs\n(2014)", 
                     var = c(10,10000,25000,64000),
                     inches = 0.4, col = "lightgrey")

## explore a map interactively
# install.packages(mapview)
# library(mapview)
# mapview(dep69) 

# add a few toponymes
labelLayer(x = dep69[dep69$INSEE_COM %in% c("69091","69243", "69256","69264"),], 
           txt = "LIBELLE", pos=3, halo=TRUE)




# Carte de Typologie
par(mar=c(0,0,1.2,0), bg="grey95", mfrow=c(1,1))
dep69$dom <- "Indéterminé"
dep69$r <- dep69$ouv / dep69$cad
dep69$r[is.na(dep69$r)] <- 0
dep69[dep69$r > 1.1,"dom"] <- "Plus d'ouvriers que de cadres"
dep69[dep69$r < 0.91,"dom"] <- "Plus de cadres que d'ouvriers"

typoLayer(dep69, var="dom", 
          legend.values.order = c("Plus d'ouvriers que de cadres", 
                                  "Plus de cadres que d'ouvriers",
                                  "Indéterminé" ),
          col = c('darkred', 'darkblue', 'grey'), 
          border = "white", lwd=0.5, legend.title.txt = "Type dominant") 
layoutLayer(title = "Ouvriers vs. Cadres", 
            sources = "", author = "", 
            col = "darkred", coltitle = "white",  postitle = "center",
            frame = F, scale = NULL, north = FALSE)      

par(mar=c(0,0,1.2,0), bg="grey95", mfrow=c(1,1))
plot(st_geometry(dep69), col="lightblue", border="ivory")
propSymbolsTypoLayer(dep69, var = "act",var2="dom", 
                     legend.var2.values.order = c("Plus d'ouvriers que de cadres", 
                                                  "Plus de cadres que d'ouvriers",
                                                  "Indéterminé" ),
                     col = c('darkred', 'darkblue', 'grey'), inches = 0.45,
                     legend.var.title.txt = "Nb. actifs", legend.var.pos = "right",
                     border = "white", lwd=0.8, legend.var2.title.txt = "Type dominant") 
layoutLayer(title = "Ouvriers vs. Cadres", 
            sources = "", author = "", 
            col = "darkred", coltitle = "white",  postitle = "center",
            frame = F, scale = NULL, north = FALSE)




# Cartes choroplèthes
dep69$pcad <- 100 * dep69$cad / dep69$act
summary(dep69$pcad)
par(mfrow=c(1,2))
boxplot(dep69$pcad)
hist(dep69$pcad, 20)
rug(dep69$pcad)

bks <- getBreaks(v = dep69$pcad, method = "q6")
cols <-carto.pal("green.pal", 3,"wine.pal",3)
par(mar=c(0.2,0.2,1.4,0.2), bg="grey95", mfrow=c(1,1))
choroLayer(dep69, var = "pcad", breaks = bks, 
           col = cols, border = "grey80", 
           lwd = 0.4, legend.pos = "topright", 
           legend.title.txt = "Part des cadres\n(en %)")
layoutLayer(title = "Les cadres", 
            sources = "", author = "", theme = "green.pal", 
            col = "darkred", coltitle = "white",  postitle = "center",
            frame = TRUE, scale = 10)
north(pos = "topleft", south = TRUE)




# Grid map
grid <- getGridLayer(x = dep69, cellsize = 3000*3000, 
                     type = "hexagonal", var = c('cad', 'act'))
grid$pcad <- 100*grid$cad/grid$act
par(mar=c(0.2,0.2,1.4,0.2), bg="grey95", mfrow=c(1,1))
choroLayer(grid, var = "pcad", breaks=bks, 
           col = cols, border = "grey80", 
           lwd = 0.4, legend.pos = "topright", 
           legend.title.txt = "Part des cadres\n(en %)")
layoutLayer(title = "Les cadres", 
            sources = "", author = "", theme = "green.pal", 
            col = "darkred", coltitle = "white",  postitle = "center",
            frame = TRUE, scale = 10)
north(pos = "topleft", south = TRUE)


# Smooth Map
grid$cad100 <- grid$cad * 100
smoothLayer(x = grid, var = "cad100", var2 = "act", typefct = "exponential", 
            span = 2500, beta = 2, breaks = bks, col = cols,
            legend.pos = "topright", mask =dep69,
            legend.title.txt = "Part des cadres\n(en %)", 
            border = "grey90", lwd = 0.2)
north(pos = "topleft", south = TRUE)
layoutLayer(title = "Les cadres", 
            sources = "", author = "", theme = "green.pal", 
            col = "darkred", coltitle = "white",  postitle = "center",
            frame = TRUE, scale = 10)
text(x = 866875, y = 6531871, 
     labels = "Lissage par potentiels\n fonction exponnentielle\n span = 2.5km, beta = 2", 
     font = 3, cex = 0.8)

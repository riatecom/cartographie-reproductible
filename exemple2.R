library(sf)
com <- st_read(dsn = "data/communes_reg_84.geojson", stringsAsFactors = F)
com <- st_transform(com, 2154 )
com <- com[,c("INSEE_COM")]



library(readxl)
csp <- read_excel("/home/tim/Téléchargements/pop-act2554-csp-cd-6814.xls", 
                  sheet = "COM_2014", skip = 15)
depcode <-  unique(com$CODE_DEPT)
csp <- data.frame(csp[csp$DR %in% depcode,])
csp$INSEE_COM <- paste0(csp$DR, csp$CR)
csp <- csp[,-c(1:5)]
reg <- st_union(com)

com[substr(com$INSEE_COM, 1,4) =="6938", "INSEE_COM" ] <- "69123"
com <- aggregate(com, by=list(com$INSEE_COM), FUN = head, 1)
com <- com[,-1]
com <- merge(com, csp, by="INSEE_COM", all.x=TRUE)
cs <- c("agr1", "agr0", "art1", "art0", "cad1", "cad0", "int1","int0", 
        "emp1", "emp0","ouv1", "ouv0")
names(com)[3:14] <- cs

st_geometry(com) <- st_cast(com$geometry, "MULTIPOLYGON")

com$agr <- com$agr0 + com$agr1
com$art <- com$art0 + com$art1
com$cad <- com$cad0 + com$cad1
com$int <- com$int0 + com$int1
com$emp <- com$emp0 + com$emp1
com$ouv <- com$ouv0 + com$ouv1
dev.off()
par(mfrow=c(2,3), mar=c(0,0,1.2,0))
plot(reg, col="grey", border=NA)
propSymbolsLayer(com, var = "agr", fixmax=60000, lwd=0.2, col="ivory1", legend.pos = "n")

plot(reg, col="grey", border=NA)
propSymbolsLayer(com, var = "art", fixmax=60000, lwd=0.2, col="ivory1", legend.pos = "n")

plot(reg, col="grey", border=NA)
propSymbolsLayer(com, var = "cad", fixmax=60000, lwd=0.2, col="ivory1", legend.pos = "n")

plot(reg, col="grey", border=NA)
propSymbolsLayer(com, var = "int", fixmax=60000, lwd=0.2, col="ivory1", legend.pos = "n")

plot(reg, col="grey", border=NA)
propSymbolsLayer(com, var = "emp", fixmax=60000, lwd=0.2, col="ivory1", legend.pos = "n")

plot(reg, col="grey", border=NA)
propSymbolsLayer(com, var = "ouv", fixmax=60000, lwd=0.2, col="ivory1", legend.pos = "n")


warnings()

warning()
dev.off()
summary(com)








com$ouvs <- 100*com$ouv0/com$act
library(cartography)
plot(reg, col="grey", border=NA)
propSymbolsChoroLayer(com[com$act>0,], var = "act", var2 = "emps")
choroLayer(com[com$ouv1>0,], var = "ouvs", border="white", method="q6", 
           lwd=0.2, legend.values.rnd = 2, add=T, 
           col=carto.pal("turquoise.pal", 6))

smoothLayer(x = com[com$emp1>0,], var = "ouv1", var2 = "act", span=10000, beta = 2,
                 resolution = 20000, mask = as(reg, "Spatial"))

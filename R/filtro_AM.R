#' @title Mark occurrences with the municipality informed different from the coordinate
#' @name filt
#'
#' @description A function to mark occurrences with the municipality informed different from the coordinate.
#'
#' @param pts data.frame. Table with points of occurrence, including the municipalities informed on the label. the data frame must contain the following columns in this order: "species","lon","lat", "municipality", "adm1"
#' @param inverted logical. If TRUE (default), it will check if longitude and latitude are changed. For now this option may be slow when there are many records of occurrence.
#' @param shape.municipios It can be a shape of municipalities of Brazil in format "SpatialPolygonsDataFrame". If it is NULL, the Brazilian shape will be used available on the IBGE website.
#'
#' @details
#'
#' @return a data frame
#'
#' @author Diogo S. B. Rocha
#'
#'
#' @examples
#' 
#' filt.generic(euterpe)
#'
#' @import raster
#' @import rgdal
#' 
#' @export

pts = read.table(
  "./lista.csv",
  h = T,
  sep = ";"
)

filt.generic = function(pts, shape, few.pts = T, value = 10){
  if(class(pts)!="data.frame"){
    stop('The occurrence points are only accepted in data.frame format, with at least the columns "sp", "lon", "lat" in the first positions')
  }else{
    st = names(pts)[1]!="species"|names(pts)[2]!="lon"|names(pts)[3]!= "lat"
    if(st){stop('The data.frame with occurrence points needs at least the columns "sp", "lon", "lat" in the first positions')}}
  pts$sp = as.character(pts$sp)
  especies <- unique(pts$sp)
  message(paste0("Species number: ", length(especies)), "\n", paste0("Records number: ", dim(pts)[1]))
  
  if(few.pts == T){
    message("\n#   Removing the species with", value,"or fewer records   #\n")
    pts = remove.pts(pts, especies, value = value)
  }
  
  message("\n# Checking the points #\n")
  
  coordinates(pts) <- ~ lon + lat
  
}




#-------------------#
# Filtros  BR e AMS #
#-------------------#

coordinates(pts) <- ~ lon + lat
pts

#am = readOGR("./SIG/SA.shp")
am = readOGR("C:\\Users\\JBRJ\\Documents\\MEGA\\Alunos\\joao\\Shapefiles\\ShapeNeo\\neotropic.shp")
am = spTransform(am,
                 CRS(
                   "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
                 ))
#am = am[, 4:5]
am = am[, 3:4]
am$nome = "Neotrop"
am


proj4string(pts) <-
  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


pts1 = as.data.frame(pts)

head(pts1)

(ini = Sys.time())
#pres = over(pts, am)[, 2]
pres = over(pts, am)[, 3]
Sys.time() - ini

pts1$loc = pres
head(pts1)

pts1$status = NA
head(pts1)


pb <- txtProgressBar(min = 1,
                     max = dim(pts1)[1],
                     style = 3)

#início do filtro

(ini = Sys.time())
for (i in 1:dim(pts1)[1]) {
  
  setTxtProgressBar(pb, i)
  
  if (is.na(pts1$loc[i]) & pts1$lon[i] > 90 | pts1$lon[i] < -90) {
    pts1$status[i] = "suspeita"
    }
  
  if(!is.na(pts1$loc[i])){
    pts1[i,"status"]="OK"
    }
  
  if (is.na(pts1$status[i])) {
    new = pts1[i, ]
    coordinates(new) = ~ lon + lat
    proj4string(new) <-
      CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    result = over(new, am)[, 3]
    if(is.na(result)){#testando invertidas
      new1 = pts1[i, ]
      coordinates(new1) = ~ lat + lon
      proj4string(new1) <-
        CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      result = over(new1, am)[, 3]
      if(!is.na(result)){
        pts1$status[i] = "invertida"
        pts1[i, c(1, 3, 2, 4, 5)] = pts1[i, c(1, 2, 3, 4, 5)]
        }
    }
    
    if(is.na(pts1$status[i])){#testando sinal lon
      pts1[i, c(2)] = (pts1[i, c(2)])*-1
      new1 = pts1[i, ]
      coordinates(new1) = ~ lon + lat
      proj4string(new1) <-
        CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      result = over(new1, am)[, 3]
      if(!is.na(result)){
        pts1$status[i] = "sinal_lon"
      }else{pts1[i, c(2)] = (pts1[i, c(2)])*-1}
    }
    
    if(is.na(pts1$status[i])){#testando sinal lat
      pts1[i, c(3)] = (pts1[i, c(3)])*-1
      new1 = pts1[i, ]
      coordinates(new1) = ~ lon + lat
      proj4string(new1) <-
        CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      result = over(new1, am)[, 3]
      if(!is.na(result)){
        pts1$status[i] = "sinal_lat"
      }else{pts1[i, c(3)] = (pts1[i, c(3)])*-1}
    }
    
    if(is.na(pts1$status[i])){#testando sinal lon e lat
      pts1[i, c(2,3)] = (pts1[i, c(2,3)])*-1
      new1 = pts1[i, ]
      coordinates(new1) = ~ lon + lat
      proj4string(new1) <-
        CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      result = over(new1, am)[, 3]
      if(!is.na(result)){
        pts1$status[i] = "sinal_lon_lat"
      }else{pts1[i, c(2,3)] = (pts1[i, c(2,3)])*-1}
    }
    
    if(is.na(pts1$status[i])){#testando sinal lon e lat
        pts1$status[i] = "suspeita"
    }
    
  }
  
}

fim = Sys.time()
(fim - ini)

# #envia email....
# mailR::send.mail(
#   from = "diogosbr@gmail.com",
#   to = c("diogosbr@gmail.com"),
#   #cc = c("CC Recipient <cc.recipient@gmail.com>"),
#   #bcc = c("BCC Recipient <bcc.recipient@gmail.com>"),
#   subject = "Joao Finished",
#   body = paste0("Seu job terminou!", "\n durou:", Sys.time() -
#                   ini),
#   smtp = list(host.name = "aspmx.l.google.com", port = 25),
#   authenticate = FALSE,
#   send = TRUE
# )


pts1$status = as.factor(pts1$status)

#resumo
table(pts1$status)

pts = pts1
coordinates(pts) = ~lon+lat


#plotando com base no status
plot(am, axes = T)
points(pts, col = pts1$status, pch = 16)
legend("bottomright", legend = unique(pts1$status), pch = 16,col = unique(pts1$status), bty="n", bg = "white")

maps::map()
maps::map.axes()
points(pts, col = pts1$status, pch = "+")

legend("bottomright", legend = unique(pts1$status),
       pch="+", col = unique(pts1$status), bty="o", bg = "white")



especies <- unique(pts1$sp)
#número de espécies
length(especies)

#número de registros não suspeitos:
dim(pts1[pts1$status!="suspeita",])[1]
#número de registros corrigidos:
dim(pts1[pts1$status=="invertida" | pts1$status=="sinal_lat" | pts1$status=="sinal_lon"| pts1$status=="sinal_lon_lat",])[1]


write.table(pts1[pts1$status!="suspeita",], "lista_joao_ok.csv", sep = ";", row.names = F)




pts1 = read.table("lista_joao_ok.csv", sep = ";", h = T)
head(pts1)
table(pts1$status)
unique(pts1$status)




##



#----------------------------------------------------------#
# deixando apenas os pontos espacialmente únicos e sem NAs #
#----------------------------------------------------------#

#carregando um raster das preditoras
r = raster("F:/Diogo/IIS/R/pca_iis.tif")
plot(r)


# Selecionado pontos espacialmente únicos #
reg.clean = c()

for(especie in especies){
  occs = pts1[pts1$species==especie, c('lon','lat')]
  cell <- cellFromXY(r, occs) # get the cell number for each point
  dup <- duplicated(cbind(occs, cell))
  occs2 = pts1[pts1$species==especie,]
  occs2 = occs2[!dup,]
  reg.clean = rbind(reg.clean,occs2)
}

head(reg.clean)
dim(reg.clean)

registros = reg.clean
registros$species = as.vector(registros$species)

especies <- unique(registros$sp)
especies

#------#
#Retirando as espécies com 10 ou menos registros
(ini = Sys.time())
registros.10 = c()
pb <- txtProgressBar(min = 1,
                     max = length(especies),
                     style = 3)
for (especie in especies) {
  setTxtProgressBar(pb, grep(especie, especies)[1])
  occs <- registros[registros$species == especie, ]
  if (dim(occs)[1] > 10) {
    #occs$sp=especie
    registros.10 = rbind(registros.10, occs)
  }
}
Sys.time() - ini

head(sort(table(registros.10$species)), 20)

especies <- unique(registros.10$sp)

#número de espécies
length(especies)
#número de registros
dim(registros.10)[1]

#-------------------#
# removendo os NAs #
#-------------------#

presvals <- raster::extract(r, registros.10[,c('lon', 'lat')])
compl <- complete.cases(presvals)
registros = registros.10[compl,]


#Retirando as espécies com 10 ou menos registros
(ini = Sys.time())
registros.10 = c()
pb <- txtProgressBar(min = 1,
                     max = length(especies),
                     style = 3)
for (especie in especies) {
  setTxtProgressBar(pb, grep(especie, especies)[1])
  occs <- registros[registros$species == especie, ]
  if (dim(occs)[1] > 10) {
    #occs$sp=especie
    registros.10 = rbind(registros.10, occs)
  }
}
Sys.time() - ini



especies <- unique(registros.10$sp)

#número de espécies
length(especies)
#número de registros
dim(registros.10)[1]

write.csv(registros.10, "../_especies_geral/_LIMPOS/OK_FLORA_ameacadas_250918.csv")

#-------------------#
#  This is the end  #
#  Beautiful friend #
#  This is the end  #
#-------------------#

#Função para marcar ocorrências com município informado diferente da coordenada.

filt = function(pts, shape.municipios){
  # instalando pacotes
  packages = c( "rgdal", "raster", "maptools", "dismo")
  for (p in setdiff(packages, installed.packages()[, "Package"])) {
    install.packages(p, dependencies = T)
  }
  
  #lendo pacotes
  require(rgdal)
  require(raster)
  require(maptools)
  require(dismo)
  
  #pts=manimax[,c("species","lon","lat","municipality", "adm1")]
  pts=na.exclude(pts)
  
  #convertendo em um objeto 'spatial'
  coordinates(pts)<- ~lon+lat
  
  if(missing(shape.municipios)){
    br_mun=readOGR("./Shapes/brasil_mun_ibge/brasil_mun_ibge.shp")
  } else(br_mun=shape.municipios)
  
  #br_mun=readShapeSpatial("./Shapes/brasil_mun_ibge/brasil_mun_ibge.shp")
  
  #atribuinto projeções aos shapes e aos pontos
  br_mun <- spTransform(br_mun, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  proj4string(pts) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  #criando um data frame
  pts1=as.data.frame(pts)
  
  #extraindo dados dos shapes a partir dos pontos
  muni_shape=over(pts,br_mun)[,c('NOMEMUNICP','NOMEUF')]
  muni_shape[,1]=as.vector(muni_shape[,1])
  muni_shape[,2]=as.vector(muni_shape[,2])
  pts1=cbind(pts1,muni_shape)
  pts1[,4]=as.vector(pts1[,4])
  pts1[,5]=as.vector(pts1[,5])
  
  for(i in 4:dim(pts1)[2]){
    pts1[,i]=tolower(pts1[,i])
    pts1[,i]=tolower(pts1[,i])
  }
  
  #função de Athos (https://pt.stackoverflow.com/questions/46473/remover-acentos)
  
  rm_accent <- function(str,pattern="all") {
    # Rotinas e funções úteis V 1.0
    # rm.accent - REMOVE ACENTOS DE PALAVRAS
    # Função que tira todos os acentos e pontuações de um vetor de strings.
    # Parâmetros:
    # str - vetor de strings que terão seus acentos retirados.
    # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
    #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
    #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
    #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
    if(!is.character(str))
      str <- as.character(str)
    
    pattern <- unique(pattern)
    
    if(any(pattern=="Ç"))
      pattern[pattern=="Ç"] <- "ç"
    
    symbols <- c(
      acute = "áéíóúÁÉÍÓÚýÝ",
      grave = "àèìòùÀÈÌÒÙ",
      circunflex = "âêîôûÂÊÎÔÛ",
      tilde = "ãõÃÕñÑ",
      umlaut = "äëïöüÄËÏÖÜÿ",
      cedil = "çÇ"
    )
    
    nudeSymbols <- c(
      acute = "aeiouAEIOUyY",
      grave = "aeiouAEIOU",
      circunflex = "aeiouAEIOU",
      tilde = "aoAOnN",
      umlaut = "aeiouAEIOUy",
      cedil = "cC"
    )
    
    accentTypes <- c("´","`","^","~","¨","ç")
    
    if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
      return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
    
    for(i in which(accentTypes%in%pattern))
      str <- chartr(symbols[i],nudeSymbols[i], str)
    
    return(str)
  }
  
  pts1$municipality=rm_accent(pts1$municipality)
  pts1$filt="Ok"
  
  for(i in 1:dim(pts1)[1]){
    if(is.na(pts1$municipality==pts1$NOMEMUNICP)[i]==TRUE){
      pts1[i,"filt"]="suspeito"
      pts1[i,"NOMEMUNICP"]="Fora do Brasil"
      pts1[i,"NOMEUF"]="Fora do Brasil"
    }
  }
  
  for(i in 1:dim(pts1)[1]){
    if((pts1$municipality==pts1$NOMEMUNICP)[i]==FALSE){
      pts1[i,"filt"]="suspeito"
    }
  }
  
  #pts2=pts1[pts1$filt=="suspeito",]
  
  #convertendo em um objeto 'spatial'
  #coordinates(pts)<- ~lon+lat
  coordinates(pts1)<- ~lat+lon
  
  #criando um data frame
  pts2=as.data.frame(pts1)

  #atribuinto projeçãos aos pontos
  proj4string(pts1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  #extraindo dados dos shapes a partir dos pontos
  muni_shape=over(pts1,br_mun)[,c('NOMEMUNICP','NOMEUF')]
  muni_shape[,1]=as.vector(muni_shape[,1])
  muni_shape[,2]=as.vector(muni_shape[,2])
  pts2=cbind(pts2,muni_shape)
  pts2[,9]=as.vector(pts2[,9])
  pts2[,10]=as.vector(pts2[,10])
  
  for(i in 4:dim(pts2)[2]){
    pts2[,i]=tolower(pts2[,i])
    pts2[,i]=tolower(pts2[,i])
  }
  
  for(i in 1:dim(pts2)[1]){
    if(is.na(pts2$municipality==pts2[,9])[i]==TRUE){
      pts2[i,9]="Fora do Brasil"
      pts2[i,10]="Fora do Brasil"
    }
  }
  
  for(i in 1:dim(pts2)[1]){
    if((pts2$municipality==pts2[,9])[i]==TRUE){
      pts2[i,"filt"]="Invertidas"
    }
  }
  
  pts2[pts2$filt=="suspeito",2:3]=pts2[pts2$filt=="suspeito",2:3]*(-1)
  
  coordinates(pts2)<- ~lon+lat
  
  #criando um data frame
  pts3=as.data.frame(pts2)
  
  #atribuinto projeçãos aos pontos
  proj4string(pts2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  #extraindo dados dos shapes a partir dos pontos
  muni_shape=over(pts2,br_mun)[,c('NOMEMUNICP','NOMEUF')]
  muni_shape[,1]=as.vector(muni_shape[,1])
  muni_shape[,2]=as.vector(muni_shape[,2])
  pts3=cbind(pts3,muni_shape)
  pts3[,11]=as.vector(pts3[,11])
  pts3[,12]=as.vector(pts3[,12])
  
  for(i in 4:dim(pts3)[2]){
    pts3[,i]=tolower(pts3[,i])
    pts3[,i]=tolower(pts3[,i])
  }
  
  for(i in 1:dim(pts3)[1]){
    if(is.na(pts3$municipality==pts3[,11])[i]==TRUE){
      pts3[i,11]="Fora do Brasil"
      pts3[i,12]="Fora do Brasil"
    }
  }
  
  for(i in 1:dim(pts3)[1]){
    if((pts3$municipality==pts3[,10])[i]==TRUE){
      pts3[i,"filt"]="Sinal_trocado"
    }
  }
  
  return(pts3)
}





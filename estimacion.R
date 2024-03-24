library(pacman)
p_load(dplyr)

#periodo = 1: Enero - Junio
#poriodo = 2: Julio - Diciembre
lee_año = function(año,periodo = 1){
  if(periodo == 1){
    nombre = paste0("./Datos/tabula-Tutorados ",año,"-EJ.csv")
    archivo1 = read.csv(nombre,blank.lines.skip = TRUE)
    return(archivo1)
  }else{
    nombre2 = paste0("./Datos/tabula-Tutorados ",año,"-JD.csv")
    archivo2 = read.csv(nombre2, blank.lines.skip = TRUE)
    return(archivo2)
  }
}

# preprocesamiento

#los siguientes años cuentan con ambos periodos
años = c(2015,2016,2017,2021,2023)
nombres = c("cuenta","semestre")

#2015 primer periodo
a2015 = lee_año(2015,1)
#quitamos aquellas filas que no contienen un numero de cuenta
indices = grepl("[0-9]{6}", a2015[,1])
a2015 = a2015[indices,]
a2015 = a2015[,c(1,2)]
colnames(a2015) = nombres

#2015 segundo periodo
b2015 = lee_año(2015,2)
b2015 = na.omit(b2015)[,1:2]
colnames(b2015) = nombres

#2016 primer periodo
a2016 = lee_año(2016,1)[,1:2]
a2016 = na.omit(a2016)
colnames(a2016) = nombres
#a2016[,1] = as.integer(a2016[,1])

#2016 segundo periodo
b2016 = lee_año(2016,2)[,1:2]
b2016 = b2016[grepl("[0-9]{6}",b2016[,1]),]
colnames(b2016) = nombres
b2016$cuenta = as.integer(b2016$cuenta)

#2017 primer periodo
a2017 = lee_año(2017,1)[,1:2]
a2017 = na.omit(a2017)
colnames(a2017) = nombres
a2017$cuenta = as.integer(a2017$cuenta)

#2017 segundo periodo
b2017 = lee_año(2017,2)[,1:2]
b2017 = b2017[grepl("[0-9]{6}",b2017[,1]),]
colnames(b2017) = nombres
b2017$cuenta = as.integer(b2017$cuenta)

#2021 primer periodo
a2021 = lee_año(2021,1)[,c(1,3)]
colnames(a2021) = nombres

#2021 segundo periodo
b2021 = lee_año(2021,2)[,c(1,3)]
colnames(b2021) = nombres

#2022 segundo periodo
b2022 = lee_año(2022,2)[,1:2]
colnames(b2022) = c("semestre","cuenta")
b2022 = b2022[-c(1),]
b2022 = b2022[grepl("[0-9]{6}",b2022[,2]),]

#2023 primer periodo
a2023 = lee_año(2023,1)[,c(1,3)]
a2023 = a2023[grepl("[0-9]{6}",a2023[,1]),]
colnames(a2023) = nombres

#2023 segundo periodo
b2023 = lee_año(2023,2)[,c(2,4)]
b2023 = b2023[grepl("[0-9]{6}",b2023[,1]),]
colnames(b2023) = nombres


## Hasta este punto se han cargado todos los datos y se encuentran limpios
## cada variable consta de una columna 
## No cuenta | Semestre


## Esta funcion hace el recuento de los estudiantes
## y devuelve una matriz que tiene elementos n_{i,j} 
## donde indica el numero de estudiantes que estaban en i y pasaron a estar en j
cuenta = function(pasado,presente){
  interseccion = inner_join(pasado,presente, by = "cuenta")
  mat = matrix(rep(0,12^2),nrow = 12,ncol = 12)
  nom = c(1:10,"deserta","egresa")
  colnames(mat) = nom
  rownames(mat) = nom
  
  for(i in 1:10){
    #Estudiantes que desertaron
    mat[i,11] = sum(!(subset(pasado,semestre == i)$cuenta %in% presente$cuenta))
    # Aquellos que no avanzan de grado
    mat[i,i] = nrow(subset(interseccion,semestre.x == i & semestre.y == i))
    # Aquellos que logran avanzar
    mat[i,i+1] = nrow(subset(interseccion,semestre.x == i & semestre.y == i+1))
    
    # Estudiantes que van a egresar
    mat[10,12] = length(which(pasado$semestre == "10"))
    
    # Estudiantes que ya egresaron
    mat[10,12] = mat[10,12] + length(which(presente$semestre == "E"))
  }
  return(mat)
}

## Existe un problema el semestre julio - diciembre 2015,
## sucede que no se anota el numero de cuenta de los alumnos de nuevo ingreso

## Esta funcion hara las estimaciones con las matrices,
## esto tratando el estimador de probabilidades no estacionarias
calcula_prob = function(matriz){
  k = rowSums(matriz)
  k[k == 0] = 1
  k = 1/k
  return(diag(k) %*% matriz)
}


matrices = list(
  cuenta(a2015,b2015),
  cuenta(b2015,a2016),
  cuenta(a2016,b2016),
  cuenta(b2016,a2017),
  cuenta(a2017,b2017),
  cuenta(a2021,b2021),
  cuenta(b2022,a2023),
  cuenta(a2023,b2023)
)

prob_matrices = lapply(matrices,calcula_prob)

## Ahora haremos estimaciones como si se tratara de 
## una cadena estacionaria

M = Reduce("+",matrices)
P = calcula_prob(M)

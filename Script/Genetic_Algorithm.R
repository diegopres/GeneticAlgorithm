### Algoritmo Genético
library(genalg)

#7 genotipos para cada carrera
# posicion 0: incremento matrícula 50%
# posicion 1: incremento matrícula 25%
# posicion 2: incremento matrícula 10%
# posicion 3: queda igual
# posicion 4: decremento del 20%
# posicion 5: decremento del 50%
# posicion 6: eliminar

#1- Ingenieria Civil
#2- Ingenieria Electromecanica
#3- Ingenieria en Gestion Empresarial
#4- Ingenieria en Sistemas Automotrices
#5- Ingenieria en Sistemas Computacionales
#6- Ingenieria Industrial


limite.presupuesto <- 1500000
cantidad.carreras <- 6

# Nombre de Carreras
vector.carreras <- c("Ingenieria Civil", "Ingenieria Electromecanica", "Ingenieria en Gestion Empresarial", 
                     "Ingenieria en Sistemas Automotrices", "Ingenieria en Sistemas Computacionales",
                     "Ingenieria Industrial")

# Costo de matricula por cada carrera (6 carreras)
costo.carrera <- c(2400, 2800, 1800, 3200, 2400, 2200)
# Se tomo como base el número de ingresos.
oferta.actual <- c(50, 40, 150, 140, 50, 70)
# Tendencia de la carrera basado en el crecimiento empresarial obentido del reporte y de los datos.
tendencia.empresas <- c(10, 10, 30, 30, 20, 30)
#Numero de empresas
numero.empresas <- c(50, 30, 400, 500, 450, 1000)
# Crear dataset
gen.data.set <- data.frame(carreras = vector.carreras, costoCarrera = costo.carrera, oferta = oferta.actual, 
                           tendencia = tendencia.empresas, empresas = numero.empresas)

# Costo por cada genotipo (7 genotipos para cada carrera)
costo.medida <- c(50, 25, 10,0, 2, 3, 5)
genotipos <- 7
cromosoma.len <- cantidad.carreras * genotipos


# Agregar la proyeccion para que se tome en cuenta la imporancia de la carrera (e.g. castigar si se quiere incrementar una carrera poco demandada)
# Agregar el presupuesto como limite (castigo si se pasa)
gen.data.set$empresas[1]
fitness.educacion.v1 <- function(x) {

  carrera.actual <- 1
  medida.tomada <- 0
  costo.total <- 0
  beneficio <- 0
  
  
  for (i in 1:cantidad.carreras) {
      cantidad.unos.segmento <- sum(x[((i - 1)*genotipos + 1):(i*genotipos)]) # valida un uno por
      if (cantidad.unos.segmento != 1) {
        return(4 * (cantidad.unos.segmento+1)) # favorecer menos unos en segmentos de 7
      }
      for (j in 1:genotipos){
        if (x[((i-1)*genotipos)+ j]==1) {
          costo.total <- costo.total + ((1 + (costo.medida[j]/100)) * gen.data.set$costoCarrera[i] * gen.data.set$oferta[i])
          print(costo.total)
          beneficio <- beneficio + (gen.data.set$tendencia[i] * gen.data.set$empresas[i] * costo.medida[j])
        }
        
        if (costo.total > limite.presupuesto)
        {
          return(1000)
        }
    }
  }
  
  return(-beneficio)
}
gen.data.set$tendencia[]
#vector de 42 elementos (7 genotipos * 6 carreras)
test <- sample(c(0,1),42,replace=T, prob=c(0.7,0.3))
test
fitness.educacion.v1(test)

?rbga.bin


ga.results <- rbga.bin(size=cromosoma.len, 
                       popSize=500,
                       iters=30, 
                       mutationChance=0.05,
                       elitism=60, 
                       evalFunc=fitness.educacion.v1,
                       verbose = T
)
plot(ga.results)

summary(ga.results, echo=T)
ga.results

decision <- c("Aumentar un 50% la carrera de", "Aumentar un 25% la carrera de", "Aumentar un 10% la carrera de",
              "Dejar igua la carrera de", "Disminuir un 20% la carrera de", "Disminuir un 10% la carrera de",
              "Eliminar la carrera de")

recommendation <- function(x) {
  costo.total <- 0
  for (i in 1:cantidad.carreras) {
    for (j in 1:genotipos){
      if(x[((i-1)*genotipos)+ j]==1){
        print(paste(decision[j],vector.carreras[i],sep=" "))
        costo.total <- costo.total + ((1 + (costo.medida[j]/100)) * gen.data.set$costoCarrera[i] * gen.data.set$oferta[i])
      }
    }
    
  }
  print(paste("Costo total:",as.character(costo.total)))
  
}
bestSolution<-ga.results$population[which.min(ga.results$evaluations),]
recommendation(bestSolution)

nr_simulari<-100000
nr_linii<-5

#simularea numarului de intersectii Z
simuleaza_cruce <- function() 
{
  d<-runif(1, 0, 0.5)
  theta <- runif(1, 0, pi/2)
  
  intersectie_orizontala<-as.numeric(d<=0.5*sin(theta))
  intersectie_verticala<-as.numeric(d<=0.5*cos(theta))
  
  return(intersectie_orizontala+intersectie_verticala)
}

Z_valori<-replicate(nr_simulari, simuleaza_cruce())

#media si varianta lui Z/2
Z_medie<-mean(Z_valori)/2
Z_varianta<-var(Z_valori)/4
cat("Media simulata a lui Z/2:", Z_medie, "\n")
cat("Varianta simulata a lui Z/2:", Z_varianta, "\n")

media_teoretica<-2/pi
varianta_teoretica<-(((3-sqrt(2))/pi)-4/(pi^2))
cat("Media teoretica a lui Z/2:", media_teoretica, "\n")
cat("Varianta teoretica a lui Z/2:", varianta_teoretica, "\n")

plot(NA, xlim=c(-nr_linii, nr_linii), ylim=c(-nr_linii, nr_linii), xlab="X", ylab="Y", main="Aruncarea crucii pe plan")
abline(h=-nr_linii:nr_linii, col="gray", lty=2) #desenarea liniilor orizontale

#functie pentru a desena o cruce
deseneaza_cruce <- function() 
{
  d<-runif(1, 0, 0.5)
  theta <- runif(1, 0, pi/2)
  
  intersectie_orizontala<-as.numeric(d<=0.5*sin(theta))
  intersectie_verticala<-d<=0.5*cos(theta)
  culoare<-ifelse(intersectie_orizontala|intersectie_verticala, "green", "blue")
  
  #coordonatele centrului crucii
  centru_x<-runif(1, -nr_linii+0.5, nr_linii-0.5)
  centru_y<-runif(1, -nr_linii, nr_linii)
  
  #segmentele orizontal si vertical
  x_orizontal1<-centru_x-0.5*cos(theta)
  y_orizontal1<-centru_y-0.5*sin(theta)
  x_orizontal2<-centru_x+0.5*cos(theta)
  y_orizontal2<-centru_y+0.5*sin(theta)
  x_vertical1<-centru_x+0.5*sin(theta)
  y_vertical1<-centru_y-0.5*cos(theta)
  x_vertical2<-centru_x-0.5*sin(theta)
  y_vertical2<-centru_y+0.5*cos(theta)
  
  lines(c(x_orizontal1,x_orizontal2),c(y_orizontal1,y_orizontal2), col=culoare, lwd=2)
  lines(c(x_vertical1,x_vertical2),c(y_vertical1,y_vertical2), col=culoare, lwd=2)
}

deseneaza_cruce()
L<-1 #lungimea acului
distanta_linii<-1 #distanta intre liniile paralele de pe grafic
nr_linii<-5 #numarul de linii paralele de pe grafic
nr_aruncari<-10000 #numarul total de aruncari

#generam liniile paralele (y = n)
linii_y<-seq(-nr_linii, nr_linii, by=distanta_linii)

#simulam nr_aruncari aruncari de ace
intersectari<-0
for(i in 1:nr_aruncari) 
{
  #generam pozitia centrului/mijlocului acului si unghiul sau
  centruAcX<-runif(1, 0, nr_linii) #coordonata de pe axa Ox a centrului acului
  centruAcY<-runif(1, min(linii_y), max(linii_y)) #coordonata de pe axa Oy a centrului acului
  unghiTheta<-runif(1, 0, pi) #unghiul theta al acului(de la 0 pana la pi)
  
  #calculam capetele acului
  y1<-centruAcY-(L/2)*sin(unghiTheta) #coordonata de pe axa Oy a capatului stang al acului
  y2<-centruAcY+(L/2)*sin(unghiTheta) #coordonata de pe axa Oy a capatului drept al acului
  
  #verificam daca acul intersecteaza o linie
  if(floor(y1/distanta_linii) != floor(y2/distanta_linii))
    intersectari<-intersectari+1
}

#probabilitatea simulata
P_simulata<-intersectari/nr_aruncari

#probabilitatea teoretica folosind formula
P_teoretica<-(2*L)/(pi*distanta_linii) #<=>2/pi pentru ca L=1 si distanta_linii=1

cat("Probabilitatea simulata (nr_aruncari=", nr_aruncari, "aruncari):", P_simulata, "\n")
cat("probabilitatea teoretica:", P_teoretica, "\n")

#GRAFIC pentru un singur ac

#generam pozitia centrului/mijlocului acului si unghiul sau
centruAcX<-runif(1, 0, nr_linii) #coordonata de pe axa Ox a centrului acului
centruAcY<-runif(1, min(linii_y), max(linii_y)) #coordonata de pe axa Oy a centrului acului
unghiTheta<-runif(1, 0, pi) #unghiul theta al acului(de la 0 pana la pi)

#calculam capetele acului
x1<-centruAcX-(L/2)*cos(unghiTheta) #coordonata de pe axa Ox a capatului stang al acului
y1<-centruAcY-(L/2)*sin(unghiTheta) #coordonata de pe axa Oy a capatului stang al acului
y2<-centruAcY+(L/2)*sin(unghiTheta) #coordonata de pe axa Oy a capatului drept al acului
x2<-centruAcX+(L/2)*cos(unghiTheta) #coordonata de pe axa Ox a capatului drept al acului

#verificam daca acul intersecteaza o linie
intersecteaza<-floor(y1/distanta_linii) != floor(y2/distanta_linii)

#desenam graficul
plot(NULL, xlim=c(0, nr_linii), ylim=c(min(linii_y)-1, max(linii_y)+1), xlab="X", ylab="Y", main="Aruncarea unui singur ac in problema Buffon cu n linii", asp=1)

# liniile paralele
for(linie in linii_y)
  abline(h=linie, col="gray", lty=2) #liniile gri
#desenam acul
culoare<-ifelse(intersecteaza, "red", "blue") #rosu daca intersecteaza, albastru altfel
segments(x1, y1, x2, y2, col = culoare, lwd = 3)

#legenda
legend("bottom", inset=-0.6, legend=c("acul intersecteaza", "acul nu intersecteaza"), col=c("red", "blue"), lwd=3, horiz=TRUE, xpd=TRUE)
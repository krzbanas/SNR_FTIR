library(hyperSpec)
library(RColorBrewer)

#ustaw katalog roboczy
setwd("D:/SNR_FTIR/DATA")
#odczytaj liste plikow w katalogu

files <- list.files(path = "/SNR_FTIR/DATA")

#odczytaj pierwsze widmo do bufora
buffer <- read.table(files [1], sep=",")

#alokuj macierz o liczbie kolumn rownej liczbie wierszy widma w buforze i liczbie wierszy rownej liczbie naszych danych
spc <- matrix (ncol = nrow (buffer), nrow = length (files))

#wpisz do pierwszego wiersza widmo numer 1
spc [1, ] <- buffer [, 2]

# a teraz pozostale dane
for (f in seq (along = files)[-1]) {
  buffer <- read.table(files [f], sep=",")
  spc [f, ] <- buffer[, 2]
}

# wektor osi X
wavenumber = buffer[, 1]

#utworz dodatkowe informacje dla pliku hyperSpec
d = data.frame(files)

#utworz obiekt hyperSpec
a= new ("hyperSpec", wavelength = wavenumber, spc = spc, data=d)
#sprawdz czy wszystko poszlo OK wywolaj obiekt hyperSpec
a
#ustawienie kolorow
col <- brewer.pal(8, "Set1")
pal <- colorRampPalette(col)
cols =pal(16)
plotspc(a[,,400~0],col = cols, wl.reverse =TRUE)

#wyciecie fragmentu od 320 do 280 cm-1
b = a [,, 270 ~ 220]
b
plotspc(b[,,270~220],col = cols, wl.reverse =TRUE)

# eksport danych z obiektu hyperSpec do macierzy
spc1 = t(b[[]])

#mean values of the columns
means=colMeans(spc1)
#standard deviation of the columns
sds=apply(spc1,2,sd)
#max and min values of the columns
max=apply(spc1,2,max)
min=apply(spc1,2,min)
#SNR peak-to-peak
SNRPP=means/(max-min)
#SNR as inverse of coefficient of variation
SNR=means/sds
#SNR root mean square - difference between the definition of variation and RMS sqrt((n-1)/n) 
N = nwl(b)
SNRRMS=means/(sds*sqrt((N-1)/N))



wynik1=data.frame(d,min, max, SNRPP, SNR, SNRRMS)
print(wynik1)


#fitting linear
k=data.frame(wavenumber,spc[1,])
colnames(k) <- c("X", "Y1")
model1 <- lm(Y1~X, data=k)
#diagnostic plots
plot.lm(model1)
#summary
summary(model1)
#plot data with fit
plot(k)
abline(lm(Y1~X), col="red") # regression line (y~x)

#quadratic function
model2 <- lm(Y1~X+I(X^2), data=k)
#diagnostic plots
plot.lm(model2)
#summary
summary(model2)
#plot data with fit
plot(k)
lines(X,model$fitted,lwd=2,col=3) #regression curve (y~x^2+x)
SNRPP2=mean(model2$fitted)/(max(model2$fitted)-min(model2$fitted))
RMS2=sqrt(deviance(model2)/(model2$df.residual+3))
SNRRMS2=mean(model2$fitted)/RMS2

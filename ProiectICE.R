setwd("C:/Users/Oana/OneDrive - Academia de Studii Economice din Bucuresti/Documents/ICE")
getwd()

# Proiect

# 1. Descrierea datelor si statistici descriptive

# Setul de date - Iris 
# Avem urmatoarele caracteristici despre florile iris si ne dorim sa prezicem clasa(specia) din care fac parte:
# SepalLengthCm = lungimea sepalelor in centimetri
# SepalWidthCm = latimea sepalelor in centimetri
# PetalLengthCm = lungimea petalelor in centimetri
# PetalWidthCm = latimea petalelor in centimetri
# Species = specia din care face parte fiecare floare, in functie de caracteristicile sepalelor si petalelor

# Incarcarea setului de date 
dateproiect <- read.csv("iris.csv", header=TRUE, row.names = 1)
View(dateproiect)
head(dateproiect)
# Tipul fiecarei variabile
str(dateproiect) # nu avem missing values
# Eliminarea variabilei categoriale
date1<-dateproiect[,-5]
summary(date1)
# Din summary, putem observa ca valorile pentru lungimea sepalelor sunt cuprinse intre 
# 4.3 cm (valoarea minima) si 7.9 cm (valoarea maxima), avand o medie de 5.8 cm.
# 25% dintre valorile lungimilor sepalelor sunt de cel putin 5.1 cm, 50% de cel putin 5.8 cm,
# iar 75% de cel putin 6.4 cm.

# Valorile pt latimile sepalelor sunt cuprinse intre 2 cm (valorea minima) si 4.4 cm (valoarea maxima),
# avand o medie de 3.05 cm. 25% dintre valori au o latime putin 2.8 cm, 50% de cel putin 3 cm,
# iar 75% de cel putin 3.3 cm.

# Valorile pt lungimile petalelor sunt cuprinse intre 1 cm (valoarea minima) si 6.9 cm (valoarea maxima),
# avand o medie de 3.8 cm. 25% dintre valori au o lungime de cel putin 1.6 cm, 50% de cel putin 4.3 cm,
# iar 75% de cel putin 55.1 cm.

# Valorile pt latimile petalelor sunt cuprinse intre 0.1 cm (valorea minima) si 2.5 cm (valoarea maxima),
# avand o medie de 1.12 cm. 25% dintre valori au o latime de cel putin 0.3 cm, 50% de cel putin 1.3 cm,
# iar 75% de cel putin 1.8 cm.


# 2.1. Clusterizare fuzzy cu functia "cmeans"

# Vom avea o clusterizare cu 3 clustere deoarece in setul de date exista 3 clase 
# care indica specia din care face parte fiecare floare (Setosa, Virginica sau Versicolor).
# Clusterizare cu 3 clustere, 100 de iteratii si m=2 gradul de fuzzificare.
# 1-virginica, 2- setosa, 3-versicolor
install.packages("e1071")
library(e1071)
set.seed(123)
result<-cmeans(date1, 3, 100, m=2, method="cmeans")
result
# In cazul algoritmului fuzzy c-means, centroizii sunt calculati ca mediile clusterelor.
# Clusterul 3 are lungimea medie a sepalelor (SepalLengthCm) 6.775 cm, urmat de clusterul 1 (5.889 cm)
# si clusterul 2 (5.003 cm).

# Clusterul 2 are latimea medie a sepalelor (SepalWidthCm) 3.403 cm, urmat de clusterul 3 (3.052 cm)
# si clusterul 1 (2.761 cm).

# Clusterul 3 are lungimea medie a petalelor (PetalLengthCm) 5.646 cm, urmat de clusterul 1 (4.364 cm)
# si clusterul 2 (1.484 cm).

# Clusterul 3 are latimea medie a petalelor (PetalWidthCm) 2.053 cm, urmat de clusterul 1 (1.397 cm)
# si clusterul 2 (0.251 cm).


# Gradele de apartenenta ale florilor la cele 3 clustere:
# Prima floare apartine clusterului 1 cu gradul 0.0025, clusterului 2 cu 0.9963 si clusterului 3 cu 0.0011.
# A doua floare apartine clusterului 1 cu 0.0158, clusterului 2 cu 0.9769 si clusterului 3 cu 0.0071, etc...

# Daca luam pe fiecare linie gradul maxim de apartenenta, decidem carui cluster apartine obiectul respectiv: 
# primele 50 obiecte apartin clusterului 2, obiectul 51 clusterului 3, obiectul 54 apartine clusterului 1, etc.


# Reprezentam observatiile intr-un sistem de axe cu valorile SepalLengthCm pe Ox si SepalWidthCm pe Oy.
par(mar=c(2,2,2,2))
plot(date1$SepalLengthCm, date1$SepalWidthCm,col=result$cluster)
points(result$centers[,c(1,4)],col=1:3,pch=8,cex=2)
text(x=date1$SepalLengthCm, y=date1$SepalWidthCm, labels=dateproiect$Species, col=result$cluster)
dev.off()
# Reprezentam observatiile intr-un sistem de axe cu valorile PetalLengthCm pe Ox si PetallWidthCm pe Oy.
plot(date1$PetalLengthCm, date1$PetalWidthCm,col=result$cluster)
points(result$centers[,c(1,4)],col=1:3,pch=8,cex=2)
text(x=date1$PetalLengthCm, y=date1$PetalWidthCm, labels=dateproiect$Species, col=result$cluster)
# Apar marcati cu * (pch=8) centroizii celor 3 clustere.
# cex=un nr. care indica marimea cu care textul si simbolurile afisate sunt mai mari decat cele implicite, pt. care cex=1). 
# De exemplu, daca cex=1.5, textul va fi scris cu 50% mai mare decat cel normal.
# Daca cex=0.5, textul va fi scris cu 50% mai mic decat cel normal. 

# Se ordoneaza crescator florile pe clustere.
o<-order(result$cluster)
o
data.frame(dateproiect$Species[o],result$cluster[o])

# Gradele de apartenenta la cele 3 clustere ale primelor 3 observatii
result$membership[1:3,]
data.frame(dateproiect$Species, result$cluster)

# 2.2. Clusterizare fuzzy cu biblioteca "cluster"
library(cluster)
max.overlaps=2000
# Se realizeaza o clusterizare fuzzy cu 3 clustere
res.fanny<-fanny(date1, 3)
data.frame(res.fanny$clustering, dateproiect$Species)
library(factoextra)
library(ggplot2)
fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE, palette = "jco", ggtheme = theme_minimal(), legend = "right")
options(ggrepel.max.overlaps = Inf)

# Pentru a evalua calitatea clusterizarii, determinam coeficientii silhouette:
fviz_silhouette(res.fanny, palette = "jco", ggtheme = theme_minimal())
# Determinam coef silhouette individual pt fiecare obiect:
res.fanny$silinfo
# O valoare apropiata de 0 indica o pozitionare a obiectului pe granita dintre 2 clustere vecine. 

# 3. Regresia logistica multinomiala
library(MASS)
# Transformam variabila raspuns “Species” in variabila factor, numita “SpeciesF"
dateproiect$SpeciesF<-factor(dateproiect$Species)
# Variabila "Species" ia 4 valori: "Iris-setosa", "Iris-virginica" si "Iris-versicolor"
library(nnet)
mymodel<-multinom(SpeciesF~SepalLengthCm+SepalWidthCm+PetalLengthCm+PetalWidthCm, data=dateproiect, trace=F)
summary(mymodel)
# Abaterea reziduala este eroarea ramasa in model - 11.899
# se pot determina ecuatiile urmatoare pentru determinarea probabilitatilor:
# ln(p(iris-setosa)/p(iris-versicolor))=-0.5480444 + 2.194888*SepalLengthCm + 6.173673*SepalWidthCm - 9.970511*PetalLengthCm - 5.093761*PetalWidthCm
exp(coef(mymodel))

predict(mymodel, dateproiect)
# primele 50 de flori fac parte din clasa Iris-setosa, urmatoarele 50 fac parte din clasa Iris-versicolor si urmatoarele 50 din clasa Iris-virginica
predict(mymodel, dateproiect, type="prob")
# Probabilitatea ca prima floare sa apartina clasei Iris-setosa este de 1%, sa apartina clasei Iris-versicolor este de 2.453955e-09,
# iar sa apartina clasei Iris-virginica este de 5.085012e-36 => prima floare apartine clasei Iris-setosa.
# Probabilitatea ca a doua floare sa apartina clasei Iris-setosa este de 9.999995e-01 (0.95), sa apartina clasei Iris-versicolor este de 5.384160e-07,
# iar sa apartina clasei Iris-virginica este de 5.085012e-36 => a doua floare apartine clasei Iris-setosa.
# Daca dorim realizarea unei predictii pt florile 18, 29, 110:
predict(mymodel, dateproiect[c(18, 29, 110),], type="prob")

# Vom compara predictiile modelului cu datele reale pentru primele 50 de observatii:
matriceconfuzie<-table(dateproiect$Species[1:50], predict(mymodel)[1:50])
matriceconfuzie
# Conform matricei de confuzie, cele 50 de observatii au fost corect clasificate in clasa Iris-setosa.
mean(dateproiect$Species[1:50]==predict(mymodel)[1:50])
# Rata de acuratete a clasificatorului pentru primele 50 de flori din setul de date este de 100%.

# 4. Arbori de decizie -  Arbori de clasificare
library(caret)
install.packages("caret")
library(lattice)
library(ggplot2)
install.packages("rpart.plot")
library(rpart.plot)
library(rpart)
table(dateproiect$Species)
set.seed(3033)
# Impartim setul de date in set de antrenare si set de testare
intrain<-createDataPartition(y=dateproiect$Species, p=0.7, list=F)
train<-dateproiect[intrain,]
test<-dateproiect[-intrain,]
dim(train)
dim(test)
# Clasificarea dupa Information Gain - alegerea nodurilor dupa valoarea Information Gain

trctrl<-trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(3333)
arbore_fit_info<-train(Species~.,data=train, method="rpart", 
                       parms=list(split="information"), 
                       trControl=trctrl, tuneLength=10)
prp(arbore_fit_info$finalModel, box.palette="Reds", tweak=1.2)
# Conform arborelui, daca lungimea petalelor este mai mica decat 2.6 cm, florile sunt de tipul Iris-setosa;
# daca nu se indeplineste aceasta conditie, se pune o alta conditie - daca latimea petalelor este mai mica decat 1.7 cm;
# daca se indeplineste aceasta conditie, florile sunt de tipul Iris-versicolor, iar daca nu, florile sunt de tipul Iris-virginica.

# Vom verifica rezultatul obtinut
test_pred_info<-predict(arbore_fit_info, newdata=test)
matriceconfuzie<-confusionMatrix(test_pred_info, as.factor(test$Species))
matriceconfuzie
# Clasificarea dupa Information Gain are o acuratete de 0.91

# Clasificarea dupa coeficientul Gini
par(mar=c(2,2,2,2))
set.seed(3333)
arbore_fit_gini<-train(Species~., data=train, method="rpart", 
                       parms=list(split="gini"),
                       trControl=trctrl, tuneLength=10)
prp(arbore_fit_gini$finalModel, box.palette = "Blues", tweak=1.2)
dev.off()
# Verificarea rezultatului obtinut
test_pred_gini<-predict(arbore_fit_gini, newdata=test)
confusionMatrix(test_pred_gini, as.factor(test$Species))
# Clasificarea dupa coeficientul Gini are rezultate bune, cu o acuratete de 0.93.

par(mar=c(2,2,2,2))
target=Species ~ SepalLengthCm + SepalWidthCm + PetalLengthCm + PetalWidthCm
arbore=rpart(target, data=train, method="class")
rpart.plot(arbore)
predictii<-predict(arbore, newdata=test)
predictii
confusionMatrix(predictii, as.factor(test$Species))

# Curatarea arborelui
# Dimensiunea unui arbore de decizie modifica adesea acuratetea predictiei;
# in general, un arbore mai mare inseamna o acuratete mai mare, dar daca arborele este prea mare,
# va depasi datele si va rezulta in acuratete scazuta => fenomenul de overfitting
# overfitting = arborele poate fi bun la a analiza datele de antrenament cu o acuratete mare,
# dar esueaza la a face predictii corect pe setul de testare

# Curatarea arborelui (pruning) este o tehnica utilizata in determinarea marimii copacului

arbore_1=rpart(target, train, control=rpart.control(minsplit=3))
arbore_2=rpart(target, train, control=rpart.control(minsplit=10))
par(mfcol=c(1,2))
rpart.plot(arbore_1, main="minsplit=3")
rpart.plot(arbore_2, main="minsplit=10")
dev.off()

# 5. KNN de clasificare
set.seed(1242)
library(class)
library(ggplot2)
library(GGally)

# Se normalizeaza datele
dateproiect[,1:4]<-scale(dateproiect[,1:4])
setosa<-rbind(dateproiect[dateproiect$Species=="Iris-setosa",])
versicolor<-rbind(dateproiect[dateproiect$Species=="Iris-versicolor",])
virginica<-rbind(dateproiect[dateproiect$Species=="Iris-virginica",])

ind<-sample(1:nrow(setosa),nrow(setosa)*0.8)
train_iris<-rbind(setosa[ind,], versicolor[ind,], virginica[ind,])
test_iris<-rbind(setosa[-ind,], versicolor[-ind,], virginica[-ind,])
dateproiect[,1:4]<-scale(dateproiect[,1:4])

# Cautam valoarea optima a lui K
error<-c()
for(i in 1:15)
{
  knn.fit<-knn(train=train_iris[,1:4], test=test_iris[,1:4], cl=train_iris$Species, k=i)
  error[i]=1-mean(knn.fit==test_iris$Species)
}

ggplot(data=data.frame(error), aes(x=1:15, y=error))+geom_line(color="Blue")

# Eroarea este minima atunci cand valoarea lui k=7

pred<-knn(train=train_iris[,1:4], test=test_iris[,1:4], cl=train_iris$Species, k=7)
table(test_iris$Species, pred)
confusionMatrix(pred, as.factor(test_iris$Species))
# Pentru k=7, avem o acuratete de 100% pe setul de testare.

# 6. Retele neuronale de clasificare

setosa<-dateproiect$Species=="Iris-setosa"
versicolor<-dateproiect$Species=="Iris-versicolor"
virginica<-dateproiect$Species=="Iris-virginica"
train_idx<-sample(x=nrow(dateproiect), size=nrow(dateproiect)*0.5)
train_iris<-dateproiect[train_idx,]
test_iris<-dateproiect[-train_idx,]
install.packages("neuralnet")
library(neuralnet)
net_iris<-neuralnet(Species ~ 
                      SepalLengthCm + SepalWidthCm + PetalLengthCm + PetalWidthCm, 
                    data=train_iris, hidden=c(10,10), rep = 5, err.fct = "ce", 
                    linear.output = F, lifesign = "minimal", stepmax = 1000000,
                    threshold = 0.001)
par(mar=c(2,2,2,2))
plot(net_iris, rep="best")
pred_iris<-compute(net_iris,test_iris[-5:-8] )
idx<-apply(pred_iris$net.result, 1, which.max)
predicted<-c('Iris-setosa', 'Iris-versicolor', 'Iris-virginica')[idx]
table(predicted, test_iris$Species)
pred_iris$net.result[1:10,] # gradele de apartenenta la fiecare clasa


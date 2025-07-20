rm(list = ls())

library(mosaicData)
library(car)
library("GGally")
library(effects)
library(relaimpo)
library(olsrr)
library(splines)
library(lmtest)
library(MASS)
library(ggcorrplot)

mydata=SaratogaHouses
#head(mydata)
#summary(mydata)
str(mydata)
set.seed(123)
ind = sample(1:nrow(SaratogaHouses), floor(0.8 * nrow(SaratogaHouses)), replace = FALSE)
train = SaratogaHouses[ind, ]
test = SaratogaHouses[-ind, ]

price = train[,1];lotSize = train[,2]; age = train[,3]; landValue = train[,4]; 
livingArea = train[,5]; pctCollege = train[,6]; bedrooms = train[,7]; 
fireplaces = train[,8]; bathrooms = train[,9]; rooms = train[,10]
waterfront = train[,14]; newConstruction = train[,15]; 
#faze 1
#koef determinace
m11 = lm(price~train[,2]); r11=summary(m11)$r.squared
m12 = lm(price~train[,3]); r12=summary(m12)$r.squared
m13 = lm(price~train[,4]); r13=summary(m13)$r.squared
m14 = lm(price~train[,5]); r14=summary(m14)$r.squared
m15 = lm(price~train[,6]); r15=summary(m15)$r.squared
m16 = lm(price~train[,7]); r16=summary(m16)$r.squared
m17 = lm(price~train[,8]); r17=summary(m17)$r.squared
m18 = lm(price~train[,9]); r18=summary(m18)$r.squared
m19 = lm(price~train[,10]); r19=summary(m19)$r.squared

determinace = data.frame(r11,r12,r13,r14,r15,r16,r17,r18,r19); determinace
which.max(determinace)
max(determinace)
#nejvyssi ma tedy promenna v 5. sloupci, tj. livingArea
#
#
#
#
#
#
#
#
#
#
#korel koef
(c1 = cor(price, train[,2])^2)
c2 = cor(price, train[,3])^2
c3 = cor(price, train[,4])^2
c4 = cor(price, train[,5])^2
c5 = cor(price, train[,6])^2
c6 = cor(price, train[,7])^2
c7 = cor(price, train[,8])^2
c8 = cor(price, train[,9])^2
c9 = cor(price, train[,10])^2
(korelace = data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9))
which.max(korelace)
max(korelace)
#stejny vysledek :) nejv kvadrat vyber korel koeficiantu je opet s promennou livingArea




#faze 2
summary(m16) #price~berooms - cim vic loznic, tim vyssi cena za byt
m21 = lm(price~ bedrooms + livingArea); summary(m21) #nyni je u promenne bedrooms na rozdil od modelu m16 zaporne znamenko
#mozny duvod - samozrejme obecne plati, ze cim vic pokoju, tim lip, ale to hlavne proto, ze predpokladame, ze cim vic pokoju, tim vetsi byt
# nyni ale beru v potaz i udaje o rozloze bytu - PRI STEJNE ROZLOZE JE NAOPAK LEPSI MIT MENE VELKYCH POKOJU, NEZ HODNE MALYCH!

scatterplotMatrix(~ price +  bedrooms + livingArea, id=list (n=3))
cor(train[,c(1,5,7)])
#eff1 = Effect(c("bedrooms", "livingArea"), m21, xlevels = list(bedrooms = seq(1,7, by = 1), livingArea = seq(600, 5400, by = 600)))
#plot(eff1, x.var = "livingArea", lines = list(multiline = TRUE))
eff2 = Effect(c("bedrooms", "livingArea"), m21, xlevels = list(livingArea = seq(600, 5400, by = 900), bedrooms = seq(1,7,by = 1))) 
plot(eff2, x.var = "bedrooms", lines = list(multiline = TRUE))
# ad 1) Interaktivní 3D graf
scatter3d(price ~  bedrooms + livingArea, surface = FALSE)


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#



#faze 3 - model kvantitativnich promennych
model_kvanti = lm(price~ lotSize + age + landValue + livingArea
                  + pctCollege + bedrooms + fireplaces + bathrooms + rooms)
summary(model_kvanti)

# Vyrovnané hodnoty 
vyrovnane=fitted(model_kvanti)
boxplot(vyrovnane~train$waterfront)
aggregate(vyrovnane, list(train$waterfront), FUN=mean)

#rezidua:
rezidua = resid(model_kvanti)
aggregate(rezidua, list(train$waterfront), FUN=mean)
boxplot(rezidua~train$waterfront)
#rezidua pro Yes jsou naadhodnocena, pro No naopak podhodnocena, takze model_kvanti 
  #pro yes podhodnocuje a pro No nadhodnocuje


#
#
#
#
#faze 4 model se vsemi promennymi
#musime se nepreve podivat na dummy promenne pro ty kateg. prom.
contrasts(train[,11]) # kódování užitím dummy proměnných
#contrasts(train[,12])
contrasts(train[,13])
contrasts(train[,14])
contrasts(train[,15])
contrasts(train[,16])
#referencni hodnota je vzdy ta prvni 

# Model se zařazením kategoriální proměnné
model_all = lm(price ~ lotSize + age + landValue + livingArea + pctCollege + bedrooms + fireplaces + bathrooms + rooms + heating + fuel + sewer + waterfront + newConstruction+centralAir, data = train)
coef(summary(model_all))[19,]
#zjisteni referencni hodnoty promenne centralAir:
contrasts(train[,16]) #referencni hodnota je Yes

# interpretace odhadu regresního parametru u dummy promenné reprezentující kategoriální prom. centralAir:
# tj posledni parametr v modelu
# pri jinak stejnych podminkach se odhad stredni hodnoty ceny bytu o 10440 nizsi pro byt bez klimatizace
  #oproti bytu s klimatizaci

klima_yes_data = subset(train, train$centralAir=="Yes")
model_all_klima = lm(price ~ lotSize + age + landValue + livingArea + pctCollege + bedrooms 
                     + fireplaces + bathrooms + rooms + heating + fuel + sewer + waterfront
                     + newConstruction, data=klima_yes_data)
#summary(model_all_klima)

klima_no_data = subset(train, train$centralAir=="No")
model_all_No_klima = lm(price ~ lotSize + age + landValue + livingArea + pctCollege + bedrooms 
                        + fireplaces + bathrooms + rooms + heating + fuel + sewer + waterfront
                        + newConstruction, data=klima_no_data)
#summary(model_all_No_klima)

compareCoefs(model_all_klima,model_all_No_klima)

compareCoefs(model_all, model_all_klima)
compareCoefs(model_all, model_all_No_klima)


#faze5
model_vlastni= lm(price~ poly(landValue, degree = 2, raw = TRUE)  + livingArea + newConstruction + bedrooms + livingArea:bedrooms, data = train)
summary(model_vlastni)

model_bezInt = lm(price~ poly(landValue, degree = 2, raw = TRUE)  + livingArea + newConstruction + bedrooms, data = train)
anova(model_vlastni, model_bezInt)
#nezamitame H0: odhad bety u interakce=0 -> nezam, ze je tato interakce v modelu stat. nevyznamna!

model_lin = lm(price~ landValue + livingArea + newConstruction + bedrooms + livingArea:bedrooms, data = train)
anova(model_vlastni, model_lin) #zde je velmi tesne zamitame H0
summary(model_bezInt)

#
#
#
#
#
#
#
#
#
#faze 6
#prvni zpusob:
anova(model_vlastni)
#jiny zpusob rozkladu:
anova(lm(price~ newConstruction  + bedrooms + livingArea + livingArea:bedrooms 
         + poly(landValue, degree = 2, raw = TRUE), data = train))
#dalsi moznost rozkladu:
anova(lm(price~ newConstruction + livingArea  + bedrooms + livingArea:bedrooms 
         + poly(landValue, degree = 2, raw = TRUE), data = train))

#toto nejde pro polynomicky model vzhledem k silne linearni zavislosti v regresni matici 
# ad 3) Průměrné příspěvky přes všechny kombinace
(relI = calc.relimp(model_lin, type = "lmg")) # metrika lmg pro index determinace
plot(relI) #opet vidim, ze z tohoto pohledu jsou nejmene prinosnymi promennymi newContruction, a hlavne interakce liv:bedrooms
#prilis se to ale neshoduje s vysledky stat analyzy


#faze 7
pairs(train[,c(1:10)])
par("mfrow"=c(2,3))
boxplot(price~newConstruction, data = train) #jo?
boxplot(price~train$heating, data = train)
boxplot(price~train$fuel, data = train) 
boxplot(price~train$sewer, data = train)
boxplot(price~train$waterfront, data = train)#jo
boxplot(price~train$centralAir, data = train)
corr=cor(data.frame(price,rooms,bedrooms,bathrooms,livingArea,age, lotSize, fireplaces, landValue,pctCollege, as.numeric(newConstruction), as.numeric(waterfront)))
ggcorrplot(corr,lab=TRUE)
#neuspesne modely bez transformace - heteroskedasticita
ols_step_backward_p(model_all)
ols_step_both_aic(model_all)
ols_step_backward_p(model_vlastni)
ols_step_both_aic(model_vlastni)
model_all2 = lm(price ~ (lotSize + landValue + livingArea  + bathrooms + rooms + waterfront + newConstruction)^2, data = train)
model_all3 = lm(price ~ (landValue + livingArea  + bathrooms + rooms + waterfront + newConstruction)^2, data = train)
model_all4 = lm(price ~ (landValue + livingArea  + rooms + waterfront + newConstruction)^2, data = train)
model_all5 = lm(price ~ (landValue + livingArea  + rooms + waterfront + newConstruction) +livingArea:rooms + livingArea:waterfront + rooms:waterfront  + landValue:newConstruction  , data = train)
model_all6 = lm(price ~ (landValue + livingArea  + rooms + waterfront + newConstruction) + livingArea:waterfront  + landValue:newConstruction  , data = train)
model_all7 = lm(price ~ (landValue + livingArea  + rooms + waterfront + newConstruction) + I(rooms^2)+ I(livingArea^2) + I(landValue^2) + livingArea:waterfront  + landValue:newConstruction  , data = train)
model_all8 = lm(price ~ (landValue + livingArea  + rooms + waterfront + newConstruction) + I(landValue^2) + livingArea:waterfront  + landValue:newConstruction  , data = train)
ols_step_both_p(model_all8)


model_all17 = lm(price ~ (landValue + livingArea + waterfront) + livingArea:waterfront, data = train)
summary(model_all17) 

model_all20 = lm(price ~ (landValue + livingArea + waterfront + bedrooms)  , data = train)
summary(model_all20) #ANO
(relI = calc.relimp(model_all20, type = "lmg"))
model_all21 = lm(price ~ (landValue + livingArea + waterfront + bedrooms)  + I(landValue^2) , data = train)
summary(model_all21) #ANO - ALE r2 STOUPLO JEN MIRNE
anova(model_all20, model_all21)#zamitame H0 :)

model_all22 = lm(price ~ (landValue + livingArea + waterfront + bedrooms) + landValue:livingArea + I(landValue^2) , data = train)
summary(model_all22) #NE
model_all23 = lm(price ~ (landValue + livingArea + waterfront + bedrooms) + landValue:livingArea , data = train)
summary(model_all23) #ANO - r2 vychazi lepsi nez u 2. mocniny
model_all24 = lm(price ~ (landValue + livingArea + waterfront + bedrooms) +I(livingArea^2) + landValue:livingArea , data = train)
summary(model_all24) #ANO -  opet trochu lepsi r2

anova(model_all20, model_all23)#zamitame H0 :)
anova(model_all24, model_all23)#zamitame H0 :)
anova(model_all23)

#spliny:
fit6 = lm(price ~ bs(livingArea, degree = 1, df = 2 +1) + bedrooms + landValue + waterfront, data = train)
summary(fit6)#ANO
adjr2_f6 = summary(fit6)$adj.r.squared
BIC(fit6)
fit7 = lm(price ~ bs(landValue, degree = 1, df = 1 +1) + bedrooms + livingArea + waterfront, data = train)
summary(fit7)#ANO
adjr2_f7 = summary(fit7)$adj.r.squared
BIC(fit7)
fit8 = lm(price ~ bs(livingArea, degree = 1, df = 2 +1) + bedrooms + landValue + waterfront+ landValue:livingArea, data = train)
summary(fit8)
adjr2_f8 = summary(fit8)$adj.r.squared
BIC(fit8)

#z techto modelu volim napr. model 23 (idealni pomer R^2, BIC a poctu parametru 
  #vzhledem k ostatnim modelum)
#tento model nyni otestuji
# 1) Vnějšně studentizovaná rezidua a nekorektně specifikovaný model
residualPlots(fit8, tests = TRUE, id = TRUE, type = "rstudent") 
#grafy celkem ok
#testy: Tukey ok (p.h. 0,48), ale mela bych zkusit bedrooms^2!
dev.new()
residualPlots(model_all23, tests = TRUE, id = TRUE, type = "rstudent") 
#grafy o malinko horsi nez f8
#testy: tukey sice nezam H0, ale p.h. jen 0,069 -> skoro rika, ze bychom meli neco pridat (hl.livArea a bedrooms)

# 2) Goldfeld-Quandtův test
gqtest(model_all23, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ livingArea, data = train) #zam H0: konst rezidua
gqtest(model_all23, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ landValue, data = train) #zam H0: konst rezidua

# 3) Glejserův test
fit.d = lm(abs(resid(model_all23)) ~ (landValue + livingArea + waterfront + bedrooms) + landValue:livingArea, data = train)
#abs. hodnota rezidui se meni -> heterosk.
summary(fit.d) #zam H0: konst rezidua


#TRANSFORMOVANA PRICE
f = function(modell) {
  plot(abs(resid(modell)) ~ fitted(modell))
  abline(lm(abs(resid(modell)) ~ fitted(modell), data = train), col="red")
  
}

pctColl.cat <- cut(pctCollege, breaks = c(20,35,82),include.lowest = TRUE)

model_log = lm(log(price) ~ ns(landValue, df = 3)  + livingArea , data = train)
summary(model_log)
residualPlots(model_log, tests = TRUE, id = TRUE, type = "rstudent")
f(model_log)

dev.new()#OK, R2=57,81%
model_log1 = lm(log(price) ~ ns(landValue, df = 3) + ns(livingArea, df=3) +bathrooms , data = train)
summary(model_log1)
residualPlots(model_log1, tests = TRUE, id = TRUE, type = "rstudent")
f(model_log1)

dev.new()#ok, R2 = 56,48%
model_log2 = lm(log(price) ~ ns(landValue, df = 3) + ns(livingArea, df=3) , data = train)
summary(model_log2)
residualPlots(model_log2, tests = TRUE, id = TRUE, type = "rstudent")
f(model_log2)

dev.new()
model_log3 = lm(log(price) ~  landValue + ns(livingArea, df=3) + lotSize +waterfront + pctColl.cat+newConstruction+bathrooms, data = train)
summary(model_log3)
(relI = calc.relimp(model_log3, type = "lmg"))
residualPlots(model_log3, tests = TRUE, id = TRUE, type = "rstudent")
f(model_log3)

dev.new()
model_log4 = lm(log(price) ~   landValue + ns(livingArea, df=3)  +waterfront +bathrooms , data = train)
summary(model_log4)
(relI = calc.relimp(model_log4, type = "lmg"))
residualPlots(model_log4, tests = TRUE, id = TRUE, type = "rstudent")
f(model_log4)

dev.new()#ne, R2=59,7%, ale zbytecne moc parametru
model_log5 = lm(log(price) ~   poly(landValue, degree=3)+ ns(livingArea, df=3)+waterfront + pctColl.cat+newConstruction +bathrooms , data = train)
summary(model_log5)
(relI = calc.relimp(model_log5, type = "lmg"))
residualPlots(model_log5, tests = TRUE, id = TRUE, type = "rstudent")
f(model_log5)

dev.new()#OK, R2=59,12%
model_log6 = lm(log(price) ~   poly(landValue, degree=3)+ ns(livingArea, df=3)+waterfront  +bathrooms , data = train)
summary(model_log6)
(relI = calc.relimp(model_log6, type = "lmg"))
residualPlots(model_log6, tests = TRUE, id = TRUE, type = "rstudent")
f(model_log6)

dev.new()#OK, R2=57,8%
model_log7 = lm(log(price) ~ ns(landValue, df = 3) + ns(livingArea, df=2) +bathrooms , data = train)
summary(model_log7)
residualPlots(model_log7, tests = TRUE, id = TRUE, type = "rstudent")
f(model_log7)

dev.new()#OK, R2 = 58,87%, mohl by byt i polynom degree=3 -> R2=59,12%
model_log8 = lm(log(price) ~ poly(landValue, degree=2)+ ns(livingArea, df=2)+waterfront  +bathrooms  , data = train)
summary(model_log8)
residualPlots(model_log8, tests = TRUE, id = TRUE, type = "rstudent")
f(model_log8)

dev.new()#OK, R2=56,44%
model_log9 = lm(log(price) ~ ns(landValue, df = 3) + ns(livingArea, df=2) , data = train)
summary(model_log9)
residualPlots(model_log9, tests = TRUE, id = TRUE, type = "rstudent")
f(model_log9)

dev.new()
plot(log(price) ~ livingArea);lines(lowess(log(price)~livingArea), col="red")
plot(log(price) ~ landValue);lines(lowess(log(price)~landValue), col="red")
boxplot(log(price)~waterfront)





# ad d) Goldfeld-Quandtův test - vse ok :)
gqtest(model_log8, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ landValue, data = train)
gqtest(model_log8, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ livingArea, data = train)
gqtest(model_log8, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ waterfront, data = train)
gqtest(model_log8, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ bathrooms, data = train)

gqtest(model_log7, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ landValue, data = train)
gqtest(model_log7, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ livingArea, data = train)
gqtest(model_log7, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ bathrooms, data = train)

gqtest(model_log9, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ landValue, data = train)
gqtest(model_log9, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ livingArea, data = train)

# ad e) Glejserův test - ok
fit.d = lm(abs(resid(model_log7)) ~ ns(landValue, df = 3) + ns(livingArea, df=2) +bathrooms, data = train)
summary(fit.d)

fit.d = lm(abs(resid(model_log9)) ~ ns(landValue, df = 3) + ns(livingArea, df=2), data = train)
summary(fit.d)

fit.d = lm(abs(resid(model_log8)) ~ poly(landValue, degree=2)+ ns(livingArea, df=2)+waterfront  +bathrooms , data = train)
summary(fit.d)
#TENTO TEST VYCHAZI OK PRO MODEL log7 A log8 A log9, TY JSOU PROTO NEJLEPSI!
anova(model_log8, lm(log(price) ~ poly(landValue, degree=3)+ ns(livingArea, df=2) +bathrooms+waterfront, data = train)) 
anova(model_log8, lm(log(price) ~ poly(landValue, degree=2)+ ns(livingArea, df=2) +bathrooms, data = train)) #VIDIM, ZE WATERFRONT TAM PATRI -> 
# -> NEJLEPSI JE ASI MODEL log8 :) ... ALE ZASE O 2 PARAMETRY VIC
BIC(model_log9)
BIC(model_log8)
BIC(model_log7)
#BIC NEJNIZSI PRO m8 ->m7 ->m9




#ODLEHLA POZOROVANI - DIVAM SE NA P-H. U BONF KOREKCE!
# H0: v souboru není odlehlé pozorování z hlediska regresního modelu
outlierTest(model_log9)
outlierTest(model_log8)
outlierTest(model_log7)
# VE VSECH PRIPADECH JSOU URCITE ODLEHLA TATO POZOROVANI:
# c. 1011, 891, 459, 1195, 1279

# Vlivná pozorování: Cookova vzdálenost
sort(cooks.distance(model_log9), decreasing = TRUE) # nejv. ma D=1,359 = C.702
sort(cooks.distance(model_log8), decreasing = TRUE) # nejv. ma D=1,818 = C.702
sort(cooks.distance(model_log7), decreasing = TRUE) # nejv. ma D=0,998 = C.702
#ODLEHLA POZOROVANI MAJI NEJSPIS VYSOHOU HODNOTU RSTUDENT, ALE VEEELMI NIZKE HII 
  #-> NEVERMIND, ALE PRAVY OPAK JE C.702: SKORO NULOVE STD RES, ALE VEEEELKE HII!
plot(model_log7, which = 5)
# Graf více ukazatelů
influenceIndexPlot(model_log8, id=list (n=6))

# Vynechání atypického pozorování
fit2 = update(model_log8, subset = rownames(train) != "702")
summary(fit2) #R2 = 58,89%
compareCoefs(model_log8, fit2)
# o moc lepsi std. erro neni, spis naopak -> neni nutne vynechavat
residualPlots(fit2, tests = TRUE, id = TRUE, type = "rstudent")
f(fit2)
gqtest(fit2, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ landValue, data = train[c(rownames(train) != "702"),])
gqtest(fit2, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ livingArea, data = train[c(rownames(train) != "702"),])
gqtest(fit2, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ waterfront, data = train[c(rownames(train) != "702"),])
gqtest(fit2, point = 0.5, fraction = 0.33, alternative = "greater",
       order.by = ~ bathrooms, data = train[c(rownames(train) != "702"),])
BIC(fit2) # :)

#MULTIKOLINEARITA: OK :)
vif(model_log8)
vif(model_log9)
vif(model_log7)
vif(fit2)




# Testovací datový soubor
chyby =log(test$price) - predict(model_log8, test) # chyby předpovědi
hist(chyby)
abline(v=0, col="red")

plot(predict(model_log8, test) ~ log(test$price),  xlab = "skutecnost",
     ylab = "predikce", col = "blue", cex = 1.5); abline(a = 0, b = 1,
                                                         col = "red")

(ME = mean(chyby))
(MSE = mean(chyby^2))
sqrt(MSE)






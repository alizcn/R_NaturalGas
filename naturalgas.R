dogalgaz=read.csv(file="C:/Users/Ali/Desktop/Eğitim/_Yükseklisans/1.Dönem/R ile Programlama/RFinal2/dogalgaz.csv")
dogalgaz

head(dogalgaz)

class(dogalgaz)
class(dogalgaz$state)
class(dogalgaz$statecode)
class(dogalgaz$year)
class(dogalgaz$consumption)
class(dogalgaz$price)
class(dogalgaz$eprice)
class(dogalgaz$oprice)
class(dogalgaz$lprice)
class(dogalgaz$pleasure)
class(dogalgaz$income)


mean(dogalgaz$statecode)
mean(dogalgaz$year)
mean(dogalgaz$consumption)
mean(dogalgaz$price)
mean(dogalgaz$eprice)
mean(dogalgaz$oprice)
mean(dogalgaz$lprice)
mean(dogalgaz$income)

median(dogalgaz$statecode)
median(dogalgaz$year)
median(dogalgaz$consumption)
median(dogalgaz$price)
median(dogalgaz$eprice)
median(dogalgaz$oprice)
median(dogalgaz$lprice)
median(dogalgaz$income)

sd(dogalgaz$statecode)
sd(dogalgaz$year)
sd(dogalgaz$consumption)
sd(dogalgaz$price)
sd(dogalgaz$eprice)
sd(dogalgaz$oprice)
sd(dogalgaz$lprice)
sd(dogalgaz$income)

summary(dogalgaz)



shapiro.test(dogalgaz$statecode)

boxplot(dogalgaz$statecode)

hist(dogalgaz$statecode,freq = FALSE,main = "?ehir da??l?mlar?",xlab = "?ehirler",ylab = "Y?zde",col = c("blue","red"))

lines(density(dogalgaz$statecode),col="black",lwd=4)
box(which = "plot")

qqnorm(dogalgaz$statecode)
qqline(dogalgaz$statecode)



shapiro.test(dogalgaz$year)

boxplot(dogalgaz$year)

hist(dogalgaz$year,freq = FALSE,main = "Y?llara G?re Da??l?m",xlab = "Y?llar",ylab = "Y?zde",col = c("blue","red"))

lines(density(dogalgaz$year),col="black",lwd=4)
box(which = "plot")

qqnorm(dogalgaz$year)
qqline(dogalgaz$year)


shapiro.test(dogalgaz$consumption)

boxplot(dogalgaz$consumption)

hist(dogalgaz$consumption,freq = FALSE,main = "T?ketim Da??l?mlar?",xlab = "T?ketim",ylab = "Y?zde",col = c("blue","red"))

lines(density(dogalgaz$consumption),col="black",lwd=4)
box(which = "plot")

qqnorm(dogalgaz$consumption)
qqline(dogalgaz$consumption)


shapiro.test(dogalgaz$price)

boxplot(dogalgaz$price)

hist(dogalgaz$price,freq = FALSE,main = "Fiyat Da??l?m?",xlab = "Fiyat",ylab = "Y?zde",col = c("blue","red"))

lines(density(dogalgaz$price),col="black",lwd=4)
box(which = "plot")

qqnorm(dogalgaz$price)
qqline(dogalgaz$price)


shapiro.test(dogalgaz$eprice)

boxplot(dogalgaz$eprice)

hist(dogalgaz$eprice,freq = FALSE,main = "Elektrik Fiyat Da??l?m?",xlab = "Elektrik Fiyat?",ylab = "Y?zde",col = c("blue","red"))

lines(density(dogalgaz$eprice),col="black",lwd=4)
box(which = "plot")

qqnorm(dogalgaz$eprice)
qqline(dogalgaz$eprice)


shapiro.test(dogalgaz$oprice)

boxplot(dogalgaz$oprice)

hist(dogalgaz$oprice,freq = FALSE,main = "Dam?t?lm?? Yak?t Fiyat Da??l?m",xlab = "Dam?t?lm?? Yak?t Fiyat?",ylab = "Y?zde",col = c("blue","red"))

lines(density(dogalgaz$oprice),col="black",lwd=4)
box(which = "plot")

qqnorm(dogalgaz$oprice)
qqline(dogalgaz$oprice)




shapiro.test(dogalgaz$lprice)

boxplot(dogalgaz$lprice)

hist(dogalgaz$lprice,freq = FALSE,main = "Likit Yak?t Fiyat Da??l?m?",xlab = "Likit Yak?t Fiyat?",ylab = "Y?zde",col = c("blue","red"))

lines(density(dogalgaz$lprice),col="black",lwd=4)
box(which = "plot")

qqnorm(dogalgaz$lprice)
qqline(dogalgaz$lprice)



shapiro.test(dogalgaz$income)

boxplot(dogalgaz$income)

hist(dogalgaz$income,freq = FALSE,main = "Gelir Da??l?m?",xlab = "Gelir",ylab = "Y?zde",col = c("blue","red"))

lines(density(dogalgaz$income),col="black",lwd=4)
box(which = "plot")

qqnorm(dogalgaz$income)
qqline(dogalgaz$income)






pie(table(dogalgaz$pleasure),main = "Memnuniyet Da??l?m?", xlab = "Memnuniyet"
    ,ylab = "",col = c("red","blue"))

pie(table(dogalgaz$state),main = "?ehir Da??l?m?", xlab = "?ehirler"
    ,ylab = "",col = c("red","blue"))

barplot(table(dogalgaz$pleasure),main = "Memnuniyet Da??l?m?"
        , xlab = "Memnuniyet",ylab = "Say?",names.arg = c("Evet","Hay?r"),col = c("red","blue"))

barplot(table(dogalgaz$state),main = "?ehir Da??l?m?"
        , xlab = "?ehirler",ylab = "Say?",col = c("red","blue"))



plot(dogalgaz$price~dogalgaz$oprice,xlab = "Do?algaz Fiyat?",ylab = "Dam?t?lm?? Akaryak?t Fiyat?",col=c("blue","red"))


plot(dogalgaz$eprice~dogalgaz$lprice,xlab = "Elektrik Fiyat?",ylab = "S?v?la?t?r?lm?? Akaryak?t Fiyat?",col=c("blue","red"))



bartlett.test (dogalgaz$price ~ dogalgaz$state)
bartlett.test (dogalgaz$eprice ~ dogalgaz$pleasure)
bartlett.test (dogalgaz$oprice ~ dogalgaz$statecode)
bartlett.test (dogalgaz$lprice ~ dogalgaz$pleasure)


t.test (dogalgaz$price ~ dogalgaz$pleasure)
t.test (dogalgaz$eprice ~ dogalgaz$pleasure)
t.test (dogalgaz$oprice ~ dogalgaz$pleasure)
t.test (dogalgaz$lprice ~ dogalgaz$pleasure)




t.test(dogalgaz$price,dogalgaz$statecode, paired = TRUE, alternative = "two.sided")
t.test(dogalgaz$eprice,dogalgaz$year, paired = TRUE, alternative = "two.sided")
t.test(dogalgaz$oprice,dogalgaz$statecode, paired = TRUE, alternative = "two.sided")
t.test(dogalgaz$lprice,dogalgaz$year, paired = TRUE, alternative = "two.sided")


anovtest1=aov(dogalgaz$price ~ dogalgaz$state)
anovtest1
summary(anovtest1)

TukeyHSD (anovtest1, conf.level = 0.90)



anovtest2=aov(dogalgaz$eprice ~ dogalgaz$pleasure)
anovtest2
summary(anovtest2)

anov2tuk=TukeyHSD (anovtest2)
anov2tuk





regresyontest1= lm(dogalgaz$lprice ~ dogalgaz$pleasure)
summary(regresyontest1)

plot(regresyontest1)










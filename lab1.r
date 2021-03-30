library(MASS)
MASS::painters
composition = painters$Composition
composition.freq = table(composition) #szereg rodzielczy
cbind(composition.freq)
hist(composition)

pie(composition.freq)

data.frame(faithful)
waiting = faithful$waiting
breaks = seq(40,100, by= 10)
waiting.cut = cut(waiting, breaks, right = FALSE)
waiting.freq = table(waiting.cut)
waiting.cumfreq = cumsum(waiting.freq)
waiting.cumfreq
cfreq = c(0, cumsum(waiting.freq))
plot(breaks, cfreq, main="Eruption waiting periods", xlab = " The lenght of waiting periods" , ylab = "Cumulative eruptions" )
lines(breaks,cfreq)

###NWM CZY TO MA BYÆ
eruptions = faithful$eruptions 
breaks = seq(1.5, 5.5, by=0.5) 
eruptions.cut = cut(waiting, breaks, right=FALSE) 
eruptions.freq = table(waiting.cut) 
eruptions.cumfreq = cumsum(waiting.freq) 
eruptions.cumrelfreq = waiting.cumfreq / nrow(faithful)

cumrelfreq0 = c(0, eruptions.cumrelfreq) 
plot(breaks, cumrelfreq0, main="Old Faithful Eruptions",  # main title 
         xlab="Duration minutes", 
         ylab="Cumulative eruption proportion") 
lines(breaks, cumrelfreq0)        
####
#MEAN
waiting2 = faithful$waiting     
mean(waiting2) 
#MEDIAN
waiting3 = faithful$waiting     
median(waiting3)
#QUARTILE
waiting4 = faithful$waiting     
quantile(waiting4)

waiting5 = faithful$waiting     
quantile(waiting5,c(.17,.43,.67,.85))
#Range
waiting6 = faithful$waiting    
max(waiting6) - min(waiting6)
#Interquartile Range
waiting7 = faithful$waiting    
IQR(waiting7)
#Variance
waiting8= faithful$waiting
var(waiting8)
#Standard_Deciation
waiting9= faithful$waiting
sd(waiting9)
#Covariance
duration=faithful$eruptions
waiting10= faithful$waiting
cov(duration, waiting10)
#Correlation Coeffient
duration1=faithful$eruptions
waiting11= faithful$waiting
cor(duration1, waiting11)
#centarl_moment
install.packages("e1071", dependencies = TRUE)

library(e1071)                    # load e1071 
waiting12 = faithful$waiting   
moment(waiting12, order=3, center=TRUE) 
#skewness
waiting13 = faithful$waiting     # eruption durations 
skewness(waiting13)
#kurtosis
waiting14 = faithful$waiting     # eruption durations 
kurtosis(waiting14)
#BOX_PLOT
waiting2 = faithful$waiting      
boxplot(waiting2, horizontal=TRUE)  


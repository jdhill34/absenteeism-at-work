library(ROCR)
library(pROC)
library(ggplot2)

library(quantreg)
pre_absent = read.table(file.choose(), header = TRUE, sep=';')
absent=pre_absent
#nrow(pre_absent[(pre_absent$Absenteeism.time.in.hours <= 8),])
#absent = pre_absent[(pre_absent$Absenteeism.time.in.hours <= 8),]
yuh = pre_absent[(pre_absent$Absenteeism.time.in.hours <= 8),]
yuh2 = pre_absent[(pre_absent$Reason.for.absence!=0),]

absent = absent[(absent$Reason.for.absence!=0),]
#absent = absent[(absent$Reason.for.absence!=2),]
#absent = absent[(absent$Reason.for.absence!=3),]
#absent = absent[(absent$Reason.for.absence!=4),]
#absent = absent[(absent$Reason.for.absence!=5),]




split = .5
absent["randomDecider"] = runif(nrow(absent)) #gives a random value to each row in absent, for us to randomize on


ggplot(data=absent, aes(x=Reason.for.absence, y=Absenteeism.time.in.hours)) + geom_point()

train <- absent[(absent$randomDecider <= split), ]
test <- absent[(absent$randomDecider > split), ]


quant = rq(Absenteeism.time.in.hours~Reason.for.absence+Age+Day.of.the.week+Weight+Height+Body.mass.index, data = train)
summary(quant)


plot(Absenteeism.time.in.hours~Reason.for.absence, data=train)
abline(lm(Absenteeism.time.in.hours~Reason.for.absence, data=train), col="red")
abline(rq(Absenteeism.time.in.hours~Reason.for.absence, data=train, tau=.25), col="purple")
abline(rq(Absenteeism.time.in.hours~Reason.for.absence, data=train, tau=.5), col="blue")


rqP = predict(quant, test)

summary(absent$Education)



par(adj = .5)
plot(x = test$Reason.for.absence, y = rqP, col="blue", xlab = "Reason for Absence", ylab = "Hours Missing", main = "Hours Missing ~ Reason for Absence")

par(adj =1)
title(sub="    Predicted Values in Blue \n Actual Values in Red")
par(adj = .5)
points(x = test$Reason.for.absence, y = test$Absenteeism.time.in.hours, col="red")


ggplot(data=train, aes(quant$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Quantile Regression Residuals")

sum(rqP <= 8)


plottingData = data.frame(test)
plottingData$pred = rqP
plottingData$TPraw = ifelse(plottingData$pred<=8 & plottingData$Absenteeism.time.in.hours<=8, 1, 0) #TEST LESS THAN 8 RESULT LESS THAN 8
plottingData$FPraw = ifelse(plottingData$pred<=8 & plottingData$Absenteeism.time.in.hours>8, 1, 0) #TEST LESS THAN 8 GREATER THAN 8
plottingData$FNraw = ifelse(plottingData$pred>8 & plottingData$Absenteeism.time.in.hours<=8, 1, 0) #TEST GREATER THAN 8 RESULT LESS THAN 8
plottingData$TNraw = ifelse(plottingData$pred>8 & plottingData$Absenteeism.time.in.hours>8, 1, 0) #TEST GREATER THAN 8 RESULT GREATER THAN 8
#plottingData = plottingData[c(1:21)]
TP = sum(plottingData$TPraw)
FP = sum(plottingData$FPraw)
FN = sum(plottingData$FNraw)
TN = sum(plottingData$TNraw)

a = data.frame(c(TP, FP), c(TN, FN))
a

ctable <- as.table(matrix(c(TP, FN, TN, FP), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")


#.971 .916 .978
sensitivity = TP/(TP+FN)

accuracy = (TP+TN)/(TP+FN+FP+TN)

#.05 .27 .08
specificty = TN/(TN+FP)

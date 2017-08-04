icu <- read.csv(file = "C:/DATA630/icu.csv", sep = ",", head = TRUE)

#data preperation
icu$ID <- NULL
summary(icu)
icu$STA <- factor(icu$STA)
icu$GENDER <- factor(icu$GENDER)
icu$RACE <- factor(icu$RACE)
icu$SER <- factor(icu$SER)
icu$CAN <- factor(icu$CAN)
icu$CRN <- factor(icu$CRN)
icu$INF <- factor(icu$INF)
icu$CPR <- factor(icu$CPR)
icu$PRE <- factor(icu$PRE)
icu$TYP <- factor(icu$TYP)
icu$FRA <- factor(icu$FRA)
icu$PO2 <- factor(icu$PO2)
icu$PH <- factor(icu$PH)
icu$PCO <- factor(icu$PCO)
icu$BIC <- factor(icu$BIC)
icu$CRE <- factor(icu$CRE)
icu$LOC <- factor(icu$LOC)

attach(icu)
set.seed(1)
index <- sample(2, nrow(icu), replace = TRUE, prob = c(0.75, 0.25))
train.icu <- icu[index == 1,]
test.icu <- icu[index == 2,]

#develop default model
icu.glm.default <- glm(STA ~ ., data = train.icu, family = binomial)
summary(icu.glm.default)

#refine model
icu.glm.step <- step(icu.glm.default, direction = "both")
summary(icu.glm.step)

icu.predict.default <- predict(icu.glm.default, newdata = test.icu, type = "response")
contrasts(STA)
summary(icu.predict.default)

icu.predict.step <- predict(icu.glm.step, newdata = test.icu, type = "response")
contrasts(STA)
summary(icu.predict.step)

#goodness of fit
icu.predict.default <- ifelse(icu.predict.default > 0.5, 1, 0)
misClasificError <- mean(icu.predict.default != test.icu$STA)
print(paste('Accuracy', 1 - misClasificError))

icu.predict.step <- ifelse(icu.predict.step > 0.5, 1, 0)
misClasificError <- mean(icu.predict.step != test.icu$STA)
print(paste('Accuracy', 1 - misClasificError))

#ROCR Curve
#par(mfrow = c(2, 2))
#install.packages("ROCR")
library(ROCR)


#ROC and AUC for default model
ROCRpred.default <- prediction(icu.predict.default, test.icu$STA)
ROCRperf <- performance(ROCRpred.default, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7))

auc.default <- performance(ROCRpred.default, measure = "auc")
auc.default <- auc.default@y.values[[1]]
auc.default

minauc <- min(round(auc.default, digits = 2))
maxauc <- max(round(auc.default, digits = 2))
minauct <- paste(c("min(AUC)  = "), minauc, sep = "")
maxauct <- paste(c("max(AUC) = "), maxauc, sep = "")
legend(0.3, 0.6, c(minauct, maxauct, "\n"), border = "white", cex = 1.0, box.col = "white")

#ROC and AUC for min model
ROCRpred.step <- prediction(icu.predict.step, test.icu$STA)
ROCRperf <- performance(ROCRpred.step, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7))

auc.step <- performance(ROCRpred.step, measure = "auc")
auc.step <- auc.step@y.values[[1]]
auc.step

minauc <- min(round(auc.step, digits = 2))
maxauc <- max(round(auc.step, digits = 2))
minauct <- paste(c("min(AUC)  = "), minauc, sep = "")
maxauct <- paste(c("max(AUC) = "), maxauc, sep = "")
legend(0.3, 0.6, c(minauct, maxauct, "\n"), border = "white", cex = 1.0, box.col = "white")
#good area under the curve-->good fit

#Hosmer-Lemeshow Goodness of Fit 
#install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(icu.glm.step$y, fitted(icu.glm.step))

hoslem.test(icu.glm.default$y, fitted(icu.glm.default))

#Create a function that will be deployed as a WS
predictMortalityStatus <- function(newdata) {
    predictSTA <- predict(icu.glm.step, newdata = newdata, type = "response")
    predictSTA <- ifelse(predictSTA > 0.5, 1, 0)
}


#install.packages("AzureML")
library(AzureML)
wsAuth <- 'baedeef56bf843b09fe533bbd44e3434'
wsID <- '4541c44faeca4419ad52a3dc507df7ec'
ws <- workspace(
  id = wsID,
  auth = wsAuth
)
deleteWebService(ws, name = "icuMortalityWebService", refresh = TRUE)

icuMortalityWebService <- publishWebService(ws, fun = predictMortalityStatus,
                                            name = "icuMortalityWebService",
                                            inputSchema = train.icu, data.frame = TRUE)
str(icuMortalityWebService)
# OK, try this out, and compare with raw data
newdata <- data.frame(STA = 1, AGE = c(27), GENDER = c(1), RACE = c(1), SER = c(0), CAN = c(0),
                      CRN = c(0), INF = c(1), CPR = c(0), SYS = c(142), HRA = c(88), PRE = c(0),
                      TYP = c(1), FRA = c(0), PO2 = c(0), PH = c(0), PCO = c(0), BIC = c(0), CRE = c(0), LOC = c(0))
#27,1,1,0,0,0,1,0,142,88,0,1,0,0,0,0,0,0,0
#ans <- consume(icuFatalityWebService, test.icu)$ans
ans <- consume(icuMortalityWebService, newdata)$ans
summary(ans)
str(ans)
ans[1]
#plot(ans, test.icu$STA)



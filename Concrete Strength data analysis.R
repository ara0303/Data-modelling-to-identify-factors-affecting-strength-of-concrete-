setwd("E:/Spring 2017/Regression/MLR project")
mlr <- read.csv("MLR.csv")
head(mlr)
y <- mlr[,1]
x1 <- mlr[,2]
x2 <- mlr[,3]
x3 <- mlr[,4]
x4 <- mlr[,5]
x5 <- mlr[,6]
x6 <- mlr[,7]
traindata <- lm(y ~ x1 + x2 + x3 +x4 + x5 + x6 , data = mlr)
traindata
plot(traindata)#plots of the model
anova(traindata)#anova
summary(traindata)#P-value, intercept, standard error, r-sq
aov(traindata)#anova 
res <- residuals(traindata)
leveragePlots(traindata)
resdf <- data.frame(res)
head(resdf)
plot(res, main = "Residuals vs order")#residual plot against index
fitted <- fitted.values(traindata)#fitted values
plot(fitted, res, main = "Residual Plot", xlab = "Fitted values", ylab = "Residuals")#residuals vs fittedvalues
X <- cbind(x1,x2,x3,x4,x5,x6)
Xdf <- data.frame(X)
windows()
absres <- abs(res)
scatterplotMatrix(cbind(absres,X))
scatterplotMatrix(cbind(y,X))
cor(mlr, method = "pearson")
#Normality
qqnorm(res, ylab = "Residuals")
qqline(res)
shapiro.test(res)
#outliers
car::outlierTest(traindata)
stdres <- rstudent(traindata)#studentized residuals of the observations
stdres[abs(stdres) > 4.02]#check if any studentized residual > Bonferroni's cutoff of 4.02
#correlation
car::durbinWatsonTest(traindata)
#leverage
lev <- hatvalues(traindata)
head(lev)
plot(lev, main = "Leverage Plot")
lev[lev > 0.2]# no influential pts
#cooksd
cooksd <- cooks.distance(traindata)
plot(cooksd,main = "Cook's Distance")#plot of cooks distance
cooksd[ cooksd > 2.107]#all cooks distance < cutoff
#levene's test
res <- residuals(traindata)#residuals
resdf <- data.frame(res)#converting residuals to data frame
orderres <- data.frame(resdf[order(resdf$res),])#arranging residuals in ascending order
sample1 <- orderres[1:515,]#Splitting the residuals into 515 values each
sample2 <- orderres[515:1030,]
w <- c(sample1, sample2)#forming a vector combination of residuals
group <- as.factor(c(rep(1, length(sample1)),rep(2,length(sample2))))#grouping of residuals
car::leveneTest(w,group)#levene's test on the samples.
#variance inflation factors
car::vif(traindata)
sqrt(car ::vif(traindata)) > 5
#check for transformation
scatterplotMatrix(resdf, X)
x7 <- log(x6)
#main model
model <- lm(y ~x1 + x2 + x3 +x4 + x5 + x7, data = mlr)
#VAIBHAV
model
res1 <- residuals(model)
shapiro.test(res1)
summary(model)
aov(model)
anova(model)
windows()
plot(model)
vif(model)
#VAIBHAV
#levene's test
resmodel <- residuals(model)#residuals of transformed model
resmodeldf <- data.frame(resmodel)
orderres1 <- data.frame(resmodeldf[order(resmodeldf$resmodel),])#arranging residuals in ascending order
sample11 <- orderres1[1:515,]#Splitting the residuals into 515 values each
sample21 <- orderres1[515:1030,]
w1 <- c(sample11, sample21)#forming a vector combination of residuals
group1 <- as.factor(c(rep(1, length(sample1)),rep(2,length(sample2))))#grouping of residuals
car::leveneTest(w1,group1)#levene's test on the samples.
plot(resmodel, main = "Residuals vs Order")

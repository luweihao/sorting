names=read.csv("names.csv")
breast=read.csv("breast cancer.csv", header = FALSE)
colnames(breast)=make.names(names$Attribute)


#library(mi)
#mdf <- missing_data.frame(breast)
#mdf=change(mdf, y="Bare.Nuclei", what="type", to="ordered-categorical")
#df0=mi(mdf)
#df=complete(df0, 1)
#df=df[,-c(1, 12)]
#df[,-10]=apply(df[,-10], 2, as.integer)

df=breast[,-1]


set.seed(086)
df$Class=factor(df$Class, levels=c(2,4), labels=c("benign", "malignant"))
train=sample(nrow(df), 0.7*nrow(df))
df.train=df[train, ]
df.validate=df[-train, ]
table(df.train$Class)
table(df.validate$Class)


fit.logit=glm(Class~., data=df.train, family=binomial())
summary(fit.logit)  # model check
fit.logit.step=step(fit.logit, trace = 0)
prob=predict(fit.logit, df.validate, type = "response")
logit.pred=factor(prob>.5, levels = c(FALSE, TRUE),
                  labels=c("benign", "malignant"))  # sort the validate sample
table(df.validate$Class, logit.pred, dnn=c("Actual", "Predicted"))

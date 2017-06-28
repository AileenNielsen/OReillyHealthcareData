setwd('~/Desktop/OReillyHealthcareData/code/Overview/prediction')

require(data.table)

df <- fread('fertility.csv')

# don't do this
train <- df[1:8000,]
test <- df[8001:10000,]

# do this
smpl.Size = floor(nrow(df)*.8)
train.ind <- sample(seq_len(nrow(df)), size = smpl.Size)
test.ind <- setdiff(seq_len(nrow(df)), train.ind)

train <- df[train.ind,]
test <- df[test.ind,]

# fitting the model
model <- glm(outcome ~., family=binomial(link='logit'), data=train)
summary(model)

# checking the fit
fitted.results <- predict(model, newdata=test, type='response')
fitted.results <- ifelse(fitted.results > 0.5, 1,0)
test[, prediction:= fitted.results]
table(test[, .(outcome, prediction),])
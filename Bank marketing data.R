#descriptive stat::
ggplot(bank_mkt, aes(x=job, fill = y)) + geom_bar() +
  ggtitle("Job types vs decision") + xlab("Job Types") + 
  ylab("Title") + labs(fill= "y")

ggplot(bank_mkt, aes(x=education, fill = y)) + geom_bar() + facet_wrap(~marital)+
  ggtitle("Marital status vs education") + xlab("Educational qualification") + 
  ylab("Count") + labs(fill= "y")

ggplot(bank_mkt, aes(x=housing, fill = y)) + geom_bar() + facet_wrap(~default)+
  ggtitle("Housing loan vs default") + xlab("Housing loan") + 
  ylab("Count") + labs(fill= "y")

ggplot(bank_mkt, aes(x=loan, fill = y)) + geom_bar() + facet_wrap(~default)+
  ggtitle("Personal loan vs default") + xlab("Personal loan") + 
  ylab("Count") + labs(fill= "y")

ggplot(bank_mkt, aes(x=month, fill = y)) + geom_bar() + facet_wrap(~day_of_week)+
  ggtitle("Month vs day") + xlab("Month") + 
  ylab("Count") + labs(fill= "y")

ggplot(bank_mkt, aes(x=poutcome, fill = y)) + geom_bar() +
  ggtitle("Success vs previous outcome") + xlab("Previous outcome") + 
  ylab("Count") + labs(fill= "y")

cor.test(bank_mkt$age,bank_mkt$y)
bank_mkt$y <- as.numeric(bank_mkt$y)
cor.test(bank_mkt$pdays,bank_mkt$y)
cor.test(bank_mkt$previous,bank_mkt$y)
cor.test(bank_mkt$campaign,bank_mkt$y)
cor.test(bank_mkt$previous,bank_mkt$y)
cor(bank_mkt[17:21])

#decision tree::
#model-01:: (packages: rpart, rpart.plot, rattle)
bank.rpart <- rpart(y~age+job+marital+education+default+housing+loan
                    +contact+month+day_of_week + campaign+pdays+previous
                    +poutcome+cons.price.idx+cons.conf.idx+euribor3m, data = bank_train)

T#Graphical tree
fancyRpartPlot(bank.rpart)

#predict:: train
pred_train_tree <- predict(bank.rpart, bank_train, type = "class")
pred_test_tree <- predict(bank.rpart, bank_test, type = "class")
#confusion matrix::tree
con.mat.train.tree <- prop.table(table(bank_train$y,pred_train_tree))
con.mat.test.tree <- prop.table(table(bank_test$y,pred_test_tree))

#model-02::
bank.rpart1 <- rpart(y~job+marital+education+campaign+pdays+previous+poutcome+
                      cons.price.idx+cons.conf.idx+euribor3m, data = bank_train)
#predict:: train
pred_train_tree_1 <- predict(bank.rpart1, bank_train, type = "class")
pred_test_tree_1 <- predict(bank.rpart1, bank_test, type = "class")
#confusion matrix::tree
con.mat.train.tree_1 <- prop.table(table(bank_train$y,pred_train_tree_1))
con.mat.test.tree_1 <- prop.table(table(bank_test$y,pred_test_tree_1))

#logistic regression::
bank.logistic <- glm(y~age+job+marital+education+default+housing+loan
                     +contact+month+day_of_week + campaign+pdays+previous
                     +poutcome+cons.price.idx+cons.conf.idx+euribor3m, 
                    data = bank_train, family = binomial)
#predict:: train
predictions.logistic.train <- predict(bank.logistic,bank_train,type = "response")
predictions.logistic.binary.train <- ifelse(predictions.logistic.train > .5,"yes","no")

#predict:: test
predictions.logistic.test <- predict(bank.logistic,bank_test,type = "response")
predictions.logistic.binary.test <- ifelse(predictions.logistic.test > .5,"yes","no")

#Confusion matrix::
con.mat.log.train <- prop.table(table(bank_train$y,predictions.logistic.binary.train))
con.mat.log.test <- prop.table(table(bank_test$y,predictions.logistic.binary.test))


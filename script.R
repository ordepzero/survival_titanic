test = read.csv(file.choose(),header = T)
train = read.csv(file.choose(),header = T)

summary(train)

#Adicionar a variavel "survived" ao conjunto de test para combinar as duas bases (train e test)
test.survived <- data.frame(survived = rep("None",nrow(test)),test[,])

train.order <- train[,c(2,1,3,4,5,6,7,8,9,10,11,12)]
colnames(test.survived)[1] <- "Survived"

data.combined <- rbind(train.order,test.survived)

str(data.combined)
summary(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

table(data.combined$Survived)
table(data.combined$Pclass)

library(ggplot2)
train$Pclass <- as.factor(train$Pclass)
str(train)

ggplot(train,aes(x=Pclass,fill = factor(Survived)))+
  geom_bar(width=0.5)+ # Indica a largura das barras
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill="Survived") 

head(as.character(train$Name))
length(unique(as.character(data.combined$Name))) #Quantos nomes difentes há na base

dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

data.combined[which(data.combined$Name %in% dup.names),]


library(stringr)
misses <- data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]

mirses <- data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mirses[1:5,]

data.combined$Sex <- as.factor(data.combined$Sex)
males <- data.combined[which(data.combined$Sex == "male"),]
males[1:5,]


extractTitle <- function(name){
  
  name <- as.character(name)
  
  if(length(grep("Miss.",name)) > 0){
    return("Miss.")
  }else if(length(grep("Master.",name)) > 0){
    return("Master.")
  }else if(length(grep("Mrs.",name)) > 0){
    return("Mrs.")
  }else if(length(grep("Mr.",name)) > 0){
    return("Mr.")
  }else {
    return("Other")
  }
}

titles <- NULL
for(i in 1:nrow(data.combined)){
  titles <- c(titles,extractTitle(data.combined[i,"Name"]))
}
data.combined$Title <- as.factor(titles)



ggplot(data.combined[1:891,],aes(x=Title,fill = factor(Survived)))+
  geom_bar(width=0.5)+ # Indica a largura das barras
  facet_wrap(~Pclass)+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill="Survived") 


table(data.combined$Sex)


ggplot(data.combined[1:891,],aes(x=Sex,fill = factor(Survived)))+
  geom_bar(width=0.5)+ # Indica a largura das barras
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill="Survived") 


summary(data.combined$Age)
summary(data.combined[1:891,"Age"])


ggplot(data.combined[1:891,],aes(x=Age,fill = Survived))+
  facet_wrap(~Sex+Pclass)+
  geom_histogram(binwidth=10)+
  xlab("Age")+
  ylab("Total Count")

boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

misses <- data.combined[which(data.combined$Title == "Miss."),]
summary(misses$Age)


ggplot(misses[misses$Survived != "None",],aes(x=Age,fill = Survived))+
  facet_wrap(~Pclass)+
  geom_histogram(binwidth=5)+
  ggtitle("Age for 'Miss.' by Pclass")+
  xlab("Age")+
  ylab("Total Count")
  
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

summary(data.combined$SibSp)

length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

ggplot(data.combined[1:891,],aes(x=SibSp,fill = Survived))+
  geom_bar(width=1)+
  facet_wrap(~Pclass+Title)+
  ggtitle("Pclass, Title")+
  xlab("Sibsp")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

data.combined$Parch <- as.factor(data.combined$Parch)

ggplot(data.combined[1:891,],aes(x=Parch,fill = Survived))+
  geom_bar(width=1)+
  facet_wrap(~Pclass+Title)+
  ggtitle("Pclass, Title")+
  xlab("Parch")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

temp.sibsp <- c(train$SibSp,test$SibSp)
temp.parch <- c(train$Parch,test$Parch)
data.combined$Family.size <- as.factor(temp.sibsp+temp.parch+1)#O mais 1 indica a pessoa da instancia

ggplot(data.combined[1:891,],aes(x=Family.size,fill = Survived))+
  geom_bar(width=1)+
  facet_wrap(~Pclass+Title)+
  ggtitle("Pclass, Title")+
  xlab("Family.size")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")



str(data.combined$Ticket)
#Conveter essa variável em string porque há muitos valores diferentes
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]


ticket.first.char <- ifelse(data.combined$Ticket == ""," ",substr(data.combined$Ticket,1,1))
unique(ticket.first.char)

data.combined$Ticket.first.char <-as.factor(ticket.first.char)

ggplot(data.combined[1:891,],aes(x=Ticket.first.char,fill=Survived))+
  geom_bar()+
  ggtitle("Survivavility by ticket.first.char")+
  xlab("ticket.first.char")+
  ylab("Total Counts")+
  ylim(0,350)+
  labs(fill="Survived")


ggplot(data.combined[1:891,],aes(x=Ticket.first.char,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("ticket.first.char")+
  ylab("Total Counts")+
  ylim(0,150)+
  labs(fill="Survived")

summary(data.combined$Ticket.first.char)
length(data.combined$Ticket.first.char)


ggplot(data.combined[1:891,],aes(x=Ticket.first.char,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+Title)+
  ggtitle("Pclass, Title")+
  xlab("ticket.first.char")+
  ylab("Total Counts")+
  ylim(0,200)+
  labs(fill="Survived")

summary(data.combined$Fare)
length(data.combined$Fare)

ggplot(data.combined,aes(x=Fare))+
  geom_histogram(binwidth = 5)
  ggtitle("Combined Fare Distribution")+
  xlab("Fare")+
  ylab("Total Counts")+
  ylim(0,200)
  
ggplot(data.combined[1:891,],aes(x=Fare,fill=Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass+Title)+
  ggtitle("Pclass, Title")+
  xlab("Fare")+
  ylab("Total Counts")+
  ylim(0,50)+
  labs(fill="Survived")

str(data.combined$Cabin)
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

data.combined[which(data.combined$Cabin == ""),"Cabin"] <- "U"
data.combined$Cabin[1:100]


cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)

data.combined$Cabin.first.char <- cabin.first.char

ggplot(data.combined[1:891,],aes(x=Cabin.first.char,fill=Survived))+
  geom_bar()+
  ggtitle("Survivability by cabin.first.char")+
  xlab("cabin.first.char")+
  ylab("Total Counts")+
  ylim(0,750)+
  labs(fill="Survived")

ggplot(data.combined[1:891,],aes(x=Cabin.first.char,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Survivability by cabin.first.char")+
  xlab("Pclass")+
  ylab("Total Counts")+
  ylim(0,500)+
  labs(fill="Survived")


ggplot(data.combined[1:891,],aes(x=Cabin.first.char,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+Title)+
  ggtitle("Pclass, Title")+
  xlab("Cabin.first.char")+
  ylab("Total Counts")+
  ylim(0,500)+
  labs(fill="Survived")


data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "),"Y","N"))

ggplot(data.combined[1:891,],aes(x=Cabin.multiple,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+Title)+
  ggtitle("Pclass, Title")+
  xlab("Cabin.multiple")+
  ylab("Total Counts")+
  ylim(0,350)+
  labs(fill="Survived")

str(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891,],aes(x=Embarked,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+Title)+
  ggtitle("Pclass, Title")+
  xlab("Embarked")+
  ylab("Total Counts")+
  ylim(0,300)+
  labs(fill="Survived")

library(randomForest)


rf.train.1 <- data.combined[1:891,c("Pclass","Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x=rf.train.1,y=rf.label,importance = T,ntree = 1000)
rf.1 
varImpPlot(rf.1)

rf.train.2 <- data.combined[1:891,c("Pclass","Title","SibSp")]

set.seed(1234)
rf.2 <- randomForest(x=rf.train.2,y=rf.label,importance = T,ntree = 1000)
rf.2
varImpPlot(rf.2)


rf.train.3 <- data.combined[1:891,c("Pclass","Title","Parch")]
set.seed(1234)
rf.3 <- randomForest(x=rf.train.3,y=rf.label,importance = T,ntree = 1000)
rf.3
varImpPlot(rf.3)


rf.train.4 <- data.combined[1:891,c("Pclass","Title","SibSp","Parch")]
set.seed(1234)
rf.4 <- randomForest(x=rf.train.4,y=rf.label,importance = T,ntree = 1000)
rf.4
varImpPlot(rf.4)


rf.train.5 <- data.combined[1:891,c("Pclass","Title","Family.size")]
set.seed(1234)
rf.5 <- randomForest(x=rf.train.5,y=rf.label,importance = T,ntree = 1000)
rf.5
varImpPlot(rf.5)


rf.train.6 <- data.combined[1:891,c("Pclass","Title","SibSp","Family.size")]
set.seed(1234)
rf.6 <- randomForest(x=rf.train.6,y=rf.label,importance = T,ntree = 1000)
rf.6
varImpPlot(rf.6)


rf.train.7 <- data.combined[1:891,c("Pclass","Title","Parch","Family.size")]
set.seed(1234)
rf.7 <- randomForest(x=rf.train.7,y=rf.label,importance = T,ntree = 1000)
rf.7
varImpPlot(rf.7)


test.submit.df <- data.combined[892:1309,c("Pclass","Title","Family.size")]
rf.5.preds <- predict(rf.5,test.submit.df)
table(rf.5.preds)

submit.df <- data.frame(PassengerID = rep(892:1309),Survived=rf.5.preds)
write.csv(submit.df,file="RF_SUB_20180811_1.csv",row.names = FALSE,quote = FALSE)


library(caret)
library(doSNOW)


set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label,k=10,times=10)

table(rf.label)
table(rf.label[cv.10.folds[[33]]])

ctrl.1 <- trainControl(method = "repeatedcv",number=10,repeats = 10,index = cv.10.folds)

cl <- makeCluster(1,type = "SOCK")
registerDoSNOW(cl)

set.seed(34324)
rf.5.cv.1 <- train(x=rf.train.5,y=rf.label,method="rf",tuneLength=3,ntree=1000,trControl=ctrl.1)
stopCluster(cl)


rf.5.cv.1

set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label,k=5,times=10)

ctrl.2 <- trainControl(method="repeatedcv",number=5,repeats = 10,index=cv.5.folds)
cl <- makeCluster(1,type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x=rf.train.5,y=rf.label,method="rf",tuneLength=3,
                   ntree=1000,trControl=ctrl.2)
stopCluster(cl)


rf.5.cv.2

set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label,k=3,times=10)
ctrl.3 <- trainControl(method = "repeatedcv",number=3,repeats=10
                       ,index=cv.3.folds)

cl <- makeCluster(1,type="SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x=rf.train.5,y=rf.label,method="rf",tuneLength=3,
                   ntree=64,trControl=ctrl.3)

stopCluster(cl)
rf.5.cv.3

library(rpart)
library(rpart.plot)

rpart.cv <- function(seed,training,labels,ctrl){
  cl <- makeCluster(6,type="SOCK")
  registerDoSNOW(cl)
  set.seed(seed)
  rpart.cv <- train(x=training,y=labels,method="rpart",tuneLength=30,
                    trControl=ctrl)
  
  stopCluster(cl)
  return(rpart.cv)
}


features <-c("Pclass","Title","Family.size")
rpart.train.1 <- data.combined[1:891,features]

rpart.1.cv.1 <- rpart.cv(94662,rpart.train.1,rf.label,ctrl.3)
rpart.1.cv.1

prp(rpart.1.cv.1$finalModel,type=0,extra=1,under=T)


table(data.combined$Title)
data.combined[1:25,"Name"]
name.splits <- str_split(data.combined$Name,",")
name.splits[1]
last.name <- sapply(name.splits,"[",1)
data.combined$Last.name <- last.name

name.splits <- str_split(sapply(name.splits,"[",2)," ")
titles <- sapply(name.splits,"[",2)
unique(titles)


data.combined[which(titles=="the"),]

titles[titles %in% c("Dona.","the")] <- "Lady."
titles[titles %in% c("Ms.","Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.","Don.")] <- "Sir."
titles[titles %in% c("Col.","Capt.","Major.")] <- "Officer"
table(titles)

data.combined$new.title <- as.factor(titles)

ggplot(data.combined[1:891,],aes(x=new.title,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Survival Rates for new.title by Pclass")


indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title =="Dr." |
                   data.combined$new.title =="Rev." |
                   data.combined$new.title =="Sir." |
                   data.combined$new.title =="Officer")
data.combined$new.title[indexes] <- "Mr."

ggplot(data.combined[1:891,],aes(x=new.title,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Survival Rates for new.title by Pclass")


features <- c("Pclass","new.title","Family.size")
rpart.train.2 <- data.combined[1:891,features]

rpart.2.cv.1 <- rpart.cv(94622,rpart.train.2,rf.label,ctrl.3)
rpart.2.cv.1


prp(rpart.2.cv.1$finalModel,type=0,extra=1,under=T)


indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == 1)
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)


first.mr.df[first.mr.df$Sex == "female",]


indexes <- which(data.combined$new.title == "Mr." &
                   data.combined$Sex == "female")
data.combined$new.title[indexes] <- "Mrs."

length(which(data.combined$Sex == "female" & (data.combined$new.title == "Master." |
                                                data.combined$new.title == "Mr.")))

indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == 1)
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df[first.mr.df$Survived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])


ggplot(first.mr.df, aes(x=Fare,fill=Survived))+
  geom_density(alpha=0.5)+
  ggtitle("1st Class 'Mr' Survival Rates by Fare" )



indexes <- which(data.combined$Ticket == "PC 17755" |
                   data.combined$Ticket == "PC 17611" |
                   data.combined$Ticket == "113760")
View(data.combined[indexes,])

ticket.party.size <- rep(0,nrow(data.combined))
avg.fare <- rep(0.0,nrow(data.combined))
tickets <- unique(data.combined$Ticket)


for(i in 1:length(tickets)){
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1],"Fare"]/length(party.indexes)
  
  for(k in 1:length(party.indexes)){
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare


first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)


ggplot(first.mr.df[first.mr.df$Survived != "None",],aes(x=ticket.party.size,fill=Survived))+
  geom_density(alpha=0.5)+
  ggtitle("Survival Rates 1st Class 'Mr.' by ticket.party.size")



ggplot(first.mr.df[first.mr.df$Survived != "None",],aes(x=avg.fare,fill=Survived))+
  geom_density(alpha=0.5)+
  ggtitle("Survival Rates 1st Class 'Mr.' by avg.fare")


summary(data.combined$avg.fare)

data.combined[is.na(data.combined$avg.fare),]

indexes <- with(data.combined,which(Pclass=="3" & Title=="Mr." & Family.size == 1 &
                                      Ticket != "3701"))

similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

data.combined[is.na(avg.fare),"avg.fare"] <- 7.840

preproc.data.combined <- data.combined[,c("ticket.party.size","avg.fare")]
preProc <- preProcess(preproc.data.combined,method=c("center","scale"))

postproc.data.combined <- predict(preProc,preproc.data.combined)

cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes],
    postproc.data.combined$avg.fare[indexes])

features <- c("Pclass","new.title","Family.size","ticket.party.size","avg.fare")
rpart.train.3 <- data.combined[1:891,features]

rpart.3.cv.1 <- rpart.cv(94622,rpart.train.3,rf.label,ctrl.3)
rpart.3.cv.1

prp(rpart.3.cv.1$finalModel,type=0,extra=1,under=T)


test.submit.df <- data.combined[892:1309, features]
rpart.3.preds <- predict(rpart.3.cv.1$finalModel,test.submit.df,type="class")
table(rpart.3.preds)

submit.df <- data.frame(PassengerID=rep(892:1309),Survived=rpart.3.preds)

write.csv(submit.df,file="RF_SUB_20180811_2.csv",row.names = FALSE,quote = FALSE)

features <- c("Pclass","new.title","ticket.party.size","avg.fare")
rf.train.temp <- data.combined[1:891,features]

set.seed(1234)
rf.temp <- randomForest(x=rf.train.temp,y=rf.label,ntree=1000)
rf.temp

test.submit.df <- data.combined[892:1309,features]

rf.preds <- predict(rf.temp,test.submit.df)
table(rf.preds)


submit.df <- data.frame(PassengerID=rep(892:1309),Survived=rpart.3.preds)
write.csv(submit.df,file="RF_SUB_20180815_3.csv",row.names = FALSE,quote = FALSE)




#######################
library(infotheo)
mutinformation(rf.label,data.combined$Pclass[1:891])
mutinformation(rf.label,data.combined$Sex[1:891])
mutinformation(rf.label,data.combined$SibSp[1:891])
mutinformation(rf.label,data.combined$Parch[1:891])
mutinformation(rf.label,discretize(data.combined$Parch[1:891]))
mutinformation(rf.label,data.combined$Embarked[1:891])
mutinformation(rf.label,data.combined$Title[1:891])
mutinformation(rf.label,data.combined$Family.size[1:891])
mutinformation(rf.label,data.combined$Ticket.first.char[1:891])
mutinformation(rf.label,data.combined$Cabin.multiple[1:891])
mutinformation(rf.label,data.combined$new.title[1:891])
mutinformation(rf.label,data.combined$ticket.party.size[1:891])
mutinformation(rf.label,discretize(data.combined$avg.fare[1:891]))


library(Rtsne)

most.correct <- data.combined[data.combined$new.title != "Mr.",]
indexes <- which(most.correct$Survived != "None")

tsne.1 <- Rtsne(most.correct[,features],check_duplicates = FALSE)
ggplot(NULL,aes(x=tsne.1$Y[indexes,1],y=tsne.1$Y[indexes,2],color=most.correct$Survived[indexes]))+
  geom_point()+
  labs(color = "Survived")+
  ggtitle("Tsne 2D visualization of features for females and boys")




condinformation(most.correct$Survived[indexes],discretize(tsne.1$Y[indexes,]))

condinformation(rf.label,data.combined[1:891,c("new.title","Pclass")])


misters <- data.combined[data.combined$new.title == "Mr.",]
indexes <- which(misters$Survived != "None")
tsne.2 <- Rtsne(misters[,features],check_duplicates=FALSE)
ggplot(NULL,aes(x=tsne.2$Y[indexes,1],y=tsne.2$Y[indexes,2],
                color=misters$Survived[indexes]))+
  geom_point()+
  labs(color="Survived")+
  ggtitle("TSNE 2D Visualization of features for new.title of 'Mr.'")


condinformation(misters$Survived[indexes],discretize(tsne.2$Y[indexes,]))



tsne.3 <- Rtsne(data.combined[,features],check_duplicates=FALSE)
ggplot(NULL,aes(x=tsne.3$Y[1:891,1],y=tsne.3$Y[1:891,2],
                color=data.combined$Survived[1:891]))+
  geom_point()+
  labs(color="Survived")+
  ggtitle("TSNE 2D Visualization of features for new.title of 'Mr.'")

condinformation(data.combined$Survived[1:891],discretize(tsne.3$Y[1:891,]))

data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]





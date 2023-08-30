#The variable coding is below:
 #1) PI: Process Innovation
 #2) IProd: Product Innovation
 #3) ST: Strategic Thinking
 #4) IA: Innovation Activities
#This code will explore the effects of IA on PI and IProd

#Setting Working Directory
setwd("C:/Users/idpdl/Desktop/Papers Chang")
library("randomForest"); library(corrplot); library(neuralnet); library("e1071") #SVM
#Inputs: PE; #Outputs: IProcess, individualized, average
X <- read.csv("Questionnaire Outcomes.csv", header = T)
X[is.na(X)] <- as.numeric(0); n <- ncol(X); nf <- nrow(X); X2 <- X
for (i in 1:n) {
  if(class(X[,i]) == "character"){
    for (j in 1:nf) {
      if(X[j,i] == "Totalmente de acuerdo"){ #Spanish for "Totally agree"
        X2[j,i] <- 2
      }else if(X[j,i] == "De acuerdo"){ #Spanish for "Agree"
        X2[j,i] <- 1
      }else if(X[j,i] == "Ni de acuerdo ni en desacuerdo"){ #Spanish for "Neither agree nor disagree"
        X2[j,i] <- 0
      }else if(X[j,i] == "En desacuerdo"){ #Spanish for "disagree"
        X2[j,i] <- -1
      }else if(X[j,i] == "Totalmente en desacuerdo."){ #Spanish fpr "Totally disagree"
        X2[j,i] <- -2
      }
    }
  }
}
for (i in 1:n) {
  X2[,i] = as.numeric(X2[,i])
}

#Retaining the variables to be used in this analysis (i.e., PI, IProd, and IA-related variables)
X2 <- X2[,grepl("PI",colnames(X2)) | grepl("IProd",colnames(X2)) | grepl("IA",colnames(X2))]

# Correlation Heatmaps
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], method = "spearman", ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
corMat=cor(X2, method = "spearman")
temp <- is.na(corMat)
corMat[temp] <- as.numeric(0)
p.mat <- cor.mtest(X2)
corrplot(corMat, type="upper", order="hclust", p.mat = p.mat, sig.level = 0.05, pch.cex=1.1,
         cl.cex = 0.7, tl.cex = 0.7)

#Machine Learning Models
X2$PI_mean <- rowMeans(X2[,grepl("PI",colnames(X2))])
X2$IProd_mean <- rowMeans(X2[,grepl("IProd",colnames(X2))])

#Process Innovation (PI)
#Random Forest
RF.mod_PI <- randomForest(x = X2[,grepl("IA",colnames(X2))], y = X2$PI_mean, ntree = 200)
Preds_PI <- predict(RF.mod_PI, X2[,grepl("IA",colnames(X2))])
MAVE_PI = mean(abs(X2$PI_mean - Preds_PI)) #MAVE stands for Mean Absolute Value Error
MAVE_PI
MAVE_PI_percentage_of_span_RF = 100*MAVE_PI/(max(X2$PI_mean) - min(X2$PI_mean))
MAVE_PI_percentage_of_span_RF
#varImpPlot(RF.mod_PI, sort = F)
importance(RF.mod_PI)

#Support Vector Machines
SVM.mod_PI <- svm(PI_mean ~ IA1+IA2+IA3+IA4+IA5, data = X2)
Preds_PI_2 = predict(SVM.mod_PI, X2[,grepl("IA",colnames(X2))])
MAVE_PI_2 = mean(abs(X2$PI_mean - Preds_PI_2))
MAVE_PI_2
MAVE_PI_percentage_of_span_SVM = 100*MAVE_PI_2/(max(X2$PI_mean) - min(X2$PI_mean))
MAVE_PI_percentage_of_span_SVM

#Artificial Neural Networks
set.seed(1)
ANN_PI = neuralnet(PI_mean ~ IA1+IA2+IA3+IA4+IA5, X2, hidden=c(3,6))
ANN_PI = neuralnet(PI_mean ~ IA1+IA4+IA5, X2, hidden=c(3,6))
plot(ANN_PI)
Pred <- compute(ANN_PI,X2)
Preds_PI_3 <- Pred$net.result
MAVE_PI_3 = mean(abs(X2$PI_mean - Preds_PI_3))
MAVE_PI_3
MAVE_PI_percentage_of_span_ANN = 100*MAVE_PI_3/(max(X2$PI_mean) - min(X2$PI_mean))
MAVE_PI_percentage_of_span_ANN

#Product Innovation (IProd)
#Random Forest
RF.mod_IProd <- randomForest(x=X2[,grepl("IA",colnames(X2))], y=X2$IProd_mean, ntree = 200)
Preds_IProd <- predict(RF.mod_IProd, X2[,grepl("IA",colnames(X2))])
MAVE_IProd = mean(abs(X2$IProd_mean - Preds_IProd))
MAVE_IProd
MAVE_IProd_percentage_of_span_RF = 100*MAVE_IProd/(max(X2$IProd_mean)-min(X2$IProd_mean))
MAVE_IProd_percentage_of_span_RF
importance(RF.mod_IProd, sort = T)

#Support Vector Machines
SVM.mod_IProd <- svm(IProd_mean ~ IA1+IA2+IA3+IA4+IA5, data = X2)
Preds_IProd_2 = predict(SVM.mod_IProd, X2[,grepl("IA",colnames(X2))])
MAVE_IProd_2 = mean(abs(X2$IProd_mean - Preds_IProd_2))
MAVE_IProd_2
MAVE_IProd_percentage_of_span_SVM = 100*MAVE_IProd_2/(max(X2$IProd_mean)-min(X2$IProd_mean))
MAVE_IProd_percentage_of_span_SVM

#Artificial Neural Networks
set.seed(1)
ANN_IProd = neuralnet(IProd_mean ~ IA1+IA2+IA3+IA4+IA5, X2, hidden=c(3,6))
plot(ANN_IProd)
Pred <- compute(ANN_IProd,X2)
Preds_IProd_3 <- Pred$net.result
MAVE_IProd_3 = mean(abs(X2$IProd_mean - Preds_IProd_3))
MAVE_IProd_3
MAVE_IProd_percentage_of_span_ANN = 100*MAVE_IProd_3/(max(X2$IProd_mean)-min(X2$IProd_mean))
MAVE_IProd_percentage_of_span_ANN

#Strong correlations
threshold = 0.5
diag(corMat) <- 0
List <- {}; List2 <- {}
for (i in 1:nrow(corMat)) {
  for (j in 1:ncol(corMat)) {
    cond1=(grepl("IA",rownames(corMat)[i])&grepl("PI",colnames(corMat)[j]))|(grepl("IA",rownames(corMat)[i])&grepl("IProd",colnames(corMat)[j]))
    if(abs(corMat[i,j]) >= threshold & (cond1) ) {
      List = c(List, paste0(rownames(corMat)[i], "-", colnames(corMat)[j] , sep=""))
      List2 = c(List2, corMat[i,j])
    }
  }
}
Strong_corrs = as.data.frame(List2)
rownames(Strong_corrs) = List
write.csv(Strong_corrs, file = "Strong Correlations.csv")

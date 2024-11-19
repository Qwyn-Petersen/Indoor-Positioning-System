data_1<-readLines("C:/Users/risin/OneDrive/Desktop/offline.final.trace.txt", n = 913)
data_1<- data_1[-(1:3)]
head(data_1)
library(stringr)

break_1<-str_split(data_1[1],pattern = ";")
key_info<-lapply(str_split(break_1[[1]][1:4],"="),"[[",2)
names<-lapply(str_split(break_1[[1]][1:4],"="),"[[",1)
n=4
X<-matrix(key_info,ncol=n,nrow=length(break_1[[1]])-n,byrow=T)
colnames(X) <- names
X<-cbind(X,break_1[[1]][-(1:4)])

library(tidyr)
df_1 <- as.data.frame(X, stringsAsFactors = FALSE)
df_1$V5 <- as.character(df_1$V5)
df_1.1 <- separate(df_1, col = "V5", into = c("Router", "Signal_Strength"), sep = "=")
df_1.2 <- separate(df_1.1, col = "Signal_Strength", into = c("Signal_Strength","Frequency","Mode"), sep = ",")


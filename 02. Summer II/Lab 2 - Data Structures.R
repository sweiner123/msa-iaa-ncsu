#lab 2 - Data Structures

#Q - 1
v1 <- seq(from = 10,by = 10,length.out = 100)
length(v1)
sum_of_v1 = sum(v1)


#Q - 2
v2 <- rep(25,100)
length(v2)
sum_of_v2 = sum(v2)

#Q - 3
v3 = v1 / v2
max_of_v3 = max(v3)

#Q - 4

Vector_1 = v1
Vector_2 = v2
Vector_3 = v3
  
df1 = data.frame(Vector_1,Vector_2,Vector_3)

#Q - 5
dim(df1)
nrow(df1)

#Q - 6
which(df1$Vector_3 == max(df1$Vector_3),arr.ind = TRUE)
which.max((df1$Vector_3))

#Q - 7
df1$Vector_4 = df1$Vector_1 + df1$Vector_3
max(df1$Vector_4)

#Q - 8
df1 = df1[,c(4,1,2,3)]

#Q - 9
df1[86,which.min(df1[86,])] <- 1
mean(df1$Vector_2)

#Q - 10
df1$Row_Mean <- rowMeans(df1)
df1$Row_Mean[45]

#Q - 11
l1 <- as.list(df1)

#Q - 12
mean(c(l1$Vector_1,l1$Vector_2))

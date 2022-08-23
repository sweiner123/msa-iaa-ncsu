#lab 3 - Programming 

#Q - 1
set.seed(1)

#Q - 2
pop_truth <- rexp(n = 100000, rate = 2)

#Q - 3
round(mean(pop_truth),digits = 2)

#Q - 4
set.seed(1)
sample_pop <- sample(pop_truth,5000)

#Q - 5
sample_stats <- function(data){
  
  mean_value <- mean(data)
  std_value <- sd(data)
  
  ans_list <- list(mean_value,std_value)
  
  return(ans_list)

}

#Q - 6
ans <- sample_stats(pop_truth)
round(ans[[1]],2)

#Q - 7

set.seed(1)
sequence <- seq(1,10000)

for(i in 1:length(sequence)){
  
  print(paste0("Currently on iteration: ", i))
  
  
  if(i==1){
    x <- c("mean", "stddev", "iteration")
    df_results <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df_results) <- x
  }
  
  sample_pop <- sample(pop_truth,500)
  metrics <- sample_stats(sample_pop)
  metrics <- append(metrics, i)
  
  df_results[nrow(df_results) + 1,] <- metrics
  
}

#Q - 8
round(mean(df_results$mean),2)

#Q - 9
percentiles  <- quantile(df_results$mean, probs=c(0.05, 0.95))
round(percentiles[1],2)

#Q - 10
hist(df_results$mean)
hist(pop_truth)


#push to github

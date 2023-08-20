
library(ggplot2)
library(patchwork)
library(mgcv)
library(plyr)
library(dplyr)
library(mgcv)
library(reshape2)
library(matrixStats)
cp<-read.csv("chinstrap_area.csv")
cpm<-ddply(cp,("OBJECTID"),summarise,
           nests=length(AreaM2),
           areaM2=max(AreaM2),
           Longitude=mean(X),
           Latitude=mean(Y))

# Set the number of iterations for the loop
num_iterations <- 250

# Create an empty list to store predicted counts for each iteration
predicted_counts_list <- vector("list", length = num_iterations)
sampled_list<-vector("list", length = num_iterations)
distance_df<-data.frame(Iteration = numeric(num_iterations), 
                        mean_distance = numeric(num_iterations),
                        sd_area=numeric(num_iterations))

# Loop through iterations
for (i in 1:num_iterations) {
  # Sample 100 rows randomly from cpm
  sampled_data <- cpm[sample(nrow(cpm), 100), ]
  sampled_list[[i]]<-sampled_data
  
  # Calculate the mean distance (you can replace this with your aggregation measure)
  # For example, if you have Latitude and Longitude columns in 'sampled_data'
  mean_distance <- mean(sqrt((sampled_data$Longitude - mean(sampled_data$Longitude))^2 +
                               (sampled_data$Latitude - mean(sampled_data$Latitude))^2))
  sd_area<-sd(sampled_data$areaM2)
  distance_df[i, "Iteration"] <- i
  distance_df[i, "mean_distance"] <- mean_distance
    distance_df[i, "sd_area"] <- sd_area
  
  # Fit a GAM model
  gam_model <- gam(nests ~ s(areaM2), data = sampled_data)
  
  # Predict using the GAM model
  predicted_counts <- predict.gam(gam_model, newdata = cpm, type = "response")
  
  # Store the predicted counts in the list
  predicted_counts_list[[i]] <- predicted_counts
}

correlation_df <- data.frame(Iteration = numeric(num_iterations), Correlation = numeric(num_iterations))
# Print the correlation values and predicted counts for each iteration
for (i in 1:num_iterations) {
  correlation_result <- cor.test(cpm$nests, predicted_counts_list[[i]])
  cat("Iteration", i, ": Correlation =", correlation_result$estimate, "\n")
  # Store iteration and correlation value in the data frame
  correlation_df[i, "Iteration"] <- i
  correlation_df[i, "Correlation"] <- correlation_result$estimate
  
  # You can also print or manipulate the predicted counts for each iteration here
  # predicted_counts_iteration <- predicted_counts_list[[i]]
  # print(predicted_counts_iteration)
}


correlation_df$distance<-distance_df$mean_distance
correlation_df$sd_area<-distance_df$sd_area

summary(correlation_df$Correlation)
correlation_df$R<-ifelse(correlation_df$Correlation<0,0,correlation_df$Correlation)


(ggplot(correlation_df,aes(distance*111,R))+geom_point()+
  ggtitle(label="a.Mean distance between sampled groups")+xlab("Kilometers")+
    ylab("Correlation coefficient (R)")+
    stat_smooth(method="glm",
      method.args=list(family="binomial"))+
  theme_bw())|

(ggplot(correlation_df,aes(sd_area,R))+geom_point()+
    ggtitle(label="b.Groups' area standard deviation")+xlab("Square meters")+
   ylab("Correlation coefficient (R)")+
   geom_smooth(method="glm",
               method.args=list(family="binomial"))+
    theme_bw())


### subset

hc<-subset(correlation_df,sd_area>250)

head(hc)
hcl<-hc$Iteration


hpc<-predicted_counts_list[hcl] # subset only iterations with higher area variability

hpc.df<-data.frame(hpc)

head(hpc.df)

rm<-data.frame(mean=rowMeans(hpc.df),sd=rowSds(as.matrix(hpc.df)))

rmc<-data.frame(colsize=colSums(hpc.df))

mean(rmc$colsize)

sd(rmc$colsize)

col.obs<-sum(cpm$nests)



ggplot(df,aes(nests,mean))+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd))+
  geom_point()+geom_smooth()+theme_bw()+
  xlab("Observed number of nests")+
  ylab("Predicted number of nests")+xlim(0,1000)+
  ggtitle(label="a. Nests on breeding groups")+

ggplot(rmc,aes(colsize))+geom_histogram(aes(y =..density..), 
                                        colour = "black", 
                                        fill = "grey50")+
  stat_function(fun = dnorm, 
                args = list(mean = mean(rmc$colsize), sd = sd(rmc$colsize)),
                colour="blue",linewidth=1)+
  theme_bw()+geom_vline(xintercept = col.obs,linetype="dashed",colour="red2",
                        linewidth=1)+
  xlab("Sum of number of nests")+
  ggtitle(label="b. Colony size")
  

t.test(rmc$colsize, mu = col.obs, alternative = "two.sided")



df<-cbind(cpm,rm)
head(df)



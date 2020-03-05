#####Loading packages
library(ggplot2) # visualization
library(ggthemes) # visualization
library(corrplot) # visualisation
library(VIM) #missing values
library(GGally)
library(tidyverse)
#####Loading data
file_path <- "C:/Users/???R??/Desktop/Data_mining_final/"
file_name <- "HRV_data_update.csv"
file <- paste(file_path, file_name, sep =  "")
HRV_data <- read.csv(file)
HRV_data <- HRV_data[,-1]
#####Preprocesing
#(1)Variable structure
str(HRV_data)
#(2)Data overview
summary(HRV_data)


#####(3)EDA
####missing value OK
win.graph()
HRV_data %>% select(-Label) %>%
aggr(prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE)

#####Box plot OK
#Display
HRV_data_log <- log(HRV_data[,-1])
HRV_data_log <- cbind(HRV_data[,1],HRV_data_log)
colnames(HRV_data_log)[1] = "Label"
variables <- colnames(HRV_data)
win.graph()
ggplot(HRV_data, aes(Label,pNNx)) +
  geom_boxplot(aes(fill = Label))+
  labs(y = "pNNx" )

for(i in 2:length(variables)){
  win.graph()
  y_name = variables[i]
  print(ggplot(HRV_data_log, aes(Label,get(variables[i]))) +
    geom_boxplot(aes(fill = Label))+
      labs(y = y_name ))
}
#Save
variables <- colnames(HRV_data)
for(i in 2:length(variables)){
  y_name = variables[i]
  plot <- ggplot(HRV_data, aes(Label,get(variables[i]))) +
          geom_boxplot(aes(fill = Label))+
          labs(y = y_name )
  ggsave(plot, file=paste0(y_name,".png"))
}

#####Scatter plot OK
#Scatter_matrix
plot <- ggpairs(HRV_data, columns = colnames(HRV_data)[which(colnames(HRV_data)!="Label")],
        mapping = ggplot2::aes_string(color = "Label"), lower = list(continuous = "points"),
        upper = list(continuous = "blank"),title = "Variables Scatter matrix")
# Make the legend
plot[1,8] <- points_legend(HRV_data, ggplot2::aes(SDNN, aVLF, color = Label))
win.graph()
plot
#Scatter_plot
win.graph()
ggplot(HRV_data, aes(pNNx, nLF,colour = Label))+
  geom_jitter(size = 3, width = 0.25)

#####Density plot
win.graph()
plot_dp <-ggplot(HRV_data, aes(LFHF, fill=Label)) +
  geom_density(alpha = 0.5)
ggsave(plot_dp, file=paste0("LFHF",".png"))
  
#Correlation plot OK
win.graph()
HRV_data %>% select(-Label) %>%
cor(use="complete.obs") %>%
  corrplot(type="lower", diag=FALSE)

win.graph()
HRV_data %>% select(-Label) %>%
  cor(use="complete.obs") %>%
  corrplot(type="lower", diag=FALSE,order = "hclust")


library(shiny)
library(tidyverse)
library(maps)
library(forcats)
library(MASS)
library(caret)
library(DT)

df_pulsar <- read_csv("./Data/HTRU_2.csv", col_names = FALSE)

names(df_pulsar) <- c("integ_mean","integ_sd","integ_exkur","integ_skew",
                      "DMSNR_mean","DMSNR_sd","DMSNR_exkur","DMSNR_skew","Class")

df_pulsar <- df_pulsar %>% 
  mutate(Class = ifelse(Class == 1, "Pulsar", "Non Pulsar"))

df_pulsar$Class <- as.factor(df_pulsar$Class)

df_pulsar2 <- df_pulsar %>% mutate_at(names(df_pulsar)[1:8], ~(scale(.) %>% as.vector))

var <- names(df_pulsar)

proper_names1 <- c("Integrated Mean", "Integrated Standard Deviation", 
                   "Integrated Kurtosis", "Intergrated Skew")
proper_names2 <- c("DMSNR Mean", "DMSNR Standard Deviation", "DMSNR Kurtosis",
                   "DMSNR Skew")

var_sel1 <- "integ_skew"
var_sel2 <- "DMSNR_skew"


var1 <- which(var == var_sel1)
var2 <- which(var == var_sel2)

p1 <- df_pulsar %>% rename(x = var1, y = var2) %>%
  ggplot() + geom_point(aes(x = x, y = y, col = Class))

print(p1)

df_pulsar %>% head()

df_pulsar %>% rename(x = var1, y = var2) %>%
  ggplot() + geom_point(aes(x = x, y = y, col = Class), size = 0.25) 


which(var == var_sel1)


df_pulsar %>% rename(x = var[var1], y = var[var2]) %>%
  ggplot() + geom_point(aes(x = x, y = y, col = Class), size = 0.25) + 
  labs(x = proper_names1[var1], 
       y = proper_names2[var2 - 4],
       title = paste(proper_names1[var1],
                     "vs" ,
                     proper_names2[var2 - 4])) +
  theme(legend.position = c(0.9, 0.8)) +
  scale_color_manual(values = c("Non Pulsar" = dense_colors[1],
                                "Pulsar" = dense_colors[7])) +
  stat_ellipse(aes(x = x, y = y, col = Class))

##################Colors for charts###################################
dense_colors <- c("#003f5c", "#2f4b7c", "#665191", "#a05195", 
                  "#d45087", "#f95d6a", "#ff7c43", "#ffa600")

dense_colors2 <- c("red","blue","green","yellow")
#####################################################################

ggplot(df_pulsar2) +
  geom_density(aes(x = integ_mean, fill = dense_colors[1]), alpha = .2) +
  geom_density(aes(x = integ_sd, fill = dense_colors[2]), alpha = .2) +
  geom_density(aes(x = integ_exkur, fill = dense_colors[3]), alpha = .2) +
  geom_density(aes(x = integ_skew, fill = dense_colors[4]), alpha = .2) +
  theme_gray() +
  theme(legend.position = c(0.9, 0.8)) +
  labs(x = "", y = "Density") +
  scale_fill_manual(guide = guide_legend(), name =  "Integrated \nReadings",  
                    labels = c("Mean", "Standard Deviation","Kurtosis", "Skew"),
                    values = dense_colors[1:4])

ggplot(df_pulsar2) +
  geom_density(aes(x = DMSNR_mean, fill = dense_colors[5]), alpha = .2) +
  geom_density(aes(x = DMSNR_sd, fill = dense_colors[6]), alpha = .2) +
  geom_density(aes(x = DMSNR_exkur, fill = dense_colors[7]), alpha = .2) +
  geom_density(aes(x = DMSNR_skew, fill = dense_colors[8]), alpha = .2) +
  theme_grey() +
  theme(legend.position = c(0.9, 0.8)) +
  labs(x = "", y = "Density") +
  scale_fill_manual(guide = guide_legend(), name =  "DM-SNR \nReadings",  
                    labels = c("Mean", "Standard Deviation","Kurtosis", "Skew"), 
                    values = dense_colors[5:8])
################################################################################
library(matrixStats)
a <- colMeans(df_pulsar[,1:8])
b <- t(colQuantiles(as.matrix(df_pulsar[, 1:8])))
c <- t(colIQRs(as.matrix(df_pulsar[, 1:8])))

rbind(a, b, c)

knitr::kable(summary(df_pulsar))
################################################################################

df_pulsar %>% ggplot() + geom_boxplot(aes(x = Class, y = integ_mean))



a <- colMeans(df_data()[,1:8])
b <- t(colQuantiles(as.matrix(df_data()[, 1:8])))
c <- t(colIQRs(as.matrix(df_data()[, 1:8])))

r_names <- c("Means", "Min", "25%", "Median", "75%", "Max" ,"IQ Range")

summary_table <- data.frame(rbind(a, b, c), nrow = 8)
colnames(summary_table) <- c("Integrated Mean", "Integrated Standard Deviation", "Integrated Kurtosis", 
                             "Intergrated Skew", "DMSNR Mean", "DMSNR Standard Deviation", "DMSNR Kurtosis", "DMSNR Skew")

rownames(summary_table) <- r_names

summary_table

####################################################################################
selection_list = list("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4,
                      "PC5" = 5, "PC6" = 6, "PC7" = 7, "PC8" = 8)

new_selection_list = selection_list[names(selection_list) %in% "PC2" == FALSE]

print(new_selection_list)

##################################################################################
set.seed(1)

df_redux <- df_pulsar[sample(1:nrow(df_pulsar), size = nrow(df_pulsar) * 0.1),  c(1:3, 9)]

training <- sample(1:nrow(df_redux), size = nrow(df_redux) * 0.5)
testing <- dplyr::setdiff(1:nrow(df_redux), training)
pulsarTrain <- df_redux[training, ]
pulsarTest <- df_redux[testing, ]

trctrl <- trainControl(method = "repeatedcv", 
                       number = 5, 
                       repeats = 2)

log_fit <- train(Class ~ ., 
                 method = "glm", 
                 family = "binomial", 
                 data = pulsarTrain, 
                 trControl = trctrl)
log_fit
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(maps)
library(forcats)
library(MASS)
library(caret)
library(DT)
library(matrixStats)
library(GGally)
library(shinycssloaders)
library(AMR)

df_pulsar <- read_csv("./Data/HTRU_2.csv", col_names = FALSE)

var <- c("integ_mean","integ_sd","integ_exkur","integ_skew",
         "DMSNR_mean","DMSNR_sd","DMSNR_exkur","DMSNR_skew",
         "Class")

names(df_pulsar) <- var

df_pulsar <- df_pulsar %>% 
    mutate(Class = ifelse(Class == 1, "Pulsar", "Non Pulsar"))

df_pulsar$Class <- as.factor(df_pulsar$Class)

df_pulsar2 <- df_pulsar %>% mutate_at(names(df_pulsar)[1:8], ~(scale(.) %>% as.vector))

var <- names(df_pulsar)

proper_names <- c("Integrated Mean", "Integrated Standard Deviation", 
                  "Integrated Kurtosis", "Intergrated Skew",
                  "DMSNR Mean", "DMSNR Standard Deviation", "DMSNR Kurtosis",
                  "DMSNR Skew")

names_list <- list("integ_mean","integ_sd","integ_exkur","integ_skew",
                   "DMSNR_mean","DMSNR_sd","DMSNR_exkur","DMSNR_skew",
                   "Class")

names(names_list) <- c(proper_names , "Class")

options(spinner.color="#003f5c", spinner.color.background="#ffffff", spinner.size=2)

shinyUI(fluidPage(
    
    titlePanel("Pulsar Classification"),
    
    tabsetPanel(
        tabPanel("About",
                mainPanel(
                    h3("Pulsars and how they are measured"),
                    br(),
                    p("Pulsars are a type of fast spinnig and highly active form of a neutron star."),
                    imageOutput("pulsar_image"),
                    p("Source of image: https://www.esa.int/ESA_Multimedia/Images/2003/05/NGC_1952_Crab_Nebula_pulsar_imaged_by_the_NASA_ESA_Hubble_Space_Telescope"),
                    br(),
                    p("Thier spin can be measured and recorded. The purpose of this app is to be able to classify pulsars from non pulsar neutron stars.")
                 )
        ),
        tabPanel("Data",
                 sidebarLayout(
                     sidebarPanel(
                         h3("Interactive Data Table"),
                         p("Table can be filtered on variable values by \n using the slider popup under the variable name"),
                         br(),
                         p("Select which variables to view in the table"),
                         checkboxGroupInput("var_options", "Select Variables:"," ")
                     ),
                 mainPanel(
                     dataTableOutput("filterable_data_table")
                     )
                 )
        ),
        tabPanel("Exporatory Data Analysis",
                 sidebarLayout(
                     sidebarPanel(
        
                         h4("Select options below for exploratory analysis of pulsar data"),
                         br(),
                         radioButtons("plot_type",
                                      "Select the Plot Type",
                                      list("Scatter" = "A",
                                           "Density" = "B",
                                           "Pairs" = "C")),
                         br(),
                         h4("Select below to use standardized data"),
                         br(),
                         radioButtons("df_type",
                                      "Select Data Type",
                                      list("Raw" = "raw_pulsar_data",
                                           "Standardized" = "standard_pulsar_data")),
                         br(),
                         conditionalPanel(condition = "input.plot_type == 'A'",
                         br(),                  
                         h4("Choose Variables Below for Scatterplot"),
                         br(),
                         selectInput("var_sel1", "First Variable to Plot (X Axis)", 
                                     list("Integrated Mean" = "integ_mean", 
                                          "Integrated Standard Deviation" = "integ_sd", 
                                          "Integrated Kurtosis" = "integ_exkur", 
                                          "Intergrated Skew" = "integ_skew"), 
                                     selected = "Integrated Mean"),
                         
                         selectInput("var_sel2", "Second Variable to Plot (Y Axis)", 
                                      list("DMSNR Mean" = "DMSNR_mean", 
                                           "DMSNR Standard Deviation" = "DMSNR_sd", 
                                           "DMSNR Kurtosis" = "DMSNR_exkur", 
                                           "DMSNR Skew" = "DMSNR_skew"), 
                                     selected = "DMSNR Mean")),
                         downloadButton("downloadEDA", "Download data used in this Section")
                         
                     ),
                     
                     mainPanel(
                         conditionalPanel(condition = "input.plot_type == 'A'",
                            withSpinner(plotOutput("edaPlot", dblclick = "scplot_dblclick",
                                                   brush = brushOpts(id = "scplot_brush", 
                                                                     resetOnNew = TRUE)), 
                                        type = 5)),
                         conditionalPanel(condition = "input.plot_type == 'B'",
                            h4("Density Plot for the 4 Integrated Measurements"),
                            withSpinner(plotOutput("denPlot1"), type = 5),
                            h4("Density Plot for the 4 DM-SNR Measurements"),
                            withSpinner(plotOutput("denPlot2"), type = 5)),
                         conditionalPanel(condition = "input.plot_type == 'C'",
                            h4("Pairs Plot for Pulsar Data"),
                            withSpinner(plotOutput("pairsPlot"),type = 5)),                  
                         withSpinner(tableOutput("information"), type = 5)
                     )
                 )
        ),
        tabPanel("Deep Dive",
                 sidebarLayout(
                     sidebarPanel(radioButtons("dd_type",
                                               "Select Unsupervised Learning Type",
                                               list("K Means" = "A",
                                                    "PCA" = "B")),
                                  conditionalPanel(condition = "input.dd_type == 'A'",
                                                   sliderInput("k_clust", "Number of Clusters (K)", min = 2, max = 8, value = 3),
                                                   selectInput("km_sel1", "First Variable to Plot (X Axis)", 
                                                               list("Integrated Mean" = "integ_mean", 
                                                                    "Integrated Standard Deviation" = "integ_sd", 
                                                                    "Integrated Kurtosis" = "integ_exkur", 
                                                                    "Intergrated Skew" = "integ_skew"), 
                                                               selected = "Integrated Mean"),
                                                   
                                                   selectInput("km_sel2", "Second Variable to Plot (Y Axis)", 
                                                               list("DMSNR Mean" = "DMSNR_mean", 
                                                                    "DMSNR Standard Deviation" = "DMSNR_sd", 
                                                                    "DMSNR Kurtosis" = "DMSNR_exkur", 
                                                                    "DMSNR Skew" = "DMSNR_skew"), 
                                                               selected = "DMSNR Mean")),
                                  conditionalPanel(condition = "input.dd_type == 'B'",
                                                   radioButtons("pca_plot_type", "Select a PCA Plot",
                                                                list("Biplot" = "A",
                                                                     "Screeplot" = "B",
                                                                     "Screeplot (Cumulative)" = "C")),
                                  ),
                                  br(),
                                  conditionalPanel(condition = "input.dd_type == 'B' & input.pca_plot_type == 'A'",
                                                   selectInput("PCA_pick1", "Choose a Principal Component",
                                                               list("PC1" = 1,
                                                                    "PC2" = 2,
                                                                    "PC3" = 3,
                                                                    "PC4" = 4,
                                                                    "PC5" = 5,
                                                                    "PC6" = 6,
                                                                    "PC7" = 7,
                                                                    "PC8" = 8), selected = "PC1"),
                                                   br(),
                                                   selectInput("PCA_pick2", "Choose a 2nd Principal Component",
                                                               "", selected = "PC2"),
                                  ),
                                  ),
                     mainPanel(
                         conditionalPanel(condition = "input.dd_type == 'A'",
                                          withSpinner(plotOutput("kmeans_plot", dblclick = "kplot_dblclick",
                                                                 brush = brushOpts(id = "kplot_brush", 
                                                                                   resetOnNew = TRUE)), 
                                                                 type = 5)),
                         conditionalPanel(condition = "input.dd_type == 'B' & input.pca_plot_type == 'A'",
                                          withSpinner(plotOutput("PCA_biplot"), type = 5)),
                         conditionalPanel(condition = "input.dd_type == 'B' & input.pca_plot_type == 'B'",
                                          withSpinner(plotOutput("PCA_scree"), type = 5)),
                         conditionalPanel(condition = "input.dd_type == 'B' & input.pca_plot_type == 'C'",
                                          withSpinner(plotOutput("PCA_scree_cum"), type = 5)),
                         conditionalPanel(condition = "input.dd_type == 'B'",
                                          withSpinner(tableOutput("PCA_tab"), type = 5)),
                         verbatimTextOutput("PCA_text")
                     )
                 )
        ),
        tabPanel("Modeling the Data",
                 sidebarLayout(
                     sidebarPanel(
                         checkboxGroupInput("mod_var_opt", "Select Variables for Model Fitting:", 
                                            names_list[1:8], selected = var[1:8]),
                         numericInput("train_split", "Training Data Split", 
                                      min = 0, max = 1, step = 0.05, value = 0.5),
                         numericInput("seed_set", "Set Seed Value", 
                                      min = 1, step = 1, value = 100),
                         numericInput("pop_redux", "Data Reduction Multiplier \n 
                                      (choosing 1 will use all available data)", 
                                      min = 0, max = 1, step = 0.05, value = 0.1),
                         actionButton("model_prep", "Create Test and Train splits", class = "btn-success"),
                         br(),
                         br(),
                         selectInput("method_opt", "Cross-Validation Methods", list("Repeat CV" = "repeatedcv",
                                                                                    "CV" = "cv",
                                                                                    "LOOCV" = "LOOCV")),
                         numericInput("k_fold", "K folds for Cross Validation", 
                                      min = 1, max = 10, value = 5, step = 1),
                         numericInput("cv_repeats", "Number of Repeats", 
                                      min = 1, max = 5, value = 2, step = 1),
                         actionButton("tc_update", "Update Train Control Parameters", class = "btn-success"),
                         br(),
                         h3("Model Arguments"),
                         numericInput("log_thresh", "Logistic Probability Threshhold", 
                                      min = 0, max = 1, step = 0.05, value = 0.5),
                         numericInput("max_k", "Maximum K value for KNN", 
                                      min = 2, max = 25, value = 10, step = 1),
                         numericInput("max_mtry", "Maximum Number of Variables for Random Forest", 
                                      min = 1, max = 8, value = 8, step = 1),
                         p("Using a maximum of 8 variables produces a bagging model"),
                         br(),
                         checkboxGroupInput("model_sel", "Select Models to be created", list("Logistic Regression" = "glm",
                                                                                             "KNN Analysis" = "knn",
                                                                                             "Ensemble Method" = "rf"),
                                            selected = c("glm", "knn", "rf")),
                         actionButton("run_model", "Create Models", class = "btn-success")
                         ),
                     mainPanel(
                         tabsetPanel(type = "tabs",
                                     tabPanel("Model Information", 
                                              p("There will be information here")),
                                     tabPanel("Model Fitting", 
                                              verbatimTextOutput("trnctrl"),
                                              verbatimTextOutput("train_rows"),
                                              dataTableOutput("pulsar_redux"),
                                              withSpinner(verbatimTextOutput("logFit"), type = 5),
                                              withSpinner(verbatimTextOutput("knnFit"), type = 5)),
                                     tabPanel("Prediction on Test Data", 
                                              p("There will be information here"))
                         )
                 )
             )
        )
))

)
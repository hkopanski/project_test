#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
    
df_pulsar <- df_pulsar %>% mutate(Class = ifelse(Class == 1, "Pulsar", "Non Pulsar"))
    
df_pulsar$Class <- as.factor(df_pulsar$Class)

df_pulsar2 <- df_pulsar %>% mutate_at(names(df_pulsar)[1:8], ~(scale(.) %>% as.vector))

names_list <- list("integ_mean","integ_sd","integ_exkur","integ_skew",
                   "DMSNR_mean","DMSNR_sd","DMSNR_exkur","DMSNR_skew",
                   "Class")

proper_names <-  c("Integrated Mean", "Integrated Standard Deviation", 
                   "Integrated Kurtosis", "Intergrated Skew", "DMSNR Mean", 
                   "DMSNR Standard Deviation", "DMSNR Kurtosis", "DMSNR Skew")

names(names_list) <- c(proper_names , "Class")

proper_names1 <- c("Integrated Mean", "Integrated Standard Deviation", 
                  "Integrated Kurtosis", "Intergrated Skew")

proper_names2 <- c("DMSNR Mean", "DMSNR Standard Deviation", "DMSNR Kurtosis",
                   "DMSNR Skew")

dense_colors <- c("#003f5c", "#2f4b7c", "#665191", "#a05195", 
                  "#d45087", "#f95d6a", "#ff7c43", "#ffa600")

kplot_colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
                  "#ff7f00", "#ffff33", "#a65628", "#f781bf")

PCA_pulsar <- df_pulsar %>% dplyr::select("integ_mean":"DMSNR_skew") %>% 
  prcomp(., scale = TRUE)

df_pca_plot <- as_tibble(cbind(1:8, PCA_pulsar$sdev^2))

PCA_tab <- data.frame(PCA_pulsar$rotation, 
                      row.names = proper_names)

colnames(df_pca_plot) <- c("PCA", "variance")

##################################################################################

shinyServer(function(input, output, session) {
  
  output$pulsar_image <- renderImage({ 
    return(list(src = "./Data/crab_pulsar.png",
                contentType = "image/png",
                width = 300,
                height = 300)
           )
  }, deleteFile = FALSE)
  
  df_data <- reactive({
                  switch(input$df_type,
                         "raw_pulsar_data" = df_pulsar,
                         "standard_pulsar_data" = df_pulsar2)
  })
  
  updateCheckboxGroupInput(session, "var_options", 
                           choices = names_list, selected = var)
  
  output$dynamic_value <- renderPrint({
    is.vector(input$var_options)
  })
  
  output$filterable_data_table <- renderDataTable(
      datatable(df_pulsar[ , input$var_options, drop = FALSE], 
                filter = 'top', options = list(pageLength = 25, autoWidth = TRUE)
                )
    )
  
  output$downloadEDA <- downloadHandler(
    filename = function() {
      paste(input$df_type, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df_data(), file, row.names = FALSE)
    }
  )
  
  sc_ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$scplot_dblclick, {
    sc_brush <- input$scplot_brush
    if (!is.null(sc_brush)) {
      sc_ranges$x <- c(sc_brush$xmin, sc_brush$xmax)
      sc_ranges$y <- c(sc_brush$ymin, sc_brush$ymax)
      
    } else {
      sc_ranges$x <- NULL
      sc_ranges$y <- NULL
    }
  })
  
  output$edaPlot <- renderPlot({
        
        var1 <- which(var == input$var_sel1)
        var2 <- which(var == input$var_sel2)
      
        df_data() %>% rename(x = var[var1], y = var[var2]) %>%
             ggplot() + geom_point(aes(x = x, y = y, col = Class), size = 0.5) + 
             labs(x = proper_names1[var1], 
                  y = proper_names2[var2 - 4],
                  title = paste(proper_names2[var2 - 4],
                                "vs" ,
                                proper_names1[var1])) +
             theme(legend.position = c(0.9, 0.9)) +
             scale_color_manual(values = c("Pulsar"     = dense_colors[1],
                                           "Non Pulsar" = dense_colors[7])) +
             coord_cartesian(xlim = sc_ranges$x, ylim = sc_ranges$y, expand = FALSE)
        
       
    })
  
  output$denPlot1 <- renderPlot({
    
    ggplot(df_data()) +
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
    
  })
  
  output$denPlot2 <- renderPlot({
    
    ggplot(df_data()) +
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
    
  })
  
  output$pairsPlot <- renderPlot({
    ggpairs(df_data(), mapping = ggplot2::aes(color = Class))
  })
    
  output$small_tab <- renderDataTable({
      
            if(input$df_type == "A"){df_data = df_pulsar} else {df_data = df_pulsar2 }
      
            df_data %>%
            head() %>%
            datatable()
    })
    
    output$information <- renderTable({
      a <- colMeans(df_data()[,1:8])
      b <- t(colQuantiles(as.matrix(df_data()[, 1:8])))
      c <- t(colIQRs(as.matrix(df_data()[, 1:8])))
      
      r_names <- c("Means", "Min", "25%", "Median", "75%", "Max" ,"IQ Range")
      
      summary_table <- data.frame(rbind(a, b, c))
      colnames(summary_table) <- proper_names
      
      rownames(summary_table) <- r_names
      
      summary_table
    }, rownames = TRUE)
    
    
    k_ranges <- reactiveValues(x = NULL, y = NULL)
    
    observeEvent(input$kplot_dblclick, {
      k_brush <- input$kplot_brush
      if (!is.null(k_brush)) {
        k_ranges$x <- c(k_brush$xmin, k_brush$xmax)
        k_ranges$y <- c(k_brush$ymin, k_brush$ymax)
        
      } else {
        k_ranges$x <- NULL
        k_ranges$y <- NULL
      }
    })
    
    
    output$kmeans_plot <- renderPlot ({
      set.seed(800)
      
      plot_var <- c(input$km_sel1, input$km_sel2, "Class")
      
      km_pulsar <- kmeans(df_pulsar[ , 1:8], input$k_clust, nstart = 20, iter.max = 50)
      
      df_kplot <- as_tibble(cbind(df_pulsar[ , plot_var], km_pulsar$cluster))
      
      names(df_kplot)[4] <- "cluster"
      
      #df_kplot %>% print()
      
      df_kplot %>% rename(x = plot_var[1], y = plot_var[2]) %>% 
        ggplot(aes(x = x, y = y, col = as_factor(cluster), shape = Class)) + 
        geom_point() + 
        scale_color_manual(values = list("1" = kplot_colors[1], 
                                         "2" = kplot_colors[2],
                                         "3" = kplot_colors[3],
                                         "4" = kplot_colors[4],
                                         "5" = kplot_colors[5],
                                         "6" = "black",
                                         "7" = kplot_colors[7],
                                         "8" = kplot_colors[8]),
                           name = "Clusters") +
        scale_shape_manual(values = list("Pulsar" = 19, 
                                         "Non Pulsar" = 3)) +
        #theme_minimal() +
        labs(x = proper_names[which(plot_var[1] == names(df_pulsar))], 
             y = proper_names[which(plot_var[2] == names(df_pulsar))],
             title = paste(proper_names[which(plot_var[2] == names(df_pulsar))], "vs",
                           proper_names[which(plot_var[1] == names(df_pulsar))], "using",
                           input$k_clust, "Clusters")) + 
        coord_cartesian(xlim = k_ranges$x, ylim = k_ranges$y, expand = FALSE)
    })
    
    
    
    output$PCA_biplot <- renderPlot({
      
      a_val <- length(numeric(input$PCA_pick1))
      b_val <- length(numeric(input$PCA_pick2))
      
      ggplot_pca(PCA_pulsar, 
                 choices = c(a_val, b_val), 
                 points_size = 1,
                 points_alpha = 0.15,
                 arrows = TRUE,
                 arrows_colour = dense_colors[5],
                 arrows_size = 2,
                 arrows_textsize = 6,
                 arrows_textangled = TRUE,
                 arrows_alpha = 0.75,
                 base_textsize = 12)
    })
    
    output$PCA_scree <- renderPlot ({
      
      df_pca_plot %>% ggplot(aes(x = PCA, y = variance/sum(variance))) + 
        geom_point(shape = 5, col = dense_colors[5]) + 
        geom_line(col = dense_colors[5]) + labs(x = "Principal Component", 
                                                y = "Proportion of Variance Explained",
                                                title = "Proportion of Variance") +
        geom_text(aes(x = PCA, 
                      y = variance/sum(variance), 
                      label = round(variance/sum(variance), 4),
                      hjust = -0.25,
                      vjust = -0.9)) + 
        scale_x_continuous(breaks = seq(0, 8, 1))
    
    })
    
    output$PCA_scree_cum <- renderPlot ({  
        
      df_pca_plot %>% ggplot(aes(x = PCA, y = cumsum(variance/sum(variance)))) + 
        geom_point(shape = 5, col = dense_colors[2]) + 
        geom_line(col = dense_colors[2]) + labs(x = "Principal Component", 
                                                y = "Cumulative Proportion of Variance Explained",
                                                title = "Cumulative Proportion Variance") +
        geom_text(aes(x = PCA, 
                      y = cumsum(variance/sum(variance)), 
                      label = round(cumsum(variance/sum(variance)), 4),
                      hjust = 1,
                      vjust = -2)) +
        ylim(NA, 1.025) + 
        scale_x_continuous(breaks = seq(0, 8, 1))
    })
    
    output$PCA_tab <- renderTable({
      PCA_tab
    }, rownames = TRUE)
    
    observeEvent(input$PCA_pick1, {
      selection_list <- list("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4,
                             "PC5" = 5, "PC6" = 6, "PC7" = 7, "PC8" = 8)
      
      new_selection_list <- selection_list[-length(numeric(input$PCA_pick1))]
      
      updateSelectInput(session, "PCA_pick2", choices = new_selection_list)
    })
    
    #Model Fitting Section
    
    df_redux <- eventReactive(input$model_prep, {
      set.seed(input$seed_set)
      df_pulsar[sample(1:nrow(df_pulsar), size = nrow(df_pulsar) * input$pop_redux),
                c(input$mod_var_opt, "Class"), drop = FALSE]
    })
  
    training <- eventReactive(input$model_prep, {
      set.seed(input$seed_set)
      sample(1:nrow(df_redux()), size = nrow(df_redux()) * input$train_split)
      })
    
    testing <- eventReactive(input$model_prep, {
      set.seed(input$seed_set)
      dplyr::setdiff(1:nrow(df_redux()), training())
    })
    
    df_train <- eventReactive(input$model_prep, {
      df_redux()[training() ,]
    })
    
    df_test <- eventReactive(input$model_prep, {
      df_redux()[testing() ,]
    })
    
    train_ctrl <- eventReactive(input$tc_update, {
      trainControl(method = input$method_opt, 
                   number = input$k_fold, 
                   repeats = input$cv_repeats)
    })
    
    log_fit <- eventReactive(input$run_model, {
      
      set.seed(input$seed_set)
      
      train(Class ~ .,
            method = "glm", 
            family = "binomial", 
            data = df_train(), 
            trControl = train_ctrl())
    })
    
    knn_fit <- eventReactive(input$run_model, {
      
      set.seed(input$seed_set)
      
      train(Class ~ ., 
            data = df_train(),
            method = "knn", 
            trControl = train_ctrl(), 
            preProcess = c("center", "scale"), 
            tuneGrid = data.frame(k = 2:input$max_k))
    })
    
    output$trnctrl <- renderPrint({
      
      if (is.null(train_ctrl())){
        
        return()
        
        } else {
          
      paste("Method:", train_ctrl()$method, "|","Number of K folds:", train_ctrl()$number, "|","Number of Repeats",train_ctrl()$repeats)
      
        }
          
    })
    
    output$train_rows <- renderPrint({
      
      if (is.null(training())){
        
        return()
        
      } else {
        
        length(training())
        
      }
      
    })
    
    output$pulsar_redux <- renderDataTable({
      if (is.null(df_redux())){
        
        return()
        
      } else {
        
        df_redux()
        
      }
      
    }, options = list(pageLength = 5, autoWidth = TRUE))
    
    output$logFit <- renderPrint({
      
      if (is.null(log_fit())){
        
        return()
        
      } else {
        
        log_fit()
        
      }
      
    })
    
    output$knnFit <- renderPrint({
      
      if (is.null(knn_fit())){
        
        return()
        
      } else {
        
        knn_fit()
        
      }
      
    })

})

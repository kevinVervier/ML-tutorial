#--------------------#
# ML tutorial server #
#--------------------#
library(shiny)
library(glmnet)
library(DT)
library(randomForest)
library(LiblineaR)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  #read data function
  read_dataset <- reactive({
    read.csv(input$your_data$datapath,row.names = 1)
  })
  
  # selected data set
  output$text1 <- renderText({ 
    paste("You have selected ", input$your_data[1],".",sep='')
  })
  # print data dimensions
  output$text_dim <- renderText({ 
    data = read_dataset()
    paste(dim(data)[1], "rows and", dim(data)[2],"columns.")
  })
  # load and plot the data
  output$contents <- renderDataTable({
    inFile <- input$your_data
    if(is.null(inFile)) return(NULL)
    data = read_dataset()#read.csv(inFile$datapath,row.names = 1)
    # data = cbind(Row.names = rownames(data), data)
    return(data)
  },options = list(pageLength = 10))
  
  # get data summary
  output$summary <- renderPrint({
    if(is.null(input$your_data)) return(NULL)
    dataset <- read_dataset()
    summary(dataset)
  })
  
  #warning message if PCA tried with regression problem
  output$warning_reg <-renderText({
    data = read_dataset()
    if(length(unique(data[,1])) > 20) paste("Warning: Your dataset seems to be related to a regression problem (or have more than 20 classes)! No PCA then...")
  })
  
  # create pca object
  pca_objects <- reactive({
    data = read_dataset()
    pca_output <- prcomp(data[,-1], 
                         center = TRUE, 
                         scale. = TRUE)
    return(list(pcs_df = cbind(data[,-1],pca_output$x),
                pca_output = pca_output,
                grouping=factor(data[,1])))
  })
  
  # select the PC on x-axis
  output$the_pcs_to_plot_x <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_x", 
                label = "X axis:",
                choices= colnames(pca_output), 
                selected = 'PC1')
  })
  
  # select the PC on y-axis
  output$the_pcs_to_plot_y <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_y", 
                label = "Y axis:",
                choices= colnames(pca_output), 
                selected = 'PC2')
  })
  
  # plot for PCA
  pca_biplot <- reactive({
    tmp <-  pca_objects()
    pca_output <- tmp$pca_output
    groups <-  tmp$grouping
    
    var_expl_x <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))]^2/sum(pca_output$sdev^2), 1)
    var_expl_y <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))]^2/sum(pca_output$sdev^2), 1)
    
    # plot with grouping variable
    if(!input$show_points){ 
      pc_plot_groups  <- ggplot(tmp$pcs_df, aes_string(input$the_pcs_to_plot_x, 
                                                       input$the_pcs_to_plot_y, 
                                                       fill = groups, 
                                                       colour = groups
      )) + 
        stat_ellipse(geom = "polygon", alpha = 0.1) +
        
        theme_bw(base_size = 14) +
        scale_colour_discrete(guide = FALSE) +
        guides(fill = guide_legend(title = "groups")) +
        theme(legend.position="top") +
        coord_equal() +
        xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
        ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
    }else{
      pc_plot_groups  <- ggplot(tmp$pcs_df, aes_string(input$the_pcs_to_plot_x, 
                                                       input$the_pcs_to_plot_y
      )) + 
        geom_point(aes(colour = groups)) +
        
        theme_bw(base_size = 14) +
        scale_colour_discrete(guide = FALSE) +
        guides(colour = guide_legend(title = "groups")) +
        theme(legend.position="top") +
        coord_equal() +
        xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
        ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
    }
    # the plot
    pc_plot_groups
    
  })
  
  #biplot object
  output$pca <- renderPlot({
    pca_biplot() 
    
  })
  
  # select nfolds for cross-validation
  output$nfolds <- renderUI({
    selectInput(inputId = "nfolds", 
                label = "Number of CV folds",
                choices= c('5','10','LOOCV'), 
                selected = '5')
  })
  
  # fix random seed
  output$seed <- renderUI({
    textInput(inputId="seed", label="Random seed (for reproducible research)", value = "42")
  })
  
  # function to draw fold ID
  drawFold <- reactive({
    if(is.null(input$seed)){ 
      seed = 42
    }else{
      seed = as.integer(input$seed)
    }
    set.seed(seed)
    
    if(is.null(input$your_data)) return(NULL)
    data = read_dataset()
    
    if(is.null(input$nfolds)){
      nfolds = 5
    }else{
      if(input$nfolds != 'LOOCV'){
        nfolds = as.integer(input$nfolds)
      }else{
        nfolds = nrow(data)
        return(sample(1:nfolds,nrow(data),replace=FALSE))
      }
    }
    
    return(sample(1:nfolds,nrow(data),replace=TRUE))
    
  })
  
  
  #draw data table with different colors for CV folds
  output$CVtable <- DT::renderDataTable({
    #load data
    inFile <- input$your_data
    if(is.null(inFile)) return(NULL)
    if(is.null(input$nfolds)) return(NULL)
    
    data = read_dataset()
    
    if(input$nfolds != 'LOOCV'){ 
      #get random folds
      nfolds = as.integer(input$nfolds)
      
      foldId = drawFold()
      data = cbind(Folds = foldId, data)
      data = datatable(data)
      #  )%>% 
      #######
      # TODO: find a better fix
      if(nfolds == 5 ){
        formatStyle(data,'Folds', backgroundColor = styleEqual(1:5, c("#8DD3C7","#FFFFB3", "#BEBADA", "#FB8072" ,"#80B1D3")))
      }else{
        formatStyle(data,'Folds', backgroundColor = styleEqual(c(paste('',1:9),"10"), c("#8DD3C7","#FFFFB3", "#BEBADA", "#FB8072" ,"#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5" ,"#D9D9D9" ,"#BC80BD")))
      }
    }else{
      foldId = drawFold()
      data = cbind(Folds = foldId, data)
      data = datatable(data)
      data
    }
  }) 
  
  #select regression variables for correlation matrix
  output$iv1 = renderUI({
    if(is.null(input$your_data)) return(NULL)
    data = read_dataset()
    checkboxGroupInput('iv1', h5('Input Variables'), choices = colnames(data),selected = colnames(data)[-1])
  })
  
  #function computing correlation matrix
  corr_mat <- reactive({
    if(is.null(input$your_data)) return(NULL)
    if(is.null(input$iv1)) return(NULL)
    data = read_dataset()
    d <- data[,input$iv1]
    cor <- as.data.frame(round(cor(d), 2))
    cor <- cbind(Variables = rownames(cor), cor)
    return(cor)
    
  })
  
  # correlation matrix
  output$corr <- renderDataTable({
    corr_mat()
  })
  
  
  #select input regression variables for fitting model
  output$iv2 = renderUI({
    if(is.null(input$your_data)) return(NULL)
    data = read_dataset()
    checkboxGroupInput('iv2', h5('Input Variables'), choices = colnames(data),selected = colnames(data)[2])
  })
  # select output regression variable for fitting model
  output$dv <- renderUI({
    if(is.null(input$your_data)) return(NULL)
    data = read_dataset()
    # drop down selection
    selectInput(inputId = "dv", 
                label = "Output Variable",
                choices= colnames(data), 
                selected = colnames(data)[1])
  })
  
  
  #fit regression model
  # regression formula
  regFormula <- reactive({
    if(length(input$iv2) > 1){
      as.formula(paste(input$dv, '~', paste(input$iv2, collapse = '+') ))
    }else{
      as.formula(paste(input$dv, '~', input$iv2))
    }
  })
  
  # multivariate model
  model <- reactive({
    lm(regFormula(), data = read_dataset())
  })
  
  # multivariate model
  output$model <- renderPrint({
    if(is.null(input$dv)) return(NULL)
    if(is.null(input$iv2)) return(NULL)
    summary(model())
  })
  
  # multivariate model properties
  output$model_plot <- renderPlot({
    if(is.null(input$dv)) return(NULL)
    if(is.null(input$iv2)) return(NULL)
    par(mfrow=c(2,2))
    plot(model())
  })
  
  #alpha range slider
  output$alpha_range <- renderUI({
    sliderInput("alpha_range", 
                label = "Alpha values range:",
                min = 0, max = 1, value = c(0, 1))
  })
  # step
  output$alpha_step <- renderUI({
    textInput(inputId="alpha_step", label="by", value = "0.05")
  })
  
  # Regularization part
  # Using Cross-validated fold
  
  cv_reg <- reactive({
    #load data
    if(is.null(input$your_data)) return(NULL)
    data = read_dataset()
    #get foldId if any
    if(is.null(input$nfolds)){
      nfolds = 5
    }else{
      if(input$nfolds != 'LOOCV'){
        nfolds = as.integer(input$nfolds)
      }else{
        nfolds = nrow(data)
      }
    }
    foldId = drawFold()
    
    #define alpha vector
    if(is.null(input$alpha_range)) return(NULL)
    if(is.null(input$alpha_step)) return(NULL)
    
    alphas <- seq(as.numeric(input$alpha_range[1]), as.numeric(input$alpha_range[2]), by=as.numeric(input$alpha_step))
    
    #define output
    mses <- numeric(length(alphas))
    
    #loop over alpha values
    for(i in 1:length(alphas)){
      if(nfolds != nrow(data)){
        cvfits <- cv.glmnet(x=as.matrix(data[,-1]), y=data[,1], alpha=alphas[i], nfolds=nfolds,foldid=foldId)
      }else{
        #LOOCV
        cvfits <- cv.glmnet(x=as.matrix(data[,-1]), y=data[,1], alpha=alphas[i], nfolds=nfolds,grouped=FALSE)
      }
      loc <- which(cvfits$lambda==cvfits$lambda.min)
      mses[i] <- cvfits$cvm[loc]
    }
    this <- data.frame(mse=mses, alpha=alphas)
    
    #     #get the regular linear regression LOOCV
    #     errors <- numeric(nrow(data))
    #     for(i in 1:nrow(data)){
    #       train <- data[-i,]
    #       test <- data[i,]
    #       kitchen.sink <- glm(paste0(colnames(data)[1],'~.'),data=train)
    #       the.pred <- predict(kitchen.sink, newdata= test)
    #       errors[i] <- (the.pred - test[1])^2
    #     }
    #     kitchen.sink = mean(errors)
    #     
    #     other.errors <- data.frame(method=c("kitchen sink"),
    #                                errors=c(kitchen.sink))
    #     
    plot1 <- ggplot(this, aes(x=alpha, y=mse)) +
      geom_point(shape=1) +
      ylab("CV mean squared error") +
      xlab("alpha parameter") +
      ggtitle("model error of highest performing regularized elastic-net
           regression as a function of alpha parameter")  #+ 
    #       #add kitchen sink
    #       geom_hline(aes(yintercept=errors,
    #                        color=method, group=method),
    #                    size=2, data=other.errors, show.legend=TRUE)
    
    #plot1
    
    #get best model among alpha-range
    best.alpha = alphas[tail(which(mses == min(mses)),1)]
    #retrain model with best_alpha
    if(nfolds != nrow(data)){
      cvfits <- cv.glmnet(x=as.matrix(data[,-1]), y=data[,1], alpha=best.alpha, nfolds=nfolds,foldid=foldId)
    }else{
      #LOOCV
      cvfits <- cv.glmnet(x=as.matrix(data[,-1]), y=data[,1], alpha=best.alpha, nfolds=nfolds,grouped=FALSE)
    }
    
    #get preds
    preds = predict(cvfits, newx = as.matrix(data[,-1]), s = "lambda.min")
    
    return(list('ref'=data[,1],'pred'=preds,'cv.perfs'=plot1,'best_alpha'=best.alpha,'weights'=coef(cvfits, s = "lambda.min")))
    
    
  })
  
  #render cv reg model
  output$regul_model <- renderPlot({
    res=cv_reg()
    res$cv.perfs
  })
  
  output$regul_feature_selec <- renderPrint({
    perfs = cv_reg()
    cat('Estimated feature weights. A dot means that the feature is not included in the model.\n')
    cat('Those performances have been obtained with parameter alpha =',perfs$best_alpha,'\n')
    print(perfs$weights)
  })
  
  ###############
  # Classification
  
  #knn - choose number of neighbours
  output$knn <- renderUI({
    textInput(inputId="knn", label="Number of Neighbours", value = "1")
  })
  #knn - choose distance
  output$knn_dist <- renderUI({
    selectInput(inputId = "knn_dist", 
                label = "Distance",
                choices= c('euclidean','manhattan','maximum','canberra','binary'), 
                selected = 'euclidean')
  })
  
  #knn compute distance
  knn_compute <- reactive({
    #load data
    if(is.null(input$your_data)) return(NULL)
    data = read_dataset()
    #get foldId if any
    if(is.null(input$nfolds)){
      nfolds = 5
    }else{
      if(input$nfolds != 'LOOCV'){
        nfolds = as.integer(input$nfolds)
      }else{
        nfolds = nrow(data)
      }
    }
    foldId = drawFold()
    
    #define alpha vector
    if(is.null(input$knn)) return(NULL)
    if(is.null(input$knn_dist)) return(NULL)
    
    #define output
    preds <- numeric(nrow(data))
    #compute distance matrix
    d <- as.matrix(dist(data[,-1],method = input$knn_dist))
    #define number of neighbours
    k = as.integer(input$knn) 
    
    #init preds vector
    preds = rep(0,nrow(data))
    
    #loop over folds
    for(i in 1:nfolds){
      idx = which(foldId == i)
      
      # init
      n = length(idx)
      knn.mat = matrix(0, ncol = k, nrow = n)
      p <- NULL
      #loop over test
      for(j in 1:n){
        knn.mat[j,] = order(d[idx[j],-idx])[1:k]
        labels = data[-idx,1][knn.mat[j,]]
        tmp = table(labels)
        nties = length(which(tmp == max(tmp)))
        if(nties == 1){
          p <- c(p,names(which.max(tmp)))
        }else{
          p <- c(p,sample(x = names(tmp[which(tmp == max(tmp))]),size = 1))
        }
      }
      preds[idx] = p
    }
    return(list('ref'=data[,1],'preds'=preds))
  })
  
  #compute knn performance
  output$knn_perfs <- renderPrint({
    if(is.null(input$knn_dist)) return(NULL)
    if(is.null(input$knn)) return(NULL)
    perfs = knn_compute()
    cm = as.matrix(table(Actual = perfs$ref, Predicted = perfs$preds)) # create the confusion matrix
    cat('Confusion matrix:\n')
    print(cm)
    n = sum(cm) # number of instances
    nc = nrow(cm) # number of classes
    diag = diag(cm) # number of correctly classified instances per class 
    rowsums = apply(cm, 1, sum) # number of instances per class
    colsums = apply(cm, 2, sum) # number of predictions per class
    p = rowsums / n # distribution of instances over the actual classes
    q = colsums / n # distribution of instances over the predicted classes
    accuracy = sum(diag) / n #accuracy
    cat('Accuracy:\n')
    print(accuracy)
    
    #per-class information
    precision = diag / colsums 
    recall = diag / rowsums 
    f1 = 2 * precision * recall / (precision + recall) 
    cat('Per-class information:\n')
    print(data.frame(precision, recall, f1)) 
    
  })
  
  #Logistic regression
  output$alpha_range_log <- renderUI({
    sliderInput("alpha_range_log", 
                label = "Alpha values range:",
                min = 0, max = 1, value = c(0, 1))
  })
  # step
  output$alpha_step_log <- renderUI({
    textInput(inputId="alpha_step_log", label="by", value = "0.05")
  })
  
  
  cv_log_reg <- reactive({
    #load data
    if(is.null(input$your_data)) return(NULL)
    data = read_dataset()
    #get foldId if any
    if(is.null(input$nfolds)){
      nfolds = 5
    }else{
      if(input$nfolds != 'LOOCV'){
        nfolds = as.integer(input$nfolds)
      }else{
        nfolds = nrow(data)
      }
    }
    foldId = drawFold()
    
    #define alpha vector
    
    if(is.null(input$alpha_range_log)) return(NULL)
    if(is.null(input$alpha_step_log)) return(NULL)
    alphas <- seq(as.numeric(input$alpha_range_log[1]), as.numeric(input$alpha_range_log[2]), by=as.numeric(input$alpha_step_log))
    
    #define output
    mses <- numeric(length(alphas))
    
    #loop over alpha values
    for(i in 1:length(alphas)){
      if(nfolds != nrow(data)){
        cvfits <- cv.glmnet(x=as.matrix(data[,-1]), y=data[,1], alpha=alphas[i], nfolds=nfolds,foldid=foldId,family='multinomial',type.measure="class") #including binomial case
      }else{
        #LOOCV
        cvfits <- cv.glmnet(x=as.matrix(data[,-1]), y=data[,1], alpha=alphas[i], nfolds=nfolds,grouped=FALSE,family='multinomial',type.measure="class")
      }
      loc <- which(cvfits$lambda==cvfits$lambda.min)
      mses[i] <- cvfits$cvm[loc]
    }
    this <- data.frame(mse=mses, alpha=alphas)
    
    plot1 <- ggplot(this, aes(x=alpha, y=mse)) +
      geom_point(shape=1) +
      ylab("misclassification error (%)") +
      xlab("alpha parameter") +
      ggtitle("model error of highest performing regularized elastic-net
           logistic regression as a function of alpha parameter")
    # plot1
    
    #get best model among alpha-range
    best.alpha = alphas[tail(which(mses == min(mses)),1)]
    #retrain model with best_alpha
    if(nfolds != nrow(data)){
      cvfits <- cv.glmnet(x=as.matrix(data[,-1]), y=data[,1], alpha=best.alpha, nfolds=nfolds,foldid=foldId,family='multinomial',type.measure="class") #including binomial case
    }else{
      #LOOCV
      cvfits <- cv.glmnet(x=as.matrix(data[,-1]), y=data[,1], alpha=best.alpha, nfolds=nfolds,grouped=FALSE,family='multinomial',type.measure="class")
    }
    
    #get preds
    preds = predict(cvfits, newx = as.matrix(data[,-1]), s = "lambda.min",type='class')
    
    return(list('ref'=data[,1],'pred'=preds,'cv.perfs'=plot1,'best_alpha'=best.alpha,'weights'=coef(cvfits, s = "lambda.min")))
    
  })
  
  #render cv Log-reg model
  output$log_regul_model_plot <- renderPlot({
    res = cv_log_reg()
    res$cv.perfs
  })
  
  #render best Log-reg model performances
  output$log_regul_model_perfs <- renderPrint({
    perfs = cv_log_reg()
    cat('Those performances have been obtained with parameter alpha =',perfs$best_alpha,'\n')
    cm = as.matrix(table(Actual = perfs$ref, Predicted = perfs$pred)) # create the confusion matrix
    cat('Confusion matrix:\n')
    print(cm)
    n = sum(cm) # number of instances
    nc = nrow(cm) # number of classes
    diag = diag(cm) # number of correctly classified instances per class 
    rowsums = apply(cm, 1, sum) # number of instances per class
    colsums = apply(cm, 2, sum) # number of predictions per class
    p = rowsums / n # distribution of instances over the actual classes
    q = colsums / n # distribution of instances over the predicted classes
    accuracy = sum(diag) / n #accuracy
    cat('Accuracy:\n')
    print(accuracy)
    
    #per-class information
    precision = diag / colsums 
    recall = diag / rowsums 
    f1 = 2 * precision * recall / (precision + recall) 
    cat('Per-class information:\n')
    print(data.frame(precision, recall, f1)) 
  })
  
  output$log_regul_feature_selec <- renderPrint({
    perfs = cv_log_reg()
    cat('Estimated feature weights. A dot means that the feature is not included in the model.\n')
    print(perfs$weights)
  })
  
  #Random forest parameters
  # ntree
  output$ntree <- renderUI({
    textInput(inputId="ntree", label="Number of Trees", value = "50")
  })
  # mtry
  output$mtry <- renderUI({
    data = read_dataset()
    textInput(inputId="mtry", label="Number of variables randomly sampled", value = round(sqrt(as.integer(ncol(data) - 1)))) # sqrt(p) is an heuristic provide by L. Breiman
  })
  
  #Random Forest model
  rf_train <-reactive({
    #load data
    if(is.null(input$your_data)) return(NULL)
    data = read_dataset()
    #no need for cross-validation, because of out-of-bag error
    
    if(is.null(input$mtry)) return(NULL)
    if(is.null(input$ntree)) return(NULL)
    mtry = as.integer(input$mtry)
    if(mtry > (ncol(data) -1)) mtry = ncol(data) - 1
    ntree = as.integer(input$ntree)
    
    #loop over alpha values
    m <- randomForest(x=as.matrix(data[,-1]), y=factor(data[,1]),ntree = ntree, mtry = mtry,proximity = TRUE,importance = TRUE)
    oob.preds <- predict(m)
    oob.err = sum(oob.preds != data[,1])/nrow(data)
    
    return(list('model'=m,'oob'=oob.err))
    
  })
  
  #render rf model
  output$rf_plot <- renderPlot({
    res = rf_train()
    data = read_dataset()
    labels = factor(data[,1])
    
    if(nlevels(labels) > 2){
      MDSplot(m,labels)
      legend("topleft", legend=levels(labels), fill=brewer.pal(length(levels(labels)), "Set1"))
    }else{
      MDSplot(m,labels,palette = rainbow(2))
      legend("topleft", legend=levels(labels), fill=rainbow(2))
    }
  })
  
  #compute rf performance
  output$rf_perfs <- renderPrint({
    perfs = rf_train()
    cm =perfs$m$confusion # create the confusion matrix
    cat('Confusion matrix:\n')
    print(cm)
    cat('Out-of-bag error:',perfs$oob,'%\n')
    
  })
  
  #compute rf feature importance
  output$rf_imp <- renderPrint({
    perfs = rf_train()
    cat('Feature importance:\n')
    print(data.frame("Gini Index"=sort(perfs$m$importance[,1],decreasing = TRUE)))
    
  })
  
  #SVM parameters
  # C
  output$C_svm_range <- renderUI({
    sliderInput("C_svm_range", 
                label = "C values range (log10):",
                min = -6, max = 6, value = c(-6,6))
  })
  # step
  output$C_svm_step <- renderUI({
    textInput(inputId="C_svm_step", label="by", value = "1")
  })
  
  # type
  output$svm_type <- renderUI({
    data = read_dataset()
    selectInput(inputId="svm_type", label="Loss-Regularizer combination", choices= c('L2-regul. Logistic Regression' = 0,
                                                                                     'L2-regul. Hinge' = 2,
                                                                                     'L1-regul. Hinge' = 5,
                                                                                     'L1-regul. Logistic Regression' = 6), selected = 0)
  })
  
  
  
  cv_svm <- reactive({
    #load data
    if(is.null(input$your_data)) return(NULL)
    data = read_dataset()
    #get foldId if any
    if(is.null(input$nfolds)){
      nfolds = 5
    }else{
      if(input$nfolds != 'LOOCV'){
        nfolds = as.integer(input$nfolds)
      }else{
        nfolds = nrow(data)
      }
    }
    
    #define C vector
    if(is.null(input$C_svm_range)) return(NULL)
    if(is.null(input$C_svm_step)) return(NULL)
    if(is.null(input$svm_type)) return(NULL)
    Cs <- 10^seq(as.numeric(input$C_svm_range[1]), as.numeric(input$C_svm_range[2]), by=as.numeric(input$C_svm_step))
    
    #define output
    mses <- numeric(length(Cs))
    
    #loop over C values
    for(i in 1:length(Cs)){
      m = LiblineaR(data = data[,-1],target = factor(data[,1]),type=as.integer(input$svm_type),cost=Cs[i],cross=nfolds)
      mses[i] <- 1- m
    }
    this <- data.frame(mse=mses, alpha=Cs)
    
    plot1 <- ggplot(this, aes(x=alpha, y=mse)) +
      geom_point(shape=1) + scale_x_log10() +
      ylab("misclassification error (%)") +
      xlab("C parameter") +
      ggtitle("model error of highest performing SVM as a function of C parameter")
    
    #get best model among alpha-range
    best_C = Cs[which.min(mses)]
    #need foldIds to do the final model training
    foldId = drawFold()
    
    #init preds vector
    preds = rep(0,nrow(data))
    #loop over folds
    for(i in 1:nfolds){
      idx = which(foldId == i)
      m = LiblineaR(data = data[-idx,-1],target = factor(data[-idx,1]),type=as.integer(input$svm_type),cost=best_C)
      preds[idx] = predict(m,data[idx,-1],proba= FALSE)$predictions
    }
    
    return(list('ref'=factor(data[,1]),'pred'= preds,'cv.perfs'=plot1,'best_C'=best_C))
    
  })
  
  #render cv Log-reg model
  output$svm_plot <- renderPlot({
    res = cv_svm()
    res$cv.perfs
  })
  
  #render best Log-reg model performances
  output$svm_perfs <- renderPrint({
    perfs = cv_svm()
    cat('Those performances have been obtained with parameter C =',perfs$best_C,'\n')
    cm = as.matrix(table(Actual = perfs$ref, Predicted = perfs$pred)) # create the confusion matrix
    cat('Confusion matrix:\n')
    print(cm)
    n = sum(cm) # number of instances
    nc = nrow(cm) # number of classes
    diag = diag(cm) # number of correctly classified instances per class 
    rowsums = apply(cm, 1, sum) # number of instances per class
    colsums = apply(cm, 2, sum) # number of predictions per class
    p = rowsums / n # distribution of instances over the actual classes
    q = colsums / n # distribution of instances over the predicted classes
    accuracy = sum(diag) / n #accuracy
    cat('Accuracy:\n')
    print(accuracy)
    
    #per-class information
    precision = diag / colsums 
    recall = diag / rowsums 
    f1 = 2 * precision * recall / (precision + recall) 
    cat('Per-class information:\n')
    print(data.frame(precision, recall, f1)) 
  })
  
  
})

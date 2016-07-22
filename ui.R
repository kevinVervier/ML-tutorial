#----------------#
# ML tutorial UI #
#----------------#
library(shiny)
library(ggplot2)
library(DT)

shinyUI(fluidPage(#theme = "bootstrap.css",
  
  titlePanel("ML tutorial"),
  
  navlistPanel(
    "Introduction",
    # warm-up
    tabPanel("Before starting...",mainPanel(
      h1('Few words before starting:'),
      p("Welcome to this tutorial."),
      p("I am glad that you are interested in learning Machine Learning!"),
      p("This tutorial aims at providing interactive tools for training your first machine learning models."),
      p("It is even possible to use your own data, but by default a public dataset will be used (e.g.",span(strong('mtcars')),"for regression,", span(strong('iris')), "for classification)."),
      p("First, we propose to load the data and do preliminary descriptive analysis.")
    )),
    #choose data to work on
    tabPanel("Load data",mainPanel(
      p('Please select your working data:'),
      fileInput('your_data', label='Select your favorite dataset', multiple = FALSE),
      p('NB: we assume that your dataset has the following properties:'),
      tags$ul(
        tags$li('The first column is the outcome/response variable.'),
        tags$li('The provided file uses .csv format.')
      ),
      textOutput("text1")
    )),
    # descriptive analysis
    tabPanel("Data viewer",mainPanel(
      p('The current data you are using contains:'),
      textOutput("text_dim"),
      dataTableOutput('contents')
    )),
    tabPanel("Data description",mainPanel(
      p('Current data summary:'),
      verbatimTextOutput("summary")
    )),
    tabPanel("PCA",mainPanel(
      textOutput("warning_reg"),
      
      p("Select the PCs to plot"),
      uiOutput("the_pcs_to_plot_x"),
      uiOutput("the_pcs_to_plot_y"),
      checkboxInput("show_points", "Show points ?", value = FALSE),
      # PCA 
      p('Get a PCA visualization of the data:'),
      plotOutput("pca", height = "300px")
    )),
    tabPanel("Cross-validation",mainPanel(
      p('To avoid overfitting your model, you want to hold some of your data out of thre training step to evaluate model performance.'),
      p('Cross-validation (CV) is one way to do so, by permutating data subsets as a validation set.'),
      p('Please select the number of folds you want to select.'),
      p('LOOCV refers to Leave-One-Out Cross-Validation, where 1 fold = 1 example.'),
      uiOutput("seed"),
      uiOutput("nfolds"),
      DT::dataTableOutput('CVtable')
    )),
    "Regression",
    tabPanel("Correlation table",mainPanel(
      p("Here, we display correlations between variables in the data."),
      uiOutput("iv1"),
      dataTableOutput("corr")
    )),
    tabPanel("Fit a linear model",mainPanel(
      p("Here, a standard linear regression is fitted to the data by selecting the covariates of interest."),
      uiOutput("dv"),
      uiOutput("iv2"),
      verbatimTextOutput("model"),
      p('Hint: you want the points on top-right plot be as close as possible from the straight line.'),
      p('If not, you are probably not meeting linear regression assumption: outliers, homosedasticity, ...'),
      plotOutput("model_plot")
    )),
    
    tabPanel("Regularization",mainPanel(
             p('Regularization is a way to constraint your linear model and account for colinearities.'),
             p('The general form of those optimization problems is given by:'),
             withMathJax("$$\\min_w L(Xw,Y) + \\Omega(w),$$"),
             withMathJax("$$\\text{where } L \\text{ is a loss function, like Mean-Square Error,}$$"),
             withMathJax("$$\\text{where } \\Omega \\text{ is a penalty function, like } (1-\\alpha)\\|.\\|_2^2 + \\alpha|.|_1,$$"),
             p('Common regularization methods are Ridge (alpha=0), Lasso (alpha=1), and Elastic-Net (other alpha).'),
             p('For more details, please refer to the excellent R package: glmnet.'),
             p('First, we need to choose parameter grid for alpha:'),
            # uiOutput("alpha_min"),
             #uiOutput("alpha_max"),
             uiOutput("alpha_range"),
             uiOutput("alpha_step"),
             p('Before starting to train a model, remember that one needs to apply cross-validation to diminish over-fitting.'),
             p('Please refer to the Cross-validation section to define your folds.'),
             plotOutput("regul_model"),
            verbatimTextOutput("regul_feature_selec")
             
    )),
    "Classification",
    tabPanel("k-nearest neighbours",mainPanel(
      p('The simple idea behind Nearest Neighbours approach is to predict a new example class, based on the majoritary class of its neighbours.'),
      p('In its basic formulation, there is two tunable parameters: the number of considered neighbours (k) and the way you measure distance between examples.'),
      uiOutput("knn"),
      uiOutput("knn_dist"),
      verbatimTextOutput("knn_perfs")
      
    )),
    tabPanel("Logistic regression",mainPanel(
      p('Logistic regression is a transformation of the standard linear regression for classification purpose.'),
      p('Because linear regression does not have a constraint on predicted values, it could return, for instance, negative probabilites.'),
      p('Logistic regression is simply considering a "logit" transformation that keeps every prediction between 0 and 1 (for binary classification),'),
      p('which corresponds to the following problem:'),
      withMathJax("$$P(Y_i=y | X_i) =  \\frac{e^{X_iw}}{1+e^{X_iw}}$$"),
      p('Therefore, this panel is very similar to the "Regression/Penalization" panel.'),
      uiOutput("alpha_range_log"),
      uiOutput("alpha_step_log"),
      p('Before starting to train a model, remember that one needs to apply cross-validation to diminish over-fitting.'),
      p('Please refer to the Cross-validation section to define your folds.'),
      plotOutput("log_regul_model_plot"),
      verbatimTextOutput("log_regul_model_perfs"),
      verbatimTextOutput("log_regul_feature_selec")
      
    )),
    tabPanel("Random forest",mainPanel(
      p('Random forest is a popular machine learning technique relying on the training of a large number of decision trees (ntree parameter).'),
      p('The prediction process occurs by majority vote, where each tree returns its own prediction.'),
      p('To allow diversity in the different trees, we usually rely on sub-sampling the predictors/features (mtry parameter).'),
      uiOutput("ntree"),
      uiOutput("mtry"),
      plotOutput("rf_plot"),
      verbatimTextOutput("rf_perfs"),
      verbatimTextOutput("rf_imp")
      
    )),
    tabPanel("Support vector machines",mainPanel(
      p('Support Vector Machine (SVM) is one of the most popular linear machine-learning approach.'),
      p('It aims at finding the best hyperplane separating data from different classes in the input space.'),
      p('Because, most of the time, classes are not strictly separable, SVM offers a trade-off between fitting the data and allowing some points to be misclassified (C parameter).'),
      p('As a regularized linear model, it is possible to consider different loss functions (Hinge, Logistic) and regularizers (L1, L2).'),
      uiOutput("C_svm_range"),
      uiOutput("C_svm_step"),
      uiOutput("svm_type"),
      plotOutput("svm_plot"),
      verbatimTextOutput("svm_perfs")
      
    ))
  )
  
))

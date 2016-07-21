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
             withMathJax("$$\\min_w L(XW,Y) + \\Omega(W),$$"),
             withMathJax("$$\\text{where } L \\text{ is a loss function, like Mean-Square Error,}$$"),
             withMathJax("$$\\text{where } \\Omega \\text{ is a penalty function, like } (1-\\alpha)\\|.\\|_2^2 + \\alpha|.|_1,$$"),
             p('Common regularization methods are Ridge (alpha=0), Lasso (alpha=1), and Elastic-Net (other alpha).'),
             p('For more details, please refer to the excellent R package: glmnet.'),
             p('First, we need to choose parameter grid for alpha:'),
             uiOutput("alpha_min"),
             uiOutput("alpha_max"),
             uiOutput("alpha_step"),
             p('Before starting to train a model, remember that one needs to apply cross-validation to diminish over-fitting.'),
             p('Please refer to the Cross-validation section to define your folds.'),
             plotOutput("regul_model")
             
    )),
    "Classification",
    tabPanel("k-nearest neighbours"),
    tabPanel("Logistic regression"),
    tabPanel("Random forest"),
    tabPanel("Support vector machines")
  )
  
))

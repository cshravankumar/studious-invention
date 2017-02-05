#############################
# Guido Giardini | Shravan Kumar Chandrasekaran | Richa Sharma | Nishant Jain | Akshit Mehta

#############################
# Set the path to our folder
getwd()
dir = "Historical prices/"
source("HMWK#1.R")
library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Customer Sentiment Analytics"),
  # Side panel design
  sidebarLayout(
    sidebarPanel(
      # Checkbox input: whether or not to superimpose the normal distribution
      radioButtons("main_radioselection","Select Mode",c("Comparison of Two Industries", "Analysis of an Industry", "Comparison of Two Brands", "Analysis of a Brand"), selected = NULL),
       
       conditionalPanel(
          condition = "input.main_radioselection == 'Comparison of Two Industries'",
            # Sidebar with a slider for the level of confidence. If 95% is chosen for example, then the
            # confidence interval is the interval in which the value of the true mean has 95% probability
            # to be, if repeating the operation when renewing the experiment.
            # Slide bar
              selectInput("c1_inds_op1",
                          label = "Choose an Industry",
                          choices = Industry_choices,
                          selected = NULL),
              selectInput("c1_inds_op2",
                          label = "Choose an Industry",
                          choices = Industry_choices,
                          selected = NULL)
                        ),
          conditionalPanel(
            condition = "input.main_radioselection == 'Analysis of an Industry'",
              # Sidebar with a slider for the level of confidence. If 95% is chosen for example, then the
              # confidence interval is the interval in which the value of the true mean has 95% probability
              # to be, if repeating the operation when renewing the experiment.
            selectInput("c2_inds_op1",
                        label = "Choose an Industry",
                        choices = Industry_choices,
                        selected = NULL),
            
            radioButtons("c2_radioselection","Select Mode",c("Comparison", "Analysis"), selected = NULL),
                  conditionalPanel(
                    condition = "input.c2_radioselection == 'Comparison'",
                   # Slide bar
                          radioButtons("c2_radioselection_s1","Select Mode",c("Comparison between two brands", "Comparison between two categories"), selected = NULL),
                                  
                               conditionalPanel(
                                    condition = "input.c2_radioselection_s1 == 'Comparison between two brands'",
                                        selectInput("c2_s1_brand_op1",
                                                    label = "Choose a Brand",
                                                    choices = Brand_choices,
                                                    selected = NULL),
                         
                                        selectInput("c2_s1_brand_op2",
                                                    label = "Choose a Brand",
                                                    choices = Brand_choices,
                                                    selected = NULL)
                                    
                                                ),
      
                                 conditionalPanel(
                                       condition = "input.c2_radioselection_s1 == 'Comparison between two categories'",
                                           selectInput("c2_s2_cat_op1",
                                                       label = "Choose a Category",
                                                       choices = Category_choices,
                                                       selected = NULL),
                                       selectInput("c2_s2_cat_op2",
                                                   label = "Choose a Category",
                                                   choices = Category_choices,
                                                   selected = NULL)
                                    #checkboxInput("c2_checkbox_cat",
                                    #"Category"),
                                  #checkboxInput(
                                 #   "c2_checkbox_brand", 
                                   # "Brand")
                                    )
  
              ),
            
            
            conditionalPanel(
              condition = "input.c2_radioselection == 'Analysis'",
              checkboxInput(
                "c2_checkbox2_cat", 
                "Category"),
                  
              conditionalPanel(condition = "input.c2_checkbox2_cat == true", selectInput("c2_s3_cat",
                                                                                         label = "Choose a Category",
                                                                                         choices = Category_choices,
                                                                                         selected = NULL)
                               ),
              
              
              checkboxInput(
                "c2_checkbox2_brand", 
                "Brand"),
              
              conditionalPanel(condition = "input.c2_checkbox2_brand == true", selectInput("c2_s3_brand",
                                                                                         label = "Choose a Brand",
                                                                                         choices = Brand_choices,
                                                                                         selected = NULL)
              )
              
            
            )
              )
              
           ,
    
    
    ###### IT STARTS HERE
    
    conditionalPanel(
      condition = "input.main_radioselection == 'Comparison of Two Brands'",
      # Sidebar with a slider for the level of confidence. If 95% is chosen for example, then the
      # confidence interval is the interval in which the value of the true mean has 95% probability
      # to be, if repeating the operation when renewing the experiment.
      # Slide bar
      selectInput("c3_brand_op1",
                  label = "Choose a Brand",
                  choices = Brand_choices,
                  selected = NULL),
      selectInput("c3_brand_op2",
                  label = "Choose a Brand",
                  choices = Brand_choices,
                  selected = NULL)
    ),
    conditionalPanel(
      condition = "input.main_radioselection == 'Analysis of a Brand'",
      # Sidebar with a slider for the level of confidence. If 95% is chosen for example, then the
      # confidence interval is the interval in which the value of the true mean has 95% probability
      # to be, if repeating the operation when renewing the experiment.
      selectInput("c4_brand",
                  label = "Choose a Brand",
                  choices = Brand_choices,
                  selected = NULL),
      radioButtons("c4_radioselection","Select Mode",c("Comparison", "Analysis")),
      conditionalPanel(
        condition = "input.c4_radioselection == 'Comparison'",
          selectInput("c4_s1_cat_op1",
                      label = "Choose a Category",
                      choices = Category_choices,
                      selected = NULL),
          selectInput("c4_s1_cat_op2",
                      label = "Choose a Category",
                      choices = Category_choices,
                      selected = NULL)
                      ),
        conditionalPanel(
          condition = "input.c4_radioselection == 'Analysis'",
          # Slide bar
          checkboxInput(
            "c4_s2_cat",
            "Category"),
          
          conditionalPanel(condition = "input.c4_s2_cat == true", selectInput("c4_s3_cat",
                                                                                     label = "Choose a Category",
                                                                                     choices = Category_choices,
                                                                                     selected = NULL)
          )
          
        )
      ),
    
    dateInput("datefrom", "From", value = "2012-02-29", format = "yyyy-mm"),
    dateInput("dateto", "From", value = "2012-02-29", format = "yyyy-mm")
    
    
    
    ),
    # Show a plot of the generated distribution, Output, Main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Overall Sentiment Chart",  plotOutput("Sentiplot")),
        tabPanel("Dimension Analysis", plotOutput("PricePlot"), plotOutput("QualityPlot")),
        tabPanel("WordCloud", plotOutput("WordCloud")),
        tabPanel("SalesRank", plotOutput("SalesRank"))
                 
    )
  )
)
))

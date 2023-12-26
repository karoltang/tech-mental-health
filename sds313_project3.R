library(shiny)
library(ggplot2)

clean_techData_subset <- read.csv('clean_techData_subset.csv')

ui <- fluidPage(
  titlePanel('Mental Health in Tech Workers'),
  
  sidebarLayout(
    
    sidebarPanel(
      h3('Customize Output'),
      hr(),
      
      #select box - var of interest
      selectInput("interest_variable", label = h4("Select variable to investigate"), 
                  choices = names(clean_techData_subset), 
                  selected = names(clean_techData_subset)[1]),
      hr(),
      
      #slider range
      sliderInput("age_slider", label = h4("Choose bounds to limit the data by age of respondents"), 
                  min = 20, max = 72, value = c(20, 40)),
      hr(),
      
      #select box - graph color
      selectInput("graph_color", label = h4("Select a color for your graph"), 
                  choices = list("Green" = 'Green', "Blue" = 'Blue', "Purple" = 'Purple'), 
                  selected = 1),
      hr(),
      
      #action button - display random var
      h4('Click button to display a random variable'),
      actionButton("random_var_button", label = "Display Random Variable")
    ),
    
    mainPanel(
      h2('Introduction', align='left'),
      p('This data is from a survey of how tech workers responded about their mental health in 2014. It was taken from:'), 
      a("Kaggle - Mental Health in Tech Survey",  href = "https://www.kaggle.com/datasets/osmi/mental-health-in-tech-survey"),
      img(src = "tech_worker.jpg", width = 400),
      br(),
      p('Image from:',a("Best Places to Work in Tech - 2020 Edition",  href = "https://finance.yahoo.com/news/best-places-tech-2020-edition-110033513.html")),
      p('The variables of interest are Family History (whether respondent has a family history of mental illness), Age, Gender, Work Interference (if the respondent has a mental health condition, whether they felt like it interfered with their work), Treatment (whether respondent had sought treatment for a mental health condition), and Condition (whether the respondent has a mental health condition, inferred from how they answered the work interference question). '),
      
      hr(),
      
      plotOutput('var_graph'),
      br(),
      h4('Statistics for the Above Graph'),
      p('For categorical variables, the statistics below correlate with the categories on the x-axis, from left to right.'),
      verbatimTextOutput('var_description')
    )
  )
)

server <- function(input, output, session) ({
  
  #create new subset of ages between age ranges
  #1. get bounds 
  slider_bounds <- reactiveVal(c(40, 60))
  
  observeEvent(input$age_slider, {
    slider_bounds(input$age_slider)
  })
  
  #2. create subset
  techData_agedSubset <- reactive({
    #slider bounds
    lower_bound <- slider_bounds()[1]
    upper_bound <- slider_bounds()[2]
    
    #subset
    subset_data <- clean_techData_subset[clean_techData_subset$Age >= lower_bound & clean_techData_subset$Age <= upper_bound, ]
    return(subset_data)
  })
  
  # reactive expression to calculate descriptive statistics
  var_description <- reactive({
    data_1 <- techData_agedSubset()
    selected_var <- input$interest_variable
    
    if (is.numeric(data_1[[selected_var]])) { #numeric variables (only age)
      mean_val <- mean(data_1[[selected_var]], na.rm = TRUE)
      sd_val <- sd(data_1[[selected_var]], na.rm = TRUE)
      description <- paste("Mean: ", round(mean_val, 2), "Standard Deviation: ", round(sd_val, 2))
    } 
    else { #categorical 
      count_table <- table(data_1[[selected_var]])
      prop_table <- prop.table(count_table)
      description <- paste("Count: ", table(data_1[[selected_var]]), "Proportions: ", prop_table)
    }
    
    return(description)
  })
  
  output$var_graph <- renderPlot({
    data_1 <- techData_agedSubset()
    selected_var <- input$interest_variable
    selected_color <- input$graph_color
    
    data_1_gg <- ggplot(data_1)
    
    data_1_gg + 
      geom_bar(aes(x=data_1[[selected_var]]), fill=selected_color) + 
      labs(title=paste("Histogram of", selected_var), x=selected_var, y='Count') + 
      theme_classic()
  })
  
  #histogram if user selects
  observeEvent(input$interest_variable, {
    updateSelectInput(session, "interest_variable", selected = input$interest_variable)
    updateSelectInput(session, "random_var_button", selected = input$interest_variable)
  })
  
  #histogram if random
  observeEvent(input$random_var_button, {
    random_var <- sample(names(clean_techData_subset), 1)
    updateSelectInput(session, "interest_variable", selected = random_var)
    updateSelectInput(session, "random_var_button", selected = random_var)
  })
  
  output$var_description <- renderPrint({
    var_description()
  })
})

shinyApp(ui = ui, server = server)

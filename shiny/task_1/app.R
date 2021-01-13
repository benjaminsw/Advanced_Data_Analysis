# install library via pacman
if (!require("pacman"))
    install.packages("pacman")
pacman::p_load(shiny, shinyWidgets, palmerpenguins, plotly, shinyjs)


# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("Palmerpenguins Dashbaord"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            shinyjs::useShinyjs(),
            awesomeRadio(
                inputId = "radio_num",
                label = "Numerical variables",
                choices = c(
                    "bill_length_mm" = "bill_length_mm",
                    "bill_depth_mm" = "bill_depth_mm",
                    "flipper_length_mm" = "flipper_length_mm",
                    "body_mass_g" = "body_mass_g"
                ),
                selected = "flipper_length_mm",
                status = "warning"
            ),
            hr(),
            awesomeRadio(
                inputId = "radio_cate",
                label = "Categorical variable",
                choices = c(
                    "sex" = "sex",
                    "year" = "year",
                    "species" = "species"
                ),
                selected = "sex",
                status = "warning"
            ),
            selectInput(
                inputId = "select_species",
                label = "Choose a subset for `species`",
                choices = c("None", "Adelie", "Chinstrap", "Gentoo"),
                selectize = FALSE
            ),
            hr(),
            checkboxInput("island", "Splited by island", FALSE)
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(plotlyOutput("box_plot")
                  , fluidRow(column(
                      3, verbatimTextOutput("value")
                  )))
        
    ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #select only complete cases
    df = penguins
    df = df[complete.cases(df),]
    df$year <- as.factor(df$year)
    
    data_num <- reactive({
        switch(
            input$radio_num,
            "bill_length_mm" = "bill_length_mm",
            "bill_depth_mm" = "bill_depth_mm",
            "flipper_length_mm" = "flipper_length_mm",
            "body_mass_g" = "body_mass_g"
        )
    })
    data_cate <- reactive({
        switch(
            input$radio_cate,
            "species" = "species",
            "sex" = "sex",
            "year" = "year"
        )
    })
    data_species <- reactive({
        switch(
            input$select_species,
            "None" = "None",
            "Adelie" = "Adelie",
            "Chinstrap" = "Chinstrap",
            "Gentoo" = "Gentoo"
        )
    })
    
    observeEvent(input$radio_cate, {
        if (input$radio_cate == "species") {
            shinyjs::enable("select_species")
        } else {
            shinyjs::reset("select_species")
            shinyjs::disable("select_species")
        }
    })
    
    output$box_plot <- renderPlotly({
        # create boxplot using ployly
        # if island is selected
        if (input$island) {
            # if none of species is selected
            if (input$select_species == "None") {
                plot_ly(
                    x = unlist(df[data_cate()]),
                    y = unlist(df[data_num()]),
                    color = unlist(df["island"]),
                    type = "box"
                ) %>% layout(boxmode = "group")
            } else{
                # select one of the species is selected
                tmp_df <- subset(df, species == data_species(), )
                max_y <- max(unlist(df[data_num()])) + 20
                min_y <- min(unlist(df[data_num()])) - 20
                plot_ly(
                    x = unlist(tmp_df[data_cate()]),
                    y = unlist(tmp_df[data_num()]),
                    color = unlist(tmp_df["island"]),
                    type = "box"
                ) %>% layout(yaxis = list(range = c(min_y, max_y)),
                             boxmode = "group")
            }
            # if island is not selected
        } else{
            # if none of species is not selected
            if (input$select_species == "None") {
                plot_ly(
                    x = unlist(df[data_cate()]),
                    y = unlist(df[data_num()]),
                    color = unlist(df[data_cate()]),
                    type = "box"
                )
                # select one of the species is selected
            } else{
                tmp_df <- subset(df, species == data_species(), )
                max_y <- max(unlist(df[data_num()])) + 20
                min_y <- min(unlist(df[data_num()])) - 20
                plot_ly(
                    x = unlist(tmp_df[data_cate()]),
                    y = unlist(tmp_df[data_num()]),
                    color = unlist(tmp_df[data_cate()]),
                    type = "box"
                ) %>% layout(yaxis = list(range = c(min_y, max_y)),
                             boxmode = "group")
            }
        }
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)

# install library via pacman
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(
  shiny,
  markdown,
  plotly,
  Hmisc,
  psych,
  scorecard,
  tippy,
  DT,
  rpart,
  rpart.plot,
  RColorBrewer,
  rattle,
  MASS
)

################################################################################
################################### UI #########################################
################################################################################
# Define UI for application that draws a histogram
ui <- fluidPage(navbarPage(
  "Fatality Dashboard",
  tabPanel("Data exploration",
           sidebarLayout(
             sidebarPanel(
               shinyjs::useShinyjs(),
               selectInput(
                 inputId = "plots",
                 label = "Plot type: ",
                 c("distribution" = "dist",
                   "box-plot by rate" = "box")
               ),
               
               radioButtons(
                 inputId = "var",
                 label = "Variables",
                 c(
                   "beertax" = "beertax",
                   "vmiles" = "vmiles",
                   "unrate" = "unrate",
                   "perinc" = "perinc",
                   "jaild" = "jaild",
                   "Rate" = "Rate"
                 ),
                 selected = "beertax"
               )
             ),
             mainPanel(
               plotlyOutput("plot"),
               hr(),
               h2("Summary Staticstics"),
               verbatimTextOutput("summary_stat"),
               hr(),
               headerPanel("")
             )
           )),
  tabPanel(
    "Classification tools",
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          inputId = "proportion",
          label = "Train vs Test ",
          min = 0.4,
          max = 0.8,
          value = 0.5,
          step = 0.1
        ),
        hr(),
        radioButtons(
          inputId = "prun",
          label = "pruned tree",
          c("view pruned tree" = "pruned",
            "view unpruned tree" = "unpruned"),
          selected = "unpruned"
        )
      ),
      mainPanel(
        h2("Tree plot"),
        plotOutput("tree_plot"),
        hr(),
        h2("Tree and QDA comparison"),
        DT::dataTableOutput("compare_table"),
        hr(),
        h3("Model Summary"),
        textOutput('model_summary'),
        hr(),
        h2("Prediction based on the best model"),
        splitLayout(
          textInput(inputId = "beertax", label = "beertax"),
          textInput(inputId = "jaild",   label = "jaild"),
          textInput(inputId = "vmiles",  label = "vmiles"),
          textInput(inputId = "unrate",  label = "unrate"),
          textInput(inputId = "perinc",  label = "perinc"),
          actionButton(inputId = "predict_class", label =
                         "Predict")
        ),
        plotOutput("pseudo_plot", height=50),
        textOutput("prediction_class"),
        hr(),
        headerPanel("")
      )
      
    )
  )
))

df <- read.csv("Fatality-task2.csv")
df$jaild <- as.factor(df$jaild)
df$Rate <- as.factor(df$Rate)

################################################################################
############################### SERVER #########################################
################################################################################
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  plot_type <- reactive({
    switch(input$plots,
           "dist" = "dist",
           "box" = "box")
  })
  var_selected <- reactive({
    switch(
      input$var,
      "beertax" = "beertax",
      "vmiles" = "vmiles",
      "unrate" = "unrate",
      "perinc" = "perinc",
      "jaild" = "jaild",
      "Rate" = "Rate"
    )
  })
  observeEvent(input$plots, {
    if (input$plots == "dist") {
      current_choices <- c(
        "beertax" = "beertax",
        "vmiles" = "vmiles",
        "unrate" = "unrate",
        "perinc" = "perinc",
        "jaild" = "jaild",
        "Rate" = "Rate"
      )
      
    } else{
      current_choices <- c(
        "beertax" = "beertax",
        "vmiles" = "vmiles",
        "unrate" = "unrate",
        "perinc" = "perinc",
        "jaild" = "jaild"
      )
    }
    updateRadioButtons(session, "var", choices = current_choices)
  })
  
  output$plot <- renderPlotly({
    if (plot_type() == "dist") {
      if (var_selected() %in% c("jaild", "Rate")) {
        tmp_df <- as.data.frame(table(df[var_selected()]))
        
        plot_ly(
          x = ~ tmp_df$Var1,
          y = ~ tmp_df$Freq,
          name = ~ var_selected(),
          type = "bar"
        ) %>% layout(
          title = paste0(var_selected(), " plot"),
          xaxis = list(title = var_selected()),
          yaxis = list(title = "count")
        )
        
      } else{
        plot_ly(x = ~ unlist(df[var_selected()]),
                type = "histogram") %>% layout(
                  title = paste0(var_selected(), " plot"),
                  xaxis = list(title = var_selected()),
                  yaxis = list(title = "count")
                )
      }
    } else{
      if (var_selected() %in% c("jaild")) {
        tmp_df <- as.data.frame(table(df[, c(var_selected() , "Rate")]))
        tmp_df[, var_selected()] <-
          as.factor(tmp_df[, var_selected()])
        tmp_df[, "Rate"] <- as.factor(tmp_df[, "Rate"])
        plot_ly(
          tmp_df,
          x =  ~ tmp_df[, "Rate"],
          y =  ~ tmp_df[, var_selected()],
          text =  ~ tmp_df[, "Freq"],
          type = 'scatter',
          mode = 'markers',
          color =  ~ tmp_df[, "Rate"],
          marker = list(size = tmp_df[, "Freq"], opacity =
                          0.5)
        ) %>% layout(
          title = paste0(var_selected(), " by Rate plot"),
          xaxis = list(title = "Rate"),
          yaxis = list(title = var_selected())
        )
      } else{
        plot_ly(
          df,
          x =  ~ df[, "Rate"],
          y =  ~ df[, var_selected()],
          color =  ~ df[, "Rate"],
          type = "box"
        ) %>% layout(
          title = paste0(var_selected(), " by Rate plot"),
          xaxis = list(title = "Rate"),
          yaxis = list(title = var_selected())
        )
      }
    }
  })
  output$summary_stat <- renderPrint({
    if (plot_type() == "dist") {
      Hmisc::describe(df[var_selected()])  #input$radio_cate
    } else{
      if (var_selected() %in% c("jaild")) {
        table(df[, var_selected()], df[, "Rate"])
      } else{
        psych::describeBy(df[, var_selected()], df[, "Rate"], mat = TRUE)
      }
    }
  })
  
  ############################### TREE ###########################################
  pruned_flag <- reactive({
    switch(input$prun,
           "pruned" = TRUE,
           "unpruned" = FALSE)
  })
  
  output$tree_plot <- renderPlot({
    idx_ <-
      sample(
        c(TRUE, FALSE),
        nrow(df),
        replace = TRUE,
        prob = c(input$proportion, 1 - input$proportion)
      )
    train_df <- df[idx_,]
    test_df <- df[!idx_,]
    tree_classifier <-
      rpart(Rate ~ ., data = train_df, method = 'class')
    tree_predict <-
      predict(tree_classifier, test_df, type = 'class')
    tree_matrix <- table(test_df$Rate, tree_predict)
    tree_correct <- sum(diag(tree_matrix))
    tree_incorrect <-
      sum(tree_matrix) - sum(diag(tree_matrix))
    tree_accuracy <-
      round(sum(diag(tree_matrix)) / sum(tree_matrix), 4)
    if (pruned_flag()) {
      tree_pruned <-
        prune(tree_classifier, cp = tree_classifier$cptable[which.min(tree_classifier$cptable[, "xerror"]), "CP"])
      fancyRpartPlot(
        tree_pruned,
        uniform = TRUE,
        main = "Pruned Classification Tree",
        palettes = "RdBu"
      ) 
    } else{
      fancyRpartPlot(
        tree_classifier,
        uniform = TRUE,
        main = "Unpruned Classification Tree",
        palettes = "RdBu"
      )
    }
    ############################### QDA ###########################################
    #prepare for summary table
    qda_classifier <- qda(Rate ~ ., data = train_df)
    qda_predict <- predict(qda_classifier, test_df)$class
    qda_matrix <- table(test_df$Rate, qda_predict)
    qda_correct <- sum(diag(qda_matrix))
    qda_incorrect <- sum(qda_matrix) - sum(diag(qda_matrix))
    qda_accuracy <-
      round(sum(diag(qda_matrix)) / sum(qda_matrix), 4)
    
    highlight_col <-
      ifelse(tree_accuracy > qda_accuracy, "Tree", "QDA")
    
    output$compare_table = DT::renderDataTable({
      compare_table <-
        datatable(
          data.frame(
            Tree = c(tree_correct, tree_incorrect, tree_accuracy),
            QDA = c(qda_correct, qda_incorrect, qda_accuracy),
            row.names = c(
              "correct_prediction",
              "incorrect_prediction",
              "accuracy"
            )
          ),
          options = list(
            searching = FALSE,
            lengthChange = FALSE,
            lengthMenu = FALSE,
            pageLength = FALSE,
            paging = FALSE,
            info = FALSE
          )
        ) %>% formatStyle(highlight_col,
                          backgroundColor = "orange",
                          fontWeight = "bold")
      #summary model
      
    })
    best_model_matrix <-
      `if`(
        tree_accuracy > qda_accuracy,
        c(tree_correct, tree_incorrect, tree_accuracy),
        c(qda_correct, qda_incorrect, qda_accuracy)
      )
    output$model_summary <-
      renderText({
        paste(
          "The model that performed best based upon the test set is ",
          highlight_col,
          " with accuratcy rate at ",
          best_model_matrix[3],
          ". This model can correctly classify ",
          best_model_matrix[1],
          " samples and misclassify ",
          best_model_matrix[2],
          " samples."
        )
      })
    
    
    #Change the value of a text input on the client
    updateTextInput(session, "beertax", value = round(mean(df$beertax, na.rm =
                                                             TRUE), 4))
    updateTextInput(session, "jaild", value = names(sort(-table(df$jaild)))[1])
    updateTextInput(session, "vmiles", value = round(mean(df$vmiles, na.rm =
                                                            TRUE), 4))
    updateTextInput(session, "unrate", value = round(mean(df$unrate, na.rm =
                                                            TRUE), 4))
    updateTextInput(session, "perinc", value = round(mean(df$perinc, na.rm =
                                                            TRUE), 4))
    
    ##predict
    observeEvent(input$predict_class, {
      new_beertax <- input$beertax
      new_jaild <- input$jaild
      new_vmiles <- input$vmiles
      new_unrate <- input$unrate
      new_perinc <- input$perinc
      
      #insert into dataframe
      df <- rbind(df,
                  c(
                    new_beertax,
                    new_jaild,
                    new_vmiles,
                    new_unrate,
                    new_perinc,
                    NA
                  ))
      
      df$beertax <- as.numeric(df$beertax)
      df$vmiles <- as.numeric(df$vmiles)
      df$unrate <- as.numeric(df$unrate)
      df$perinc <- as.numeric(df$perinc)
      
      if (tree_accuracy > qda_accuracy) {
        new_datapoint_class$outputText <-
          paste(
            "The model predict this datapoint to be Rate: ",
            predict(tree_classifier, df[nrow(df), ], type = "class")
          )
      } else{
        new_datapoint_class$outputText <-
          paste(
            "The model predict this datapoint to be Rate: ",
            predict(qda_classifier, df[nrow(df), ])$class
          )
      }
      
    })
    
    new_datapoint_class <-
      reactiveValues(outputText = "Click predict for result...")
    output$prediction_class <-
      renderText({
        new_datapoint_class$outputText
      })
    output$pseudo_plot <- renderPlot({
      #validate input
      #beertax
      validate(need(!is.na(as.numeric(
        input$beertax
      )), "Please enter number"))
      validate(need(
        as.numeric(input$beertax) <= max(df$beertax[1:nrow(df) - 1]),
        paste("The value must be less than or equal to ", round(max(df$beertax[1:nrow(df) -
                                                                                 1]), 4), ".")
      ))
      validate(need(
        as.numeric(input$beertax) >= min(df$beertax[1:nrow(df) - 1]),
        paste(
          "The value must be greater than or equal to ",
          round(min(df$beertax[1:nrow(df) - 1]), 4),
          "."
        )
      ))
      #jaild
      validate(need(
        input$jaild %in% c("yes", "no"),
        "Please enter only 'yes' or 'no'"
      ))
      #vmiles
      validate(need(!is.na(as.numeric(input$vmiles)), "Please enter number"))
      validate(need(
        as.numeric(input$vmiles) <= max(df$vmiles[1:nrow(df) - 1]),
        paste("The value must be less than or equal to ", round(max(df$vmiles[1:nrow(df) -
                                                                                1]), 4), ".")
      ))
      validate(need(
        as.numeric(input$vmiles) >= min(df$vmiles[1:nrow(df) - 1]),
        paste(
          "The value must be greater than or equal to ",
          round(min(df$vmiles[1:nrow(df) - 1]), 4),
          "."
        )
      ))
      #unrate
      validate(need(!is.na(as.numeric(input$unrate)), "Please enter number"))
      validate(need(
        as.numeric(input$unrate) <= max(df$unrate[1:nrow(df) - 1]),
        paste("The value must be less than or equal to ", round(max(df$unrate[1:nrow(df) -
                                                                                1]), 4), ".")
      ))
      validate(need(
        as.numeric(input$unrate) >= min(df$unrate[1:nrow(df) - 1]),
        paste(
          "The value must be greater than or equal to ",
          round(min(df$unrate[1:nrow(df) - 1]), 4),
          "."
        )
      ))
      #perinc
      validate(need(!is.na(as.numeric(input$perinc)), "Please enter number"))
      validate(need(
        as.numeric(input$perinc) <= max(df$perinc[1:nrow(df) - 1]),
        paste("The value must be less than or equal to ", round(max(df$perinc[1:nrow(df) -
                                                                                1]), 4), ".")
      ))
      validate(need(
        as.numeric(input$perinc) >= min(df$perinc[1:nrow(df) - 1]),
        paste(
          "The value must be greater than or equal to ",
          round(min(df$perinc[1:nrow(df) - 1]), 4),
          "."
        )
      ))
      
    })
    
  })
}
################################################################################
############################### EXECUTION ######################################
################################################################################
# Run the application
shinyApp(ui = ui, server = server)

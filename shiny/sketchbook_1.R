if (!require("pacman")) install.packages("pacman")
pacman::p_load(palmerpenguins, ggplot2, tidyr, rpart, rpart.plot, RColorBrewer, rattle, MASS)


df = penguins
df = df[complete.cases(df), ]
colSums(is.na(df))
colnames(df)
head(df, 20)
unique(df$species)
prop.table(table(df$flipper_length_mm, df$species, df$island))
table(df$species)
nrow(df)
typeof(penguins)

ggplot(df, aes(x=df$species, y=df$flipper_length_mm, fill=df$island)) + 
geom_boxplot() + stat_boxplot(geom = 'errorbar') +
scale_fill_brewer(palette="YlOrRd") +
theme_classic()

tmp_df <- df[c("sex", "flipper_length_mm")]
plot_ly(x=tmp_df$sex, y=tmp_df$flipper_length_mm, color=tmp_df$sex, type = "box") #%>% layout(boxmode = "group")
typeof(tmp_df$sex)
typeof(tmp_df["sex"])
typeof(unlist(tmp_df["sex"]))
x_<-tmp_df$sex
y_<-tmp_df["sex"]

####################### TASK 2 ##########################
df <- read.csv("task_2/Fatality-task2.csv")
head(df)
df$jaild <- as.factor(df$jaild)
df$Rate <- as.factor(df$Rate)
names(df)
plot_ly(df, x = ~beertax, y = ~jaild, color = ~beertax)


library(shiny)
library(shinyjs)
library(shinyWidgets)
if (interactive()) {
  ui <- fluidPage(
    useShinyjs(),
    selectInput(inputId="input", 
                label="choose ",
                c("A" = "a",
                  "B" = "b")),
    radioButtons(inputId="select", 
                 label="number",
                 c("1"="one",
                   "2"="two",
                   "3"="three"),
                 selected="one"),
    mainPanel(verbatimTextOutput("output")
  )
)
  server <- function(input, output, session) {
    observeEvent(input$input, {
      if(input$input=="b"){
        # disable 3
        current_choice <- c("1"="one", "2"="two")
      }else{
        # enable 3 again if input$input=="a"
        current_choice <- c("1"="one", "2"="two", "3"="three")
      }
      updateRadioButtons(session, "select", choices = current_choice)
    })
    output$output <- renderText({ input$select })
  }
  shinyApp(ui, server)
}

ave(df$Rate, df$jaild, FUN=length)
as.data.frame(with(df, table(jaild, Rate)))

as.data.frame(table(df[,c("jaild","Rate")]))

############################### TREE ###########################################
idx_ <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.75, 0.25))
train_df <- df[idx_, ]
test_df <- df[!idx_, ]
tree_classifier <- rpart(Rate~., data=train_df, method = 'class')
tree_predict <-predict(tree_classifier, test_df, type = 'class')
tree_matrix <- table(test_df$Rate, tree_predict)
tree_correct <- sum(diag(tree_matrix))
tree_incorrect <- sum(tree_matrix) - sum(diag(tree_matrix))
tree_accuracy <- sum(diag(tree_matrix)) / sum(tree_matrix)

#pruned tree
tree_pruned <- prune(tree_classifier, cp=tree_classifier$cptable[which.min(tree_classifier$cptable[,"xerror"]),"CP"])
fancyRpartPlot(tree_pruned, uniform=TRUE, main="Pruned Classification Tree", palettes=c("Greys", "Oranges"))

############################### QDA ###########################################
qda_classifier <- qda(Rate~., data=train_df)
qda_predict <- predict(qda_classifier, test_df)$class
qda_matrix <- table(test_df$Rate, qda_predict)
qda_correct <- sum(diag(qda_matrix))
qda_incorrect <- sum(qda_matrix) - sum(diag(qda_matrix))
qda_accuracy <- sum(diag(qda_matrix)) / sum(qda_matrix)
############################### DATA TABLE #####################################
a = matrix(round(rnorm(15, 50, 20)), 5)
c = which(a==max(a), arr.ind=TRUE)
datatable(as.data.frame(a)) %>% formatStyle(paste0("V",c[2]), color = 'red', fontWeight="bold")

names(sort(-table(df$jaild)))[1]

best_model_matrix <- `if`(TRUE, c(1, 2, 3), c(4, 5, 6)) 
best_model_matrix[1]
################################################################################
if (interactive()) {
  options(device.ask.default = FALSE)
  
  ui <- fluidPage(
    textInput("PValue", "P-value:", value = "0.05"),
    plotOutput('plot')
  )
  
  server <- function(input, output) {
    output$plot <- renderPlot({
      cat(str(input$PValue))
      
      validate(need(is.numeric(input$PValue), 'P-value must be a decimal between 0 and 1.')),
      validate(need(as.numeric(input$PValue) <= 1, 'P-value must be less than or equal to 1.')),
      validate(need(as.numeric(input$PValue) >= 0, 'P-value must be greater than or equal to 0.'))
      
      plot(input$PValue)
    })
    #p
  }
  shinyApp(ui, server)
}

new_datapoint <- df[0, ]
new_datapoint[1,] <- c(0.5138, as.factor("no"), 7.8363, 7.3555, 13881.198, NA)
predict(tree_classifier, new_datapoint[1,-c("Rate")], type = "class")
predict(qda_classifier, new_datapoint[1,-c("Rate")])$class


library(dplyr)
df$jaild %>% sapply(levels)
##########################################################
library(shiny)
library(DT)
RV <- reactiveValues(data = mtcars[1:3,])
app <- shinyApp(
  ui <- fluidPage(
    DT::dataTableOutput("mytable"),
    actionButton("do", "Click Me")
  ),
  server = function(input, output,session) {
    #Load the mtcars table into a dataTable
    output$mytable = DT::renderDataTable({
      tail(RV$data)
    })
    #A test action button
    observeEvent(input$do, {
      RV$data <- rbind(RV$data$cyl, RV$data$cyl[1,])
    })   
  }
)
runApp(app)




df <- rbind(df, df[1,])

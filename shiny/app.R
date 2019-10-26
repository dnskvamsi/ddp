library(shiny)
library(caret)
library(rattle)

ui <- fluidPage(
 titlePanel("Decision Tree Classifier for mtcars dataset"),
 h3("This app predicts the 'cyl' variable from the mtcars dataset"),
 sidebarLayout(
   sidebarPanel(
     sliderInput(inputId = "slider",min = 0,max = 1,value = 0.6,step = 0.1,label = "What % of data is training data?")
   ),
   mainPanel(
     tabsetPanel(
       tabPanel(title = "Dataset",h3("This is mtcars dataset"),tableOutput("data")),
       tabPanel(title = "predictions",tableOutput("predictions")),
       tabPanel(title = "Accuracy",verbatimTextOutput("accuracy")),
       tabPanel(title = "Tree",plotOutput("tree"))
       )
     )
   )
 )


server <- function(input, output) {
   output$data<-renderTable({
     mtcars
   })
   df<-reactive({
     data<-mtcars
     index<-createDataPartition(data$cyl,p=input$slider,list=FALSE)
     training<-data[index, ]
     testing<-data[-index, ]
     preobj<-preProcess(training[ ,-2],method = c("center","scale"))
     trainingnew<-predict(preobj,training[ ,-2])
     testingnew<-predict(preobj,testing[ ,-2])
     trainingnew$cyl<-as.factor(training$cyl)
     testingnew$cyl<-as.factor(testing$cyl)
     model<-train(cyl~.,data = trainingnew,method="rpart")
     answer<-predict(model,testingnew[ ,-11])
     data.frame(CarName = rownames(testingnew),Predictedcyl=answer,ActualCyl=testingnew$cyl)
   })
   
   output$predictions<-renderTable({
     df()
   })
   
   output$accuracy<-renderPrint({
     confusionMatrix(df()[ ,2],df()[ ,3])
   })
   
   output$tree<-renderPlot({
     data<-mtcars
     index<-createDataPartition(data$cyl,p=input$slider,list=FALSE)
     training<-data[index, ]
     testing<-data[-index, ]
     preobj<-preProcess(training[ ,-2],method = c("center","scale"))
     trainingnew<-predict(preobj,training[ ,-2])
     testingnew<-predict(preobj,testing[ ,-2])
     trainingnew$cyl<-as.factor(training$cyl)
     testingnew$cyl<-as.factor(testing$cyl)
     model<-train(cyl~.,data = trainingnew,method="rpart")
     fancyRpartPlot(model$finalModel)
   })
   }


# Run the application 
shinyApp(ui = ui, server = server)


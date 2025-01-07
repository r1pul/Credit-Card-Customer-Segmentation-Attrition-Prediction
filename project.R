# Install required packages if needed
install.packages(c("tidyverse", "caTools", "cluster", "rpart", "e1071", "caret", "corrplot", "ggplot2"))

# Load libraries
library(tidyverse)
library(caTools)
library(cluster)
library(rpart)
library(e1071)
library(caret)
library(corrplot)
library(ggplot2)

# Load dataset
credit_data <- read.csv(file.choose())

# Convert 'Attrition_Flag' to binary (1 for Attrited, 0 for Existing)
credit_data$Attrition_Flag <- ifelse(credit_data$Attrition_Flag == "Attrited Customer", 1, 0)

# Select features for analysis (adjusted based on the new dataset)
data_subset <- credit_data[, c("Total_Revolving_Bal", "Total_Trans_Amt", 
                               "Total_Trans_Ct", "Total_Ct_Chng_Q4_Q1", 
                               "Avg_Utilization_Ratio", "Customer_Age", 
                               "Total_Relationship_Count", "Credit_Limit")]
set.seed(123)
data_subset <- scale(data_subset[sample(1:nrow(data_subset), 200), ])



# Basic data exploration
summary(data_subset)
str(data_subset)

# Correlation plot
correlation <- cor(data_subset)
corrplot(correlation)

# Scale the data for clustering
scaled_data <- scale(data_subset)

# K-means Elbow Method
wcss <- vector()
for (i in 1:10) {
  wcss[i] <- sum(kmeans(scaled_data, i)$withinss)
}

# Plot elbow curve
plot(1:10,
     wcss,
     type = 'b',
     main = 'The Elbow Method',
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# K-means clustering
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 4)
y_kmeans <- kmeans_result$cluster

# Plot k-means clusters
clusplot(
  scaled_data,
  y_kmeans,
  lines = 0,
  shade = TRUE,
  color = TRUE,
  labels = 2,
  plotchar = FALSE,
  span = TRUE,
  main = 'Clusters of Credit Card Customers (K-means)',
  xlab = 'Feature 1',
  ylab = 'Feature 2'
)

# Hierarchical Clustering
# Create dendogram
dendogram <- hclust(dist(scaled_data, method = 'euclidean'),
                    method = 'ward.D')

# Plot dendogram
plot(dendogram,
     main = 'Dendogram',
     xlab = 'Customers',
     ylab = 'Euclidean Distance')

# Cut tree for hierarchical clusters
hc_clusters <- cutree(dendogram, k = 4)

# Plot hierarchical clusters
clusplot(
  scaled_data,
  hc_clusters,
  lines = 0,
  shade = TRUE,
  color = TRUE,
  labels = 2,
  plotchar = TRUE,
  span = TRUE,
  main = 'Hierarchical Clusters of Customers',
  xlab = 'Feature 1',
  ylab = 'Feature 2',
  col.p = "purple", 
  col.clus = c("blue", "green", "red", "purple")
)
target <- credit_data$Attrition_Flag[sample(1:nrow(credit_data), 200)]

# Split data for training and testing
set.seed(123)
split <- sample.split(target, SplitRatio = 0.7)
train_data <- scaled_data[split, ]
test_data <- scaled_data[!split, ]
train_target <- target[split]
test_target <- target[!split]

# KNN Classification
knn_model <- train(
  x = train_data,
  y = as.factor(train_target),
  method = "knn",
  tuneGrid = data.frame(k = 1:20),
  trControl = trainControl(method = "cv", number = 5)
)

# Decision Tree
dt_model <- rpart(target ~ ., 
                  data = data.frame(train_data, target = train_target))

# Naive Bayes
nb_model <- naiveBayes(x = train_data, 
                       y = as.factor(train_target))

# Make predictions
knn_pred <- predict(knn_model, newdata = test_data)
# Convert test_data to data.frame before predicting
# Ensure that the scaled data has column names
colnames(train_data) <- colnames(data_subset)
colnames(test_data) <- colnames(data_subset)

# Re-train the Decision Tree model with a proper data frame
train_df <- data.frame(train_data, target = train_target)
dt_model <- rpart(target ~ ., data = train_df)

# Predict on the test set after converting to a data frame with the correct column names
test_df <- as.data.frame(test_data)
colnames(test_df) <- colnames(data_subset)

# Make predictions with the decision tree model
dt_pred <- predict(dt_model, newdata = test_df, type = "class")
print(dt_pred)
install.packages("plotly")
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(cluster)
library(rpart)
library(corrplot)

# Shiny Dashboard
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Credit Card Customer Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Plot Type:"),
      selectInput("plotType", "Choose Visualization", 
                  choices = c("K-means Clustering", "Decision Tree", "Correlation Matrix")),
      hr(),
      h4("Decision Tree Model Accuracy"),
      verbatimTextOutput("accuracyOutput")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("mainPlot")),
        tabPanel("Data Summary", verbatimTextOutput("summaryOutput")),
        tabPanel("Correlation", plotOutput("corrPlot"))
      )
    )
  )
)

server <- function(input, output) {
  # Load preprocessed dataset (Assuming you already have it)
  credit_data <- read.csv(file.choose())
  
  # Convert 'Attrition_Flag' to binary
  credit_data$Attrition_Flag <- ifelse(credit_data$Attrition_Flag == "Attrited Customer", 1, 0)
  
  # Subset and scale the data
  data_subset <- credit_data[, c("Total_Revolving_Bal", "Total_Trans_Amt", 
                                 "Total_Trans_Ct", "Total_Ct_Chng_Q4_Q1", 
                                 "Avg_Utilization_Ratio", "Customer_Age", 
                                 "Total_Relationship_Count", "Credit_Limit")]
  scaled_data <- scale(data_subset)
  
  # K-means clustering
  set.seed(123)
  kmeans_result <- kmeans(scaled_data, centers = 4)
  credit_data$Cluster <- as.factor(kmeans_result$cluster)
  
  # Train Decision Tree model
  target <- credit_data$Attrition_Flag
  train_df <- data.frame(data_subset, target = target)
  dt_model <- rpart(target ~ ., data = train_df)
  dt_pred <- predict(dt_model, newdata = data_subset, type = "class")
  accuracy <- mean(dt_pred == target)
  
  # Data Summary
  output$summaryOutput <- renderPrint({
    summary(data_subset)
  })
  
  # Correlation Plot
  output$corrPlot <- renderPlot({
    correlation <- cor(data_subset)
    corrplot(correlation, method = "circle")
  })
  
  # Display Decision Tree Accuracy
  output$accuracyOutput <- renderPrint({
    paste("Decision Tree Accuracy:", round(accuracy * 100, 2), "%")
  })
  
  # Render the main plot based on user input
  output$mainPlot <- renderPlotly({
    if (input$plotType == "K-means Clustering") {
      # K-means Clustering Plot
      p <- ggplot(credit_data, aes(x = Total_Trans_Amt, y = Total_Revolving_Bal, color = Cluster)) +
        geom_point() +
        labs(title = "K-means Clustering", x = "Total Transaction Amount", y = "Total Revolving Balance") +
        theme_minimal()
      ggplotly(p)
      
    } else if (input$plotType == "Decision Tree") {
      # Decision Tree Plot
      rpart.plot::rpart.plot(dt_model)
      
    } else if (input$plotType == "Correlation Matrix") {
      # Correlation Matrix Plot
      correlation <- cor(data_subset)
      corrplot(correlation, method = "circle")
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


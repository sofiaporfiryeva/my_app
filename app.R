options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("stopwords", quietly = TRUE)) {
  install.packages("stopwords")
}

library(shiny)
library(keras3)
library(recipes)
library(tibble)
library(dplyr)
library(stringr)
library(stopwords)
library(purrr)
library(ggplot2)
library(textrecipes)
library(bslib)

onehot_rec <- readRDS("onehot_prep.rds")
model <- load_model("my_dense_model.keras")

# Список названий классов 
class_names <- c("Business", "Sci/Tech", "Sports", "World") 

ui <- fluidPage(
  #theme = shinytheme("cyborg"),
  theme = bs_theme(bootswatch = "superhero"),
  titlePanel("📰 Классификатор новостей"),
  sidebarLayout(
    sidebarPanel(
      width = 6,
      tags$h4("Вставьте или напечатайте новость:"),
      textAreaInput("user_text", NULL, placeholder = "Введите текст новости здесь...", rows = 6),
      actionButton("predict_btn", "🔍 Предсказать категорию", class = "btn-primary")
    ),
    mainPanel(
      width = 6,
      h3("Результаты"),
      uiOutput("result_text"),
      plotOutput("prob_plot", height = 250),
      tags$hr(),
      tags$small("Ваши данные не сохраняются, все вычисления проходят локально.")
    )
  )
)

server <- function(input, output, session) {
  pred_result <- reactive({
    req(input$user_text)
    new_data <- tibble(description = input$user_text)
    model_input <- bake(onehot_rec, 
                        new_data = new_data,
                        composition = "matrix")
    probs <- as.numeric(model |> predict(model_input))
    pred_cat <- class_names[which.max(probs)]
    list(
      category = pred_cat,
      probs = setNames(probs, class_names)
    )
  }) |> bindEvent(input$predict_btn)
  
  output$result_text <- renderUI({
    req(pred_result())
    cat <- pred_result()$category
    HTML(
      paste0(
        "<h4>🌟 Предсказанная категория: <span style='color:#0072B2;'>", cat, "</span></h4>"
      )
    )
  })
  
  output$prob_plot <- renderPlot({
    req(pred_result())
    tibble(category = class_names,
           probability = pred_result()$probs)  |> 
      ggplot(aes(y = reorder(category, probability), x = probability, fill = category)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      scale_fill_brewer(palette = "Set2") +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = "Вероятность", y = "Категория") +
      theme_minimal(base_size = 15) +
      theme(
        axis.title.y = element_blank(),
        plot.title = element_text(face="bold"),
        axis.text = element_text(size=12)
      )
  })
}

shinyApp(ui, server)

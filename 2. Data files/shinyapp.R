library(tidyverse)
library(shiny)
library(shinyFeedback)
library(plotly)
library(sf)
library(RColorBrewer)
library(ggplot2)

select.type <- c("Private", "Public")

ui <- fluidPage(
  
  titlePanel("Insurance Ownership in the US (2022)"),
  
  useShinyFeedback(),
  sidebarLayout(
    sidebarPanel(
      img(src = "https://www.appam.org/assets/1/6/harris3.png?62132",
          height = 80,
          width = 180),
      radioButtons(inputId = "insurance",
                   label = "Choose one type of insurance",
                   choices = select.type,
                   selected = "0"),
      selectInput(inputId = "income",
                  label = "Select the income level group",
                  choices = NULL),
      textInput(inputId = "state1", 
                label = "Input State 1 Code",
                placeholder = "CA"),
      textInput(inputId = "state2",
                label = "Input State 2 Code",
                placeholder = "IL")
    ), 
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("US Map", plotOutput("choro")),
        tabPanel("State Graphs", plotlyOutput("plot"))
      )
    )
  )
)

server <- function(input, output) {
  data <- read_csv("insurance_income.csv")
  us_map <- st_read("cb_2018_us_state_500k.shp") |>
    st_transform(2163)
  
  data_joined <- reactive({
    data |> 
      left_join(us_map, by = c("STUSAB" = "STUSPS")) |>
      filter(TYPE == input$insurance)
  })
  
  observeEvent(input$insurance, {
    select.income <- names(data_joined())[4:8]
    updateSelectInput(inputId = "income", choices = select.income)
    updateTextInput(inputId = "state1")
    updateTextInput(inputId = "state2")
  })
  
  data_map <- reactive({
    req(input$income)
    data_joined() |>
      select(STUSAB, STATE, TYPE, .data[[input$income]], geometry)
  })
  
  data_sf <- reactive({
    st_sf(data_map()) |> st_transform(2163)
  })
  
  data_plot <- reactive({
    req(input$state1, input$state2)
    
    data <- data_joined() |>
      filter(STUSAB %in% c(input$state1, input$state2)) |>
      select(STUSAB, STATE, TYPE, 4:8) |>
      pivot_longer(
        cols = 4:8,
        names_to = "INCOME",
        values_to = "COUNT"
      )
    
    data$STUSAB <- factor(data$STUSAB, levels = c(input$state1, input$state2))
    data
  })
  
  # Create the color palettes
  pvt_palette <- c("white", "skyblue", "steelblue")
  
  pub_palette <- c("white", "#FDB777", "#FD9346")
  
  selected_palette <- reactive({
    if (input$insurance == "Private") {
      pvt_palette
    } else {
      pub_palette
    }
  })
  
  chosen_states <- reactive({
    subset(data_sf(), STUSAB %in% c(input$state1, input$state2)) |> 
      st_transform(crs = 2163)
  })
  
  centroids <- reactive({
    st_centroid(st_geometry(chosen_states())) |> 
      st_transform(crs = st_crs(chosen_states()))
  })
  
  labels_df <- reactive({
    data.frame(geometry = centroids(), 
               label = chosen_states()$STUSAB) |> st_sf()
  })
  
  output$choro <- renderPlot({
    req(input$insurance %in% c("Private", "Public"))
    req(input$income %in% names(data_joined()))
    
    ggplot() +     
      geom_sf(data = data_sf(), aes(fill = .data[[input$income]])) +
      geom_sf(data = subset(data_sf(), STUSAB %in% c(input$state1, input$state2)), 
              color = "red", 
              fill = NA, 
              size = 8) +
      geom_sf_text(data = labels_df(), aes(label = label), 
                   color = "firebrick", 
                   size = 4) +
      scale_fill_gradientn(colors = selected_palette()) +
      labs(title = paste0("Map of ", input$insurance, " Insurance Ownership"),
           fill = "Percentage",
           caption = "Source: NHGIS Data, 2022") +
      theme_minimal() +
      theme(text = element_text(family = "Arial Narrow"),
            plot.title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold"))
  })
  
  output$plot <- renderPlotly({
    req(input$state1, input$state2)
    
    plt <- ggplot(data = data_plot(), aes(x = INCOME, y = COUNT, fill = STATE)) +
      geom_col() +
      facet_wrap(~factor(STUSAB, levels = c(input$state1, input$state2)), scales = "free_y") +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      scale_fill_brewer(palette = "Pastel2") +
      labs(x = "Income Level", 
           y = "Proportion", 
           title = "Insurance Coverage Distribution by State",
           caption = "Source: NHGIS Data, 2022") +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(family = "Arial Narrow"),
            plot.title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold"))
    
    ggplotly(plt)
  })
}

shinyApp(ui = ui, server = server)

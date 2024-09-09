library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Error Tipo II y Potencia"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha", "Nivel de Significancia (alpha):", 
                  min = 0.01, max = 0.10, value = 0.05, step = 0.01),
      sliderInput("beta", "Error Tipo II (beta):", 
                  min = 0.1, max = 0.9, value = 0.2, step = 0.05),
      sliderInput("n", "Tamaño de Muestra (n):", 
                  min = 10, max = 100, value = 30, step = 5)
    ),
    
    mainPanel(
      plotOutput("powerPlot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  output$powerPlot <- renderPlot({
    # Parámetros ajustables
    mu_0 <- 100    # Hipótesis nula
    mu_a <- 98     # Hipótesis alternativa
    sigma <- 5     # Desviación estándar
    n <- input$n   # Tamaño de la muestra (ajustable)
    alpha <- input$alpha # Nivel de significancia (ajustable)
    df <- n - 1    # Grados de libertad
    
    # Crear secuencia de x entre 90 y 105
    x <- seq(90, 105, length = 400)
    
    # Función densidad t-Student bajo H0 y Ha
    t_null <- dt((x - mu_0) / (sigma / sqrt(n)), df)
    t_alt <- dt((x - mu_a) / (sigma / sqrt(n)), df)
    
    df_plot <- data.frame(x = x, t_null = t_null, t_alt = t_alt)
    
    # Valor crítico para la prueba de hipótesis
    critical_value <- mu_0 + qt(alpha, df) * sigma / sqrt(n)
    
    # Colores
    c1 <- "orange"
    c3 <- "blue"
    c4 <- "green"
    
    # Graficar la potencia y error tipo II
    ggplot(df_plot, aes(x = x)) +
      geom_line(aes(y = t_null, color = "H0")) +
      geom_line(aes(y = t_alt, color = "Ha")) +
      geom_vline(xintercept = critical_value, linetype = "dashed", color = "red") +
      labs(title = "Error Tipo II y Potencia", 
           x = "Valores de la media", y = "Densidad") +
      scale_color_manual(values = c("H0" = c3, "Ha" = c4)) +
      annotate("rect", xmin = critical_value, xmax = max(x), ymin = 0, ymax = Inf, alpha = 0.2, fill = c1, label = "Beta") +
      annotate("rect", xmin = min(x), xmax = critical_value, ymin = 0, ymax = Inf, alpha = 0.2, fill = c4, label = "Potencia") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

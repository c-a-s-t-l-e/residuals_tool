library(shiny)
library(bslib)
library(tidyverse)

ui <- page_navbar(
  title = "Regression Statistics Calculator",
  theme = bs_theme(bootswatch = "flatly"),
  
  nav_panel("Visualization & Results",
            layout_sidebar(
              sidebar = sidebar(
                fileInput("file", "Upload CSV file"),
                selectInput("x_var", "Select X Variable", choices = NULL),
                selectInput("y_var", "Select Y Variable", choices = NULL),
                checkboxGroupInput("show_elements", "Show on Plot:",
                                   choices = c(
                                     "Residuals" = "residuals",
                                     "Mean Line" = "mean_line",
                                     "Fitted Values" = "fitted",
                                     "SSE Components" = "sse",
                                     "SSR Components" = "ssr"
                                   ),
                                   selected = "residuals"
                )
              ),
              
              layout_columns(
                card(
                  card_header("Scatter Plot with Regression Line"),
                  plotOutput("scatter_plot", height = "500px")
                ),
                card(
                  card_header("Statistical Results"),
                  tableOutput("stats_table")
                )
              )
            )
  ),
  
  nav_panel("Calculations & Code",
            card(
              card_header("Mathematical Formulas and R Code"),
              HTML("
        <h4>Formulas:</h4>
        <ul>
          <li>Residual (e<sub>i</sub>) = y<sub>i</sub> - ŷ<sub>i</sub></li>
          <li>SSE (Sum of Squared Errors) = Σ(y<sub>i</sub> - ŷ<sub>i</sub>)²</li>
          <li>SSR (Sum of Squares Regression) = Σ(ŷ<sub>i</sub> - ȳ)²</li>
          <li>SSTO (Total Sum of Squares) = Σ(y<sub>i</sub> - ȳ)²</li>
          <li>R² = SSR/SSTO = 1 - SSE/SSTO</li>
          <li>MSE (Mean Squared Error) = SSE/(n-2)</li>
          <li>Residual Standard Error = √MSE</li>
        </ul>
      "),
              verbatimTextOutput("r_code")
            )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  observeEvent(data(), {
    choices <- names(data())
    updateSelectInput(session, "x_var", choices = choices)
    updateSelectInput(session, "y_var", choices = choices)
  })
  
  model_results <- reactive({
    req(input$x_var, input$y_var)
    
    x <- data()[[input$x_var]]
    y <- data()[[input$y_var]]
    
    # Fit model
    model <- lm(y ~ x)
    
    # Calculate components
    y_hat <- fitted(model)
    residuals <- residuals(model)
    y_mean <- mean(y)
    
    # Calculate sums of squares
    sse <- sum(residuals^2)
    ssr <- sum((y_hat - y_mean)^2)
    ssto <- sum((y - y_mean)^2)
    
    # Calculate other statistics
    r_squared <- ssr/ssto
    n <- length(y)
    mse <- sse/(n-2)
    res_std_error <- sqrt(mse)
    
    # Get model summary for additional statistics
    model_summary <- summary(model)
    
    list(
      model = model,
      summary = model_summary,
      sse = sse,
      ssr = ssr,
      ssto = ssto,
      r_squared = r_squared,
      mse = mse,
      res_std_error = res_std_error,
      x = x,
      y = y,
      y_hat = y_hat,
      y_mean = y_mean,
      residuals = residuals,
      n = n
    )
  })
  
  output$scatter_plot <- renderPlot({
    res <- model_results()
    
    # Create data frame for predictions
    new_x <- data.frame(x = seq(min(res$x), max(res$x), length.out = 100))
    predicted <- predict(res$model, newdata = data.frame(x = new_x$x))
    
    # Base plot
    p <- ggplot(data.frame(x = res$x, y = res$y), aes(x = x, y = y)) +
      theme_minimal(base_size = 14) +
      labs(x = input$x_var, y = input$y_var)
    
    # Add SSR components if selected
    if ("ssr" %in% input$show_elements) {
      segments_data <- data.frame(
        x = res$x,
        y = res$y_hat,
        yend = res$y_mean
      )
      p <- p + geom_segment(data = segments_data,
                            aes(x = x, y = y, xend = x, yend = yend),
                            color = "blue", alpha = 0.5, linewidth = 2)
    }
    
    # Add SSE components if selected
    if ("sse" %in% input$show_elements) {
      segments_data <- data.frame(
        x = res$x,
        y = res$y,
        yend = res$y_hat
      )
      p <- p + geom_segment(data = segments_data,
                            aes(x = x, y = y, xend = x, yend = yend),
                            color = "red", alpha = 0.5, linewidth = 2)
    }
    
    # Add mean line if selected
    if ("mean_line" %in% input$show_elements) {
      p <- p + geom_hline(yintercept = res$y_mean, color = "darkgreen", 
                          linetype = "dashed", linewidth = 1)
    }
    
    # Add regression line using predicted values
    p <- p + geom_line(data = data.frame(x = new_x$x, y = predicted), 
                       aes(x = x, y = y), 
                       color = "blue", 
                       linewidth = 1) +
      geom_point(size = 3)
    
    # Add fitted points if selected
    if ("fitted" %in% input$show_elements) {
      p <- p + geom_point(aes(y = res$y_hat), color = "blue", size = 3, shape = 1)
    }
    
    # Add residuals if selected
    if ("residuals" %in% input$show_elements) {
      segments_data <- data.frame(
        x = res$x,
        y = res$y,
        xend = res$x,
        yend = res$y_hat
      )
      p <- p + geom_segment(data = segments_data,
                            aes(x = x, y = y, xend = xend, yend = yend),
                            color = "red", linetype = "dashed")
    }
    
    p
  })
  
  output$stats_table <- renderTable({
    res <- model_results()
    
    # Create statistics table with only the specified metrics
    data.frame(
      Statistic = c(
        "SSE (Sum of Squared Errors)",
        "SSR (Sum of Squares Regression)",
        "SSTO (Total Sum of Squares)",
        "R²",
        "MSE (Mean Squared Error)",
        "Residual Standard Error"
      ),
      Value = c(
        res$sse,         # SSE
        res$ssr,         # SSR
        res$ssto,        # SSTO
        res$r_squared,   # R-squared
        res$mse,         # MSE
        res$res_std_error # Residual Standard Error
      )
    )
  }, digits = 4)
  
  output$r_code <- renderText({
    res <- model_results()
    sprintf('
# R code for calculations:
x <- data$%s
y <- data$%s

# Fit linear model
model <- lm(y ~ x)

# Get fitted values and residuals
y_hat <- fitted(model)
residuals <- residuals(model)
y_mean <- mean(y)

# Calculate sums of squares
SSE <- sum(residuals^2)  # Sum of Squared Errors
SSR <- sum((y_hat - y_mean)^2)  # Sum of Squares Regression
SSTO <- sum((y - y_mean)^2)  # Total Sum of Squares

# Calculate other statistics
R_squared <- SSR/SSTO
n <- length(y)
MSE <- SSE/(n-2)  # Mean Squared Error
Residual_Std_Error <- sqrt(MSE)
    ', input$x_var, input$y_var)
  })
}

shinyApp(ui, server)
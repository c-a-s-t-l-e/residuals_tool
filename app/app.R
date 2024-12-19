library(shiny)
library(bslib)
library(tidyverse)

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
                                     "SSR Components" = "ssr",
                                     "RSE Bands" = "rse_bands"
                                   ),
                                   selected = "residuals"
                )
              ),
              
              layout_columns(
                card(
                  card_header("Scatter Plot with Regression Line"),
                  plotOutput("scatter_plot", height = "500px")
                ),
                layout_columns(
                  card(
                    card_header("Error Statistics"),
                    card_body(
                      tooltip(
                        span(
                          "RSE:",
                          textOutput("rse_value", inline = TRUE)
                        ),
                        "Residual Standard Error: A measure of the average deviation of observations from the regression line"
                      ),
                      tooltip(
                        span(
                          "MSE:",
                          textOutput("mse_value", inline = TRUE)
                        ),
                        "Mean Squared Error: The average squared difference between observed and predicted values"
                      ),
                      br(),
                      h5("Residuals Breakdown:"),
                      tableOutput("residuals_table")
                    )
                  ),
                  card(
                    card_header("Statistical Results"),
                    tableOutput("stats_table")
                  )
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
          <li><strong>MSE (Mean Squared Error) = SSE/(n-2)</strong></li>
          <li><strong>Residual Standard Error = √MSE</strong></li>
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
    
    validate(
      need(!is.null(x) && !is.null(y), "Please select both X and Y variables"),
      need(all(is.finite(x)), "X variable contains non-finite values"),
      need(all(is.finite(y)), "Y variable contains non-finite values")
    )
    
    model_data <- data.frame(y = y, x = x)
    model <- try(lm(y ~ x, data = model_data), silent = TRUE)
    
    if (inherits(model, "try-error")) {
      return(NULL)
    }
    
    y_hat <- fitted(model)
    residuals <- residuals(model)
    y_mean <- mean(y)
    
    sse <- sum(residuals^2)
    ssr <- sum((y_hat - y_mean)^2)
    ssto <- sum((y - y_mean)^2)
    
    r_squared <- ssr/ssto
    n <- length(y)
    mse <- sse/(n-2)
    res_std_error <- sqrt(mse)
    
    list(
      model = model,
      summary = summary(model),
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
    req(input$x_var, input$y_var)
    res <- req(model_results())
    validate(need(!is.null(res), "Unable to create plot: model fitting failed"))
    
    tryCatch({
      plot_data <- data.frame(
        x = res$x,
        y = res$y,
        y_hat = res$y_hat,
        residuals = res$residuals
      )
      
      # Initialize base plot
      p <- ggplot(plot_data, aes(x = x, y = y)) +
        theme_minimal(base_size = 14) +
        labs(x = input$x_var, y = input$y_var) +
        geom_point(size = 3)
      
      # Add regression line and bands
      if ("rse_bands" %in% input$show_elements) {
        # Use the original x values for prediction
        new_data <- data.frame(x = res$x)
        names(new_data) <- input$x_var
        pred <- predict(res$model, newdata = new_data, interval = "confidence")
        
        line_data <- data.frame(
          x = res$x,
          fit = pred[,"fit"],
          lwr = pred[,"lwr"],
          upr = pred[,"upr"]
        )
        
        p <- p + 
          geom_ribbon(data = line_data,
                      aes(x = x, y = fit, ymin = lwr, ymax = upr),
                      alpha = 0.2,
                      fill = "blue") +
          geom_line(data = line_data,
                    aes(x = x, y = fit),
                    color = "blue",
                    linewidth = 1)
      } else {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1)
      }
      
      if ("mean_line" %in% input$show_elements) {
        p <- p + geom_hline(yintercept = res$y_mean, 
                            color = "darkgreen", 
                            linetype = "dashed")
      }
      
      if ("fitted" %in% input$show_elements) {
        p <- p + geom_point(aes(y = y_hat), color = "blue", size = 3, shape = 1)
      }
      
      if ("residuals" %in% input$show_elements) {
        p <- p + geom_segment(aes(x = x, y = y, xend = x, yend = y_hat),
                              color = "red", linetype = "dashed")
      }
      
      if ("ssr" %in% input$show_elements) {
        p <- p + geom_segment(aes(x = x, y = y_hat, xend = x, yend = res$y_mean),
                              color = "blue", alpha = 0.5)
      }
      
      if ("sse" %in% input$show_elements) {
        p <- p + geom_segment(aes(x = x, y = y, xend = x, yend = y_hat),
                              color = "red", alpha = 0.5)
      }
      
      if ("mse_components" %in% input$show_elements) {
        residuals_sq <- res$residuals^2
        max_residual_sq <- max(residuals_sq)
        box_width <- diff(range(res$x)) * 0.05
        
        squares_data <- data.frame(
          x = res$x,
          y = res$y,
          y_hat = res$y_hat,
          residual = res$residuals,
          residual_sq = residuals_sq,
          box_size = sqrt(residuals_sq/max_residual_sq) * box_width
        )
        
        for(i in seq_len(nrow(squares_data))) {
          square <- squares_data[i, ]
          p <- p + 
            geom_rect(data = square,
                      aes(xmin = x - box_size/2,
                          xmax = x + box_size/2,
                          ymin = y_hat - box_size/2,
                          ymax = y_hat + box_size/2),
                      fill = "orange",
                      alpha = 0.3) +
            geom_segment(data = square,
                         aes(x = x, y = y, xend = x, yend = y_hat),
                         color = "red",
                         linetype = "dashed") +
            annotate("text",
                     x = square$x,
                     y = square$y_hat,
                     label = sprintf("%.2f²\n=%.2f", abs(square$residual), square$residual_sq),
                     size = 3)
        }
        
        p <- p + annotate("text",
                          x = mean(range(res$x)),
                          y = max(res$y),
                          label = sprintf("MSE = Σ(residuals²)/(n-2) = %.2f", res$mse),
                          size = 4,
                          fontface = "bold")
      }
      
      p
      
    }, error = function(e) {
      ggplot() + theme_void() + 
        annotate("text", x = 0, y = 0, label = "Error creating plot")
    })
  })
  
  output$residuals_table <- renderTable({
    res <- model_results()
    data.frame(
      Point = seq_len(length(res$residuals)),
      `Observed Y` = round(res$y, 4),
      `Predicted Y` = round(res$y_hat, 4),
      Residual = round(res$residuals, 4),
      `Squared Residual` = round(res$residuals^2, 4)
    )
  }, digits = 4)
  
  output$stats_table <- renderTable({
    res <- model_results()
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
        res$sse,
        res$ssr,
        res$ssto,
        res$r_squared,
        res$mse,
        res$res_std_error
      )
    )
  }, digits = 4)
  
  output$r_code <- renderText({
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
SSE <- sum(residuals^2)
SSR <- sum((y_hat - y_mean)^2)
SSTO <- sum((y - y_mean)^2)

# Calculate R-squared
R2 <- SSR/SSTO

# Calculate MSE and RSE
n <- length(y)
MSE <- SSE/(n-2)
RSE <- sqrt(MSE)
    ', input$x_var, input$y_var)
  })
}

shinyApp(ui, server)
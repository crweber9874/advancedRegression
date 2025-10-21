# inst/shinyapp/app.R
library(shiny)
library(shinythemes)
library(DT)
library(brms)
library(ggplot2)

# UI Definition
ui <- fluidPage(
  theme = shinytheme("flatly"),

  titlePanel("BRMS Regression Analysis"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Step 1: Data Selection
      h4("1. Select Data"),
      selectInput("dataset",
                  "Choose a dataset from workspace:",
                  choices = NULL),
      actionButton("refresh_data", "Refresh Data List"),
      hr(),

      # Step 2: Variable Selection
      h4("2. Select Variables"),

      # Outcome variable
      selectInput("outcome",
                  "Outcome Variable:",
                  choices = NULL),

      # Predictor variables (checkboxes)
      h5("Predictor Variables:"),
      uiOutput("predictor_checklist"),

      # Random effects (optional)
      h5("Random Effects (Optional):"),
      selectInput("random_var",
                  "Grouping Variable:",
                  choices = c("None" = ""),
                  multiple = FALSE),

      radioButtons("random_type",
                   "Random Effect Type:",
                   choices = c("Random Intercept" = "intercept",
                               "Random Slope" = "slope"),
                   selected = "intercept"),

      conditionalPanel(
        condition = "input.random_type == 'slope'",
        selectInput("random_slope_var",
                    "Variable for Random Slope:",
                    choices = NULL)
      ),

      hr(),

      # Step 3: Model Settings
      h4("3. Model Settings"),

      selectInput("family",
                  "Distribution Family:",
                  choices = c("gaussian", "binomial", "poisson",
                              "negbinomial", "gamma", "student"),
                  selected = "gaussian"),

      numericInput("chains", "Number of Chains:",
                   value = 4, min = 1, max = 10),

      numericInput("iter", "Iterations per Chain:",
                   value = 2000, min = 500, max = 10000, step = 500),

      numericInput("warmup", "Warmup Iterations:",
                   value = 1000, min = 100, max = 5000, step = 100),

      numericInput("cores", "Number of Cores:",
                   value = parallel::detectCores() - 1,
                   min = 1,
                   max = parallel::detectCores()),

      hr(),

      # Run button
      actionButton("run_model", "Run BRMS Model",
                   class = "btn-primary btn-block"),

      # Download button for model
      conditionalPanel(
        condition = "output.model_complete",
        hr(),
        downloadButton("download_model", "Download Model (RDS)")
      )
    ),

    mainPanel(
      width = 9,

      tabsetPanel(
        id = "main_tabs",

        # Data Preview Tab
        tabPanel("Data Preview",
                 h4("Dataset Summary"),
                 verbatimTextOutput("data_summary"),
                 h4("Data Preview (First 100 rows)"),
                 DT::dataTableOutput("data_preview")
        ),

        # Model Formula Tab
        tabPanel("Model Formula",
                 h4("Model Formula"),
                 wellPanel(
                   verbatimTextOutput("formula_display")
                 ),
                 h4("Model Specification"),
                 verbatimTextOutput("model_spec")
        ),

        # Results Tab
        tabPanel("Results",
                 conditionalPanel(
                   condition = "output.model_complete == false",
                   h4("No model has been run yet"),
                   p("Please select your data and variables, then click 'Run BRMS Model'")
                 ),
                 conditionalPanel(
                   condition = "output.model_complete",
                   h4("Model Summary"),
                   verbatimTextOutput("model_summary"),

                   h4("Model Diagnostics"),
                   plotOutput("diagnostic_plot", height = "600px"),

                   h4("Posterior Predictive Check"),
                   plotOutput("pp_check", height = "400px")
                 )
        ),

        # Coefficients Tab
        tabPanel("Coefficients",
                 conditionalPanel(
                   condition = "output.model_complete",
                   h4("Coefficient Plot"),
                   plotOutput("coef_plot", height = "500px"),

                   h4("Coefficient Table"),
                   DT::dataTableOutput("coef_table")
                 )
        ),

        # Predictions Tab
        tabPanel("Predictions",
                 conditionalPanel(
                   condition = "output.model_complete",
                   h4("Posterior Predictions"),
                   plotOutput("prediction_plot", height = "500px"),

                   h4("Model Fit Statistics"),
                   verbatimTextOutput("fit_stats")
                 )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {

  # Reactive values to store data and model
  values <- reactiveValues(
    data = NULL,
    model = NULL,
    model_complete = FALSE
  )

  # Get list of data frames from global environment
  get_data_list <- function() {
    objs <- ls(envir = .GlobalEnv)
    data_objs <- c()

    for (obj in objs) {
      if (is.data.frame(get(obj, envir = .GlobalEnv))) {
        data_objs <- c(data_objs, obj)
      }
    }

    if (length(data_objs) == 0) {
      return(c("No data frames found" = ""))
    }

    return(data_objs)
  }

  # Initialize and refresh data list
  observe({
    updateSelectInput(session, "dataset",
                      choices = get_data_list())
  })

  observeEvent(input$refresh_data, {
    updateSelectInput(session, "dataset",
                      choices = get_data_list())
  })

  # Load selected dataset
  observeEvent(input$dataset, {
    req(input$dataset)
    if (input$dataset != "") {
      values$data <- get(input$dataset, envir = .GlobalEnv)

      # Update variable choices
      var_choices <- names(values$data)

      updateSelectInput(session, "outcome",
                        choices = var_choices)

      updateSelectInput(session, "random_var",
                        choices = c("None" = "", var_choices))
    }
  })

  # Create predictor checklist UI
  output$predictor_checklist <- renderUI({
    req(values$data)

    var_names <- names(values$data)

    # Remove outcome variable from predictor list
    if (!is.null(input$outcome) && input$outcome != "") {
      var_names <- var_names[var_names != input$outcome]
    }

    checkboxGroupInput("predictors",
                       label = NULL,
                       choices = var_names,
                       selected = NULL)
  })

  # Update random slope variable choices
  observe({
    if (!is.null(input$predictors)) {
      updateSelectInput(session, "random_slope_var",
                        choices = input$predictors)
    }
  })

  # Generate formula
  get_formula <- reactive({
    req(input$outcome)
    req(input$predictors)

    # Base formula
    pred_terms <- paste(input$predictors, collapse = " + ")
    formula_str <- paste(input$outcome, "~", pred_terms)

    # Add random effects if specified
    if (!is.null(input$random_var) && input$random_var != "") {
      if (input$random_type == "intercept") {
        formula_str <- paste(formula_str, "+ (1 |", input$random_var, ")")
      } else if (input$random_type == "slope" && !is.null(input$random_slope_var)) {
        formula_str <- paste(formula_str, "+ (", input$random_slope_var, "|",
                             input$random_var, ")")
      }
    }

    return(formula_str)
  })

  # Display formula
  output$formula_display <- renderPrint({
    cat(get_formula())
  })

  # Display model specification
  output$model_spec <- renderPrint({
    req(input$outcome, input$predictors)

    cat("Model Specification:\n")
    cat("-------------------\n")
    cat("Family:", input$family, "\n")
    cat("Chains:", input$chains, "\n")
    cat("Iterations:", input$iter, "\n")
    cat("Warmup:", input$warmup, "\n")
    cat("Cores:", input$cores, "\n")

    if (!is.null(input$random_var) && input$random_var != "") {
      cat("\nRandom Effects:\n")
      cat("Grouping Variable:", input$random_var, "\n")
      cat("Type:", input$random_type, "\n")
    }
  })

  # Data preview
  output$data_preview <- DT::renderDataTable({
    req(values$data)
    DT::datatable(head(values$data, 100),
                  options = list(scrollX = TRUE, pageLength = 10))
  })

  # Data summary
  output$data_summary <- renderPrint({
    req(values$data)
    cat("Dataset:", input$dataset, "\n")
    cat("Dimensions:", nrow(values$data), "rows x", ncol(values$data), "columns\n\n")
    str(values$data)
  })

  # Run BRMS model
  observeEvent(input$run_model, {
    req(input$outcome, input$predictors, values$data)

    # Show progress
    withProgress(message = 'Running BRMS Model...', value = 0, {

      incProgress(0.1, detail = "Preparing data...")

      # Prepare formula
      formula_obj <- as.formula(get_formula())

      incProgress(0.2, detail = "Compiling model...")

      # Run model with error handling
      tryCatch({
        values$model <- brm(
          formula = formula_obj,
          data = values$data,
          family = input$family,
          chains = input$chains,
          iter = input$iter,
          warmup = input$warmup,
          cores = input$cores,
          seed = 123,
          silent = 2,
          refresh = 0
        )

        values$model_complete <- TRUE

        incProgress(1, detail = "Model complete!")

        showNotification("Model completed successfully!",
                         type = "success",
                         duration = 5)

        # Switch to results tab
        updateTabsetPanel(session, "main_tabs", selected = "Results")

      }, error = function(e) {
        showNotification(paste("Error running model:", e$message),
                         type = "error",
                         duration = NULL)
        values$model_complete <- FALSE
      })
    })
  })

  # Model complete flag for conditional panels
  output$model_complete <- reactive({
    values$model_complete
  })
  outputOptions(output, "model_complete", suspendWhenHidden = FALSE)

  # Model summary
  output$model_summary <- renderPrint({
    req(values$model)
    summary(values$model)
  })

  # Diagnostic plots
  output$diagnostic_plot <- renderPlot({
    req(values$model)
    plot(values$model, ask = FALSE)
  })

  # Posterior predictive check
  output$pp_check <- renderPlot({
    req(values$model)
    pp_check(values$model, ndraws = 100)
  })

  # Coefficient plot
  output$coef_plot <- renderPlot({
    req(values$model)
    mcmc_plot(values$model, type = "intervals", prob_outer = 0.95)
  })

  # Coefficient table
  output$coef_table <- DT::renderDataTable({
    req(values$model)

    # Extract fixed effects
    fixed_ef <- as.data.frame(fixef(values$model))
    fixed_ef$Parameter <- rownames(fixed_ef)
    fixed_ef <- fixed_ef[, c("Parameter", "Estimate", "Est.Error",
                             "Q2.5", "Q97.5")]

    DT::datatable(fixed_ef,
                  options = list(pageLength = 20),
                  rownames = FALSE) %>%
      DT::formatRound(columns = 2:5, digits = 3)
  })

  # Prediction plot
  output$prediction_plot <- renderPlot({
    req(values$model)

    # Get predictions
    pred_data <- values$data
    pred_data$fitted <- fitted(values$model)[, "Estimate"]

    # Create plot based on family
    if (input$family == "binomial") {
      ggplot(pred_data, aes_string(x = input$outcome, y = "fitted")) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "loess") +
        labs(x = "Observed", y = "Fitted Probability") +
        theme_minimal()
    } else {
      ggplot(pred_data, aes_string(x = input$outcome, y = "fitted")) +
        geom_point(alpha = 0.5) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        labs(x = "Observed", y = "Fitted") +
        theme_minimal()
    }
  })

  # Fit statistics
  output$fit_stats <- renderPrint({
    req(values$model)

    cat("Model Fit Statistics\n")
    cat("====================\n\n")

    # LOO
    loo_result <- loo(values$model)
    print(loo_result)

    cat("\n")

    # R-squared (for gaussian models)
    if (input$family == "gaussian") {
      r2 <- bayes_R2(values$model)
      cat("Bayesian R-squared:\n")
      print(summary(r2))
    }
  })

  # Download model
  output$download_model <- downloadHandler(
    filename = function() {
      paste0("brms_model_", Sys.Date(), ".rds")
    },
    content = function(file) {
      saveRDS(values$model, file)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)

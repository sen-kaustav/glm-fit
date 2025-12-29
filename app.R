library(shiny)
library(bslib)
library(dplyr)

source("plot_code.R")

data <- arrow::read_parquet(fs::path("data", "motor_data.parquet"))
pred_vars <- data |> select(where(is.factor)) |> names()

ui <- page_fillable(
  theme = bs_theme(preset = "zephyr"),
  title = "GLM Fitting",
  card(
    card_header(h3("GLM Fitting and Diagnostics")),
    layout_sidebar(
      sidebar = sidebar(
        width = "400px",
        open = "always",
        # Checkbox list of predictor variables
        div(
          class = "d-flex flex-wrap gap-3",
          checkboxGroupInput(
            "pred_vars_select",
            label = "Select predictor variables:",
            choices = pred_vars
          )
        ),
        actionButton(
          "fit_glm",
          label = "Fit GLM",
          class = "btn-primary mt-1 w-25"
        )
      ),
      layout_columns(
        col_widths = c(3, 9),
        card(
          selectInput(
            "var_analysis",
            "Select variable to analyse:",
            choices = NULL
          ),
        ),
        card(
          min_height = 300,
          card_header("Relativity plot"),
          plotOutput("relativity_plot")
        )
      ),
    ),
  )
)

server <- function(input, output, session) {
  df_coefs <- reactiveVal(NULL)
  show_plot <- reactiveVal(FALSE)

  observeEvent(input$fit_glm, {
    notify_start <- showNotification(
      "Fitting GLM...",
      duration = NULL,
      type = "warning"
    )
    on.exit(removeNotification(notify_start), add = TRUE)

    vars <- input$pred_vars_select

    formula <- as.formula(
      paste(
        "Claim_Count ~ ",
        paste(vars, collapse = "+"),
        "+ offset(log(Exposure))"
      )
    )

    mod <- glm(formula, data = data, family = poisson("log"))

    # Update the reactive value with new coefficients
    df_coefs(broom::tidy(mod, exponentiate = TRUE))

    # Clear out the plot
    show_plot(FALSE)

    updateSelectInput(
      inputId = "var_analysis",
      choices = c("Select variable" = "", vars),
      selected = ""
    )

    showNotification("Results refreshed!", type = "message", duration = 3)
  })

  observeEvent(input$var_analysis, {
    if (input$var_analysis != "") {
      show_plot(TRUE)
    }
  })

  output$relativity_plot <- renderPlot(
    {
      req(show_plot())
      req(input$var_analysis)
      make_relativity_plot(data, df_coefs(), input$var_analysis)
    },
    res = 96
  )
}

shinyApp(ui, server)

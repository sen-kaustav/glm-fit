library(shiny)
library(bslib)
library(dplyr)
library(forcats)

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
          accordion(
            open = FALSE,
            accordion_panel(
              title = "Apply further grouping",
              selectInput(
                "var_grouping",
                "Select categories to group:",
                choices = NULL,
                multiple = TRUE
              ),
              textInput("var_grouping_name", "New group name:"),
              div(
                class = "d-flex gap-2",
                actionButton(
                  "refit_glm",
                  span("Re-fit", icon("shuffle")),
                  class = "btn-primary flex-grow-1"
                ),
                actionButton(
                  "grouping_reset",
                  span("Reset", icon("undo")),
                  class = "btn-danger flex-grow-1"
                )
              )
            )
          )
        ),
        card(
          min_height = 300,
          card_header("Relativity plot"),
          plotOutput("relativity_plot")
        )
      ),
    ),
  ),
)

server <- function(input, output, session) {
  show_plot <- reactiveVal(FALSE)
  mod <- reactiveVal(NULL)
  formula <- reactiveVal(NULL)
  df_coefs <- reactiveVal(NULL)
  df_glm_data <- reactiveVal(data)

  observeEvent(input$fit_glm, {
    notify_start <- showNotification(
      "Fitting GLM...",
      duration = NULL,
      type = "warning"
    )
    on.exit(removeNotification(notify_start), add = TRUE)

    vars <- input$pred_vars_select

    formula(as.formula(
      paste(
        "Claim_Count ~ ",
        paste(vars, collapse = "+"),
        "+ offset(log(Exposure))"
      )
    ))

    mod(glm(formula(), data = df_glm_data(), family = poisson("log")))

    # Update the reactive value with new coefficients
    df_coefs(broom::tidy(mod(), exponentiate = TRUE))

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
      choices <- unique(df_glm_data()[[input$var_analysis]]) |> sort()
      updateSelectInput(inputId = "var_grouping", choices = choices)
      updateTextInput(inputId = "var_grouping_name", value = "")
    }
  })

  observeEvent(input$var_grouping, {
    updateTextInput(
      inputId = "var_grouping_name",
      value = paste0(input$var_grouping, collapse = "_")
    )
  })

  observeEvent(input$refit_glm, {
    req(input$var_grouping, input$var_grouping_name)

    notify_start <- showNotification(
      "Fitting GLM...",
      duration = NULL,
      type = "warning"
    )
    on.exit(removeNotification(notify_start), add = TRUE)
    df_glm_data({
      df_glm_data() |>
        mutate(across(all_of(input$var_analysis), \(x) {
          fct_collapse(
            x,
            !!!setNames(list(input$var_grouping), input$var_grouping_name)
          )
        }))
    })

    # Re-fit GLM model with updated groupings
    mod(glm(formula(), data = df_glm_data(), family = poisson("log")))

    # Update the reactive value with new coefficients
    df_coefs(broom::tidy(mod(), exponentiate = TRUE))

    # Set the grouping input boxes
    choices <- unique(df_glm_data()[[input$var_analysis]]) |> sort()
    updateSelectInput(inputId = "var_grouping", choices = choices)
    updateTextInput(inputId = "var_grouping_name", value = "")

    showNotification("Results refreshed!", type = "message", duration = 3)
  })

  model_reset_confirm <- modalDialog()

  observeEvent(input$grouping_reset, {
    showModal(modalDialog(
      title = "Reset groupings",
      "Are you sure you want to reset all the groups defined and go back to what was in the input data?",
      easyClose = TRUE,
      footer = tagList(
        actionButton("cancel", "Cancel"),
        actionButton("ok", "Reset", class = "btn btn-danger")
      )
    ))
  })

  observeEvent(input$cancel, {
    removeModal()
  })

  observeEvent(input$ok, {
    notify_start <- showNotification(
      "Resetting...",
      duration = NULL,
      type = "warning"
    )
    on.exit(removeNotification(notify_start), add = TRUE)

    # Re-set data gropuing
    df_glm_data(data)

    # Re-fit GLM model with original groupings
    mod(glm(formula(), data = df_glm_data(), family = poisson("log")))

    # Update model coefficients
    df_coefs(broom::tidy(mod(), exponentiate = TRUE))

    # Set the grouping input boxes
    choices <- unique(df_glm_data()[[input$var_analysis]]) |> sort()
    updateSelectInput(inputId = "var_grouping", choices = choices)
    updateTextInput(inputId = "var_grouping_name", value = "")

    showNotification("Results refreshed!", type = "message", duration = 3)
    removeModal()
  })

  output$relativity_plot <- renderPlot(
    {
      req(show_plot())
      req(input$var_analysis)
      make_relativity_plot(df_glm_data(), df_coefs(), input$var_analysis)
    },
    res = 96
  )
}

shinyApp(ui, server)

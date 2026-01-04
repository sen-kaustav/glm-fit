library(shiny)
library(bslib)
library(shinyjs)
library(dplyr)
library(tidyr)
library(recipes)
library(parsnip)
library(poissonreg)
library(workflows)
library(forcats)

source("plot_code.R")
source("create_recipe.R")

ui <- page_fillable(
  theme = bs_theme(preset = "zephyr") |>
    bs_add_rules(sass::sass_file("www/styles.scss")),
  title = "GLM Fitting and Diagnostics",
  useShinyjs(),
  card(
    card_header(h3("GLM Fitting and Diagnostics")),
    layout_sidebar(
      sidebar = sidebar(
        width = "400px",
        open = "open",
        # Checkbox list of predictor variables
        div(
          class = "d-flex flex-wrap",
          fileInput(
            "input_file",
            label = "Upload file for analysis",
            placeholder = "parquet files only",
            accept = ".parquet"
          ),
          hidden(
            div(
              id = "pred_vars_input",
              h5("Select predictor variables"),
              checkboxGroupInput(
                "pred_vars_select",
                label = "Categorical variables:",
                choices = NULL
              ),
              div(
                id = "pred_cont_vars_container",
                strong("Continuous variables:"),
                uiOutput("pred_cont_vars")
              ),
            )
          ),
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
            choices = c("Select variable" = "")
          ),
          hidden(
            accordion(
              open = FALSE,
              id = "var_grouping_ui",
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

  formula <- reactiveVal(NULL)
  vars_cont_df <- reactiveVal(NULL)
  spec_recipe <- reactiveVal(NULL)
  spec_model <- reactiveVal(NULL)
  wflow <- reactiveVal(NULL)

  df_coefs <- reactiveVal(NULL)
  df_orig <- reactiveVal(NULL)
  df_glm_data <- reactiveVal(NULL)
  df_glm_data_addnl_group <- reactiveVal(NULL)

  # Set file uploads to a maximum of 30 MB
  options(shiny.maxRequestSize = 30 * 1024^2)

  observeEvent(input$input_file, {
    req(input$input_file)
    df_upload <- arrow::read_parquet(input$input_file$datapath)
    df_orig(df_upload)
    df_glm_data_addnl_group(df_upload)
    df_glm_data(df_upload)

    # Reset analysis in case already present
    show_plot(FALSE)
    updateSelectInput(
      inputId = "var_analysis",
      choices = character(0),
      selected = ""
    )
  })

  pred_choices <- reactive(
    df_orig() |> select(where(is.factor)) |> names()
  )

  pred_cont_choices <- reactive({
    req(df_orig())
    all_cols <- df_orig() |> select(where(is.numeric)) |> names()
    cols_to_remove <- c("ID", "Exposure", "Claim_Count", "Claim_Amount")
    setdiff(all_cols, cols_to_remove)
  })

  observeEvent(
    input$input_file,
    {
      updateCheckboxGroupInput(
        inputId = "pred_vars_select",
        choices = pred_choices()
      )

      # Show options to select variables
      show(id = "pred_vars_input", anim = TRUE)
    }
  )

  output$pred_cont_vars <- renderUI({
    purrr::map(pred_cont_choices(), \(item) {
      tagList(
        checkboxInput(paste0("cont_var_name_", item), label = item),
        hidden(div(
          class = "cont_var_options",
          id = paste0("cont_var_options_", item),
          selectInput(
            paste0("cont_var_scale_", item),
            label = "Scale",
            choices = c("None", "Log", "Sqrt"),
            selected = "None",
            selectize = FALSE
          ),
          selectInput(
            paste0("cont_var_transform_", item),
            label = "Transform",
            choices = c("None", "Centre", "Normalise"),
            selectize = FALSE
          ),
          selectInput(
            paste0("cont_var_poly_", item),
            label = "Polynomial degree",
            choices = seq_len(3),
            selected = 1,
            selectize = FALSE
          )
        )),
      )
    })
  })

  observe({
    req(pred_cont_choices())
    lapply(pred_cont_choices(), \(var) {
      observeEvent(input[[paste0("cont_var_name_", var)]], {
        toggle(
          paste0("cont_var_options_", var),
          anim = TRUE,
          condition = input[[paste0("cont_var_name_", var)]]
        )
      })
    })
  })

  observeEvent(input$fit_glm, {
    notify_start <- showNotification(
      "Fitting GLM...",
      duration = NULL,
      type = "warning"
    )
    on.exit(removeNotification(notify_start), add = TRUE)

    vars <- input$pred_vars_select

    vars_cont_labels <- names(input)[grep(
      "cont_var",
      names(input),
      fixed = TRUE
    )]
    vars_cont_values <- sapply(vars_cont_labels, \(var) input[[var]])
    vars_cont_df(
      tibble(vars_cont_labels, vars_cont_values) |>
        separate_wider_regex(
          vars_cont_labels,
          patterns = c("cont_var", "_", name = "[a-z]+", "_", var = ".*")
        ) |>
        pivot_wider(names_from = "name", values_from = vars_cont_values) |>
        mutate(poly = as.numeric(poly), name = as.logical(name)) |>
        filter(name)
    )

    vars_all <- c(vars, vars_cont_df()$var)

    formula(as.formula(
      paste(
        "Claim_Count ~ ",
        paste(vars_all, collapse = "+"),
        " + Exposure"
      )
    ))

    if (is.null(spec_recipe())) {
      spec_recipe(create_recipe(
        vars_cat = vars,
        vars_cont_df = vars_cont_df(),
        formula = formula(),
        train_df = df_glm_data()
      ))
    } else {
      spec_recipe(create_recipe(
        vars_cat = vars,
        vars_cont_df = vars_cont_df(),
        formula = formula(),
        train_df = df_glm_data_addnl_group()
      ))
      df_glm_data(df_glm_data_addnl_group())
    }

    spec_model(
      poisson_reg() |>
        set_engine("glm", family = poisson("log"))
    )

    wflow(
      workflow() |>
        add_recipe(spec_recipe()) |>
        add_model(
          spec_model(),
          formula = Claim_Count ~ . - Exposure + offset(log(Exposure))
        ) |>
        fit(data = df_glm_data())
    )

    # Update the reactive value with new coefficients
    df_coefs(broom::tidy(wflow(), exponentiate = TRUE))

    # Clear out the plot
    show_plot(FALSE)

    updateSelectInput(
      inputId = "var_analysis",
      # choices = c("Select variable" = "", vars)
      choices = list(
        "Select variable" = "",
        "Categorical variables" = as.list(vars),
        "Continuous variables" = as.list(vars_cont_df()$var)
      )
    )

    showNotification("Results refreshed!", type = "message", duration = 3)
  })

  observeEvent(input$var_analysis, {
    req(input$var_analysis, input$var_analysis != "")
    show_plot(TRUE)
    if (input$var_analysis %in% input$pred_vars_select) {
      choices <- unique(df_glm_data()[[input$var_analysis]]) |> sort()
      updateSelectInput(inputId = "var_grouping", choices = choices)
      updateTextInput(inputId = "var_grouping_name", value = "")
      show(id = "var_grouping_ui")
    } else {
      hide(id = "var_grouping_ui")
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

    spec_recipe(
      spec_recipe() |>
        step_mutate(
          across(all_of(input$var_analysis), \(x) {
            fct_collapse(
              x,
              !!!setNames(list(input$var_grouping), input$var_grouping_name)
            )
          })
        )
    )

    # Apply the updated groupings to the GLM data with additional grouping.
    # This ensures the groupings are retained when a new model is re-fit (without reverting back to the original levels)
    df_glm_data_addnl_group(
      df_glm_data_addnl_group() |>
        mutate(
          across(all_of(input$var_analysis), \(x) {
            fct_collapse(
              x,
              !!!setNames(list(input$var_grouping), input$var_grouping_name)
            )
          })
        )
    )

    # Re-fit GLM model with updated groupings
    wflow(
      wflow() |>
        update_recipe(spec_recipe()) |>
        fit(data = df_glm_data())
    )

    # Update the reactive value with new coefficients
    df_coefs(broom::tidy(wflow(), exponentiate = TRUE))

    # Set the grouping input boxes
    choices <- unique(df_glm_data()[[input$var_analysis]]) |> sort()
    updateSelectInput(inputId = "var_grouping", choices = choices)
    updateTextInput(inputId = "var_grouping_name", value = "")

    showNotification("Results refreshed!", type = "message", duration = 3)
  })

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

    # Re-set data grouping by re-creating original recipe
    spec_recipe(
      create_recipe(
        vars_cat = input$pred_vars_select,
        vars_cont_df = vars_cont_df(),
        formula = formula(),
        train_df = df_orig()
      )
    )

    # Re-fit GLM model with original groupings
    wflow(
      wflow() |>
        update_recipe(spec_recipe()) |>
        fit(data = df_orig())
    )

    # Re-set dataframes to be the original input
    df_glm_data(df_orig())
    df_glm_data_addnl_group(df_orig())

    # Update model coefficients
    df_coefs(broom::tidy(wflow(), exponentiate = TRUE))

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
      df_plot <- extract_preprocessor(wflow()) |>
        prep() |>
        bake(new_data = NULL)
      make_relativity_plot(df_plot, df_coefs(), input$var_analysis)
    },
    res = 96
  )
}

shinyApp(ui, server)

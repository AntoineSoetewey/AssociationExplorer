library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(scales)
library(reactable)
library(tidygraph)
library(visNetwork)
library(readxl)
library(grid)
library(gridExtra)
library(janitor)
library(shinyjs)
library(tibble)
library(shinycssloaders)

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  fluidPage(
    shinyjs::useShinyjs(),
    class = "app-container",
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#0072B2",
      base_font = font_google("Roboto"),
      heading_font = font_google("Roboto Slab"),
      code_font = font_google("Fira Code")
    ),
    tags$div(class = "centered-padding-top"),
    titlePanel(div("Association Explorer", class = "app-title")),
    br(),
    tabsetPanel(
      id = "main_tabs",
      type = "tabs",
      tabPanel(
        title = tags$strong("📁 Data"),
        value = "upload_tab",
        br(),
        br(),
        fileInput("data_file", "Upload your dataset (CSV or Excel)",
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/plain",
            ".csv",
            ".xlsx",
            ".xls"
          )
        ),
        fileInput("desc_file", "(Optional) Upload variable descriptions (CSV or Excel)",
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/plain",
            ".csv",
            ".xlsx",
            ".xls"
          )
        ),
        tags$p(
          style = "font-size:0.85em; color: #666666;",
          "The descriptions file must contain exactly two columns named 'Variable' and 'Description'."
        ),
        br(),
        actionButton("process_data", "Process data", class = "btn btn-primary")
      ),
      tabPanel(
        title = tags$strong("🔍 Variables"),
        value = "variables_tab",
        br(),
        br(),
        uiOutput("variable_checkboxes_ui"),
        br(),
        uiOutput("go_to_network_ui"),
        br(),
        br(),
        br(),
        uiOutput("selected_vars_table_ui")
      ),
      tabPanel(
        title = tags$strong("🔗 Correlation Network"),
        value = "network_tab",
        sidebarLayout(
          sidebarPanel(
            sliderInput("threshold_num", "Threshold for Quantitative-Quantitative and Quantitative-Categorical Associations (R²)",
              min = 0, max = 1, value = 0.5, step = 0.05
            ),
            sliderInput("threshold_cat", "Threshold for Categorical-Categorical Associations (Cramer's V)",
              min = 0, max = 1, value = 0.5, step = 0.05
            ),
            tags$i(tags$span(
              style = "color: #666666",
              "Only associations stronger than the thresholds will be displayed in the plot."
            )),
            br(),
            br(),
            fluidRow(
              column(12,
                align = "center",
                actionButton("go_to_pairs", "See pairs plots", class = "btn btn-primary")
              )
            )
          ),
          mainPanel(
            class = "panel-white",
            withSpinner(visNetworkOutput("network_vis", height = "600px", width = "100%"), type = 6, color = "#0072B2")
          )
        )
      ),
      tabPanel(
        title = tags$strong("📊 Pairs Plots"),
        value = "pairs_tab",
        fluidPage(
          class = "panel-white",
          withSpinner(uiOutput("pairs_plot"), type = 6, color = "#0072B2")
        )
      ),
      tabPanel(
        title = tags$strong("❓ Help"),
        value = "help_tab",
        div(
          class = "help-container",
          br(),
          br(),
          h3("How to use the Association Explorer app?"),
          br(),
          tags$ul(
            tags$li("Upload your dataset (CSV or Excel) in the 'Data' tab. Optionally, upload a file with variable descriptions. This file must contain 2 columns called 'Variable' and 'Description'."),
            tags$li("In the 'Variables' tab, select the variables you want to explore. If you upload a file containing variables' descriptions, a summary table below shows the selected variables along with their descriptions."),
            tags$li("Click 'Visualize all associations' to access the correlation network."),
            tags$li("Adjust the thresholds to filter associations by strength. Only variables that have strong associations (as defined by the thresholds) will appear in the network and pairs plots."),
            tags$li("In the network, thicker and shorter edges indicate stronger associations."),
            tags$li("Click 'See pairs plots' to display bivariate visualizations for retained associations.")
          )
        )
      )
    ),
    br(),
    tags$hr(),
    tags$footer(
      class = "app-footer",
      "v3.5.5. See the ",
      tags$a(href = "https://github.com/AntoineSoetewey/AssociationExplorer", "code.", target = "_blank")
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  var_descriptions <- reactiveVal(NULL)

  observeEvent(input$process_data, {
    req(input$data_file)

    # Read the uploaded data
    data_path <- input$data_file$datapath
    if (grepl("\\.csv$", data_path, ignore.case = TRUE)) {
      data_df <- read.csv(data_path, stringsAsFactors = TRUE)
    } else if (grepl("\\.(xlsx|xls)$", data_path, ignore.case = TRUE)) {
      data_df <- read_excel(data_path, stringsAsFactors = TRUE)
    } else {
      stop("Unsupported file format for data file.")
    }

    # Remove variables with all equal values (e.g., variance zero)
    original_names <- names(data_df)
    data_df <- data_df[, sapply(data_df, function(x) length(unique(x[!is.na(x)])) > 1), drop = FALSE]

    # Store filtered data
    data(data_df)

    # Show a warning if variables were removed
    removed_vars <- setdiff(original_names, names(data_df))
    if (length(removed_vars) > 0) {
      showNotification(
        paste(
          "The following variables were removed because they contain only one unique value:",
          paste(removed_vars, collapse = ", ")
        ),
        type = "warning"
      )
    }

    # Initialize descriptions with variable names as default descriptions
    default_descriptions <- data.frame(
      variable = names(data_df),
      description = names(data_df),
      stringsAsFactors = FALSE
    )

    # Read the uploaded descriptions if a file is provided
    if (!is.null(input$desc_file)) {
      # 1) read the uploaded file
      desc_path <- input$desc_file$datapath
      if (grepl("\\.csv$", desc_path, ignore.case = TRUE)) {
        user_desc <- read.csv(desc_path, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        user_desc <- read_excel(desc_path, stringsAsFactors = FALSE)
      }

      # Trim whitespace from column names
      colnames(user_desc) <- trimws(colnames(user_desc))

      # Validate the description file
      validation_passed <- TRUE

      if (length(colnames(user_desc)) != 2) {
        showNotification(
          "The description file must contain exactly two columns named 'Variable' and 'Description'.",
          type = "error",
          duration = NULL
        )
        validation_passed <- FALSE
      } else if (!all(c("Variable", "Description") %in% colnames(user_desc))) {
        showNotification(
          paste(
            "The description file must contain exactly two columns named 'Variable' and 'Description'.",
            "Found columns:", paste(sQuote(colnames(user_desc)), collapse = ", ")
          ),
          type = "error",
          duration = NULL
        )
        validation_passed <- FALSE
      }

      if (validation_passed) {
        # If validation passes, continue with processing
        user_desc <- user_desc |>
          janitor::clean_names() |>
          select(variable, description)

        merged_desc <- default_descriptions |>
          left_join(user_desc, by = "variable") |>
          mutate(
            description = ifelse(
              is.na(description.y) | description.y == "",
              variable,
              description.y
            )
          ) |>
          select(variable, description)
        var_descriptions(merged_desc)
      } else {
        var_descriptions(default_descriptions)
      }
    } else {
      var_descriptions(default_descriptions)
    }

    # Redirect to the Variables tab after processing the data
    updateTabsetPanel(session, "main_tabs", selected = "variables_tab")
  })

  output$variable_checkboxes_ui <- renderUI({
    req(data())
    selectizeInput(
      inputId = "selected_vars",
      label = "Select variables to include:",
      choices = names(data()),
      selected = names(data()),
      multiple = TRUE,
      width = "100%", # ⬅️ This makes the input take full width of its container
      options = list(
        maxItems = NULL,
        plugins = list("remove_button"),
        placeholder = "Choose variables...",
        openOnFocus = TRUE
      )
    )
  })

  valid_selected_vars <- reactive({
    req(input$selected_vars)
    input$selected_vars
  })

  output$go_to_network_ui <- renderUI({
    req(input$selected_vars)
    actionButton("go_to_network", "Visualize all associations", class = "btn btn-primary")
  })

  output$selected_vars_table_ui <- renderUI({
    req(input$selected_vars)
    # hide the table unless the user has uploaded a custom descriptions file
    req(input$desc_file)
    reactableOutput("selected_vars_table")
  })

  output$selected_vars_table <- renderReactable({
    req(var_descriptions())
    req(valid_selected_vars())

    df <- tibble(variable = valid_selected_vars()) |>
      left_join(var_descriptions(), by = "variable")

    cols <- list(
      variable = colDef(name = "Variable", minWidth = 150),
      description = colDef(name = "Description", html = TRUE, minWidth = 400)
    )

    make_table(df, cols)
  })

  observeEvent(input$go_to_network, {
    updateTabsetPanel(session, inputId = "main_tabs", selected = "network_tab")
  })

  observeEvent(input$go_to_pairs, {
    updateTabsetPanel(session, inputId = "main_tabs", selected = "pairs_tab")
  })

  cor_matrix_reactive <- reactive({
    req(data())
    selected_vars <- valid_selected_vars()
    selected_data <- data()[, selected_vars, drop = FALSE]
    calculate_correlations(selected_data, input$threshold_num, input$threshold_cat)
  })

  cor_matrix_vals <- reactive({
    cor_matrix_reactive()
  })

  filtered_data_for_pairs <- reactive({
    mat <- cor_matrix_vals()$cor_matrix
    nodes_to_keep <- rowSums(abs(mat) > 0) > 1
    filtered_matrix <- mat[nodes_to_keep, nodes_to_keep]
    data()[, colnames(filtered_matrix), drop = FALSE]
  })

  significant_pairs <- reactive({
    mat <- cor_matrix_vals()$cor_matrix
    nodes_to_keep <- rowSums(abs(mat) > 0) > 1
    filtered_matrix <- mat[nodes_to_keep, nodes_to_keep]
    pairs <- which(filtered_matrix != 0 & upper.tri(filtered_matrix), arr.ind = TRUE)
    if (nrow(pairs) == 0) {
      return(NULL)
    }
    data.frame(
      var1 = rownames(filtered_matrix)[pairs[, 1]],
      var2 = colnames(filtered_matrix)[pairs[, 2]],
      stringsAsFactors = FALSE
    )
  })

  output$network_vis <- renderVisNetwork({
    cor_result <- cor_matrix_reactive()
    cor_matrix <- cor_result$cor_matrix
    cor_type_matrix <- cor_result$cor_type_matrix

    nodes_to_keep <- rowSums(abs(cor_matrix) > 0) > 1
    mat <- cor_matrix[nodes_to_keep, nodes_to_keep]
    type_mat <- cor_type_matrix[nodes_to_keep, nodes_to_keep]

    validate(
      need(
        ncol(mat) > 0,
        "No associations above the thresholds. Please adjust the thresholds or select different variables."
      )
    )

    # 1) Prepare nodes with descriptions instead of names
    nodes <- data.frame(id = colnames(mat), stringsAsFactors = FALSE) |>
      left_join(var_descriptions(), by = c("id" = "variable")) |>
      mutate(
        label = id, # keep the variable code as label
        title = description, # on hover, the description will be shown
        size = 15 # default size
      ) |>
      select(id, label, title, size)

    # 2) Prepare edges with appropriate correlation type
    edgelist <- which(mat != 0 & upper.tri(mat), arr.ind = TRUE)
    edges <- data.frame(
      from = rownames(mat)[edgelist[, 1]],
      to = colnames(mat)[edgelist[, 2]],
      width = 1 + 4 * (abs(mat[edgelist]) - min(abs(mat[edgelist]))) / (max(abs(mat[edgelist])) - min(abs(mat[edgelist]))),
      color = ifelse(mat[edgelist] > 0, "steelblue", "darkred"),
      title = paste0(type_mat[edgelist], " = ", round(mat[edgelist], 2)),
      stringsAsFactors = FALSE
    )

    # Adjust edge lengths based on association strengths
    strengths <- abs(mat[edgelist])
    min_len <- 100 # min length (strong association)
    max_len <- 500 # max length (weak association)
    edges$length <- (1 - strengths) * (max_len - min_len) + min_len

    # 3) Build the plot
    visNetwork(nodes, edges, width = "100%", height = "900px") |>
      visNodes(
        color = list(
          background = "lightgray",
          border = "lightgray",
          highlight = list(border = "darkgray", background = "darkgray")
        )
      ) |>
      visEdges(smooth = FALSE) |>
      visPhysics(
        enabled = TRUE,
        stabilization = TRUE,
        solver = "forceAtlas2Based"
      ) |>
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = FALSE,
        manipulation = FALSE
      ) |>
      visInteraction(
        zoomView = TRUE,
        dragView = FALSE,
        navigationButtons = FALSE
      ) |>
      visLayout(randomSeed = 123)
  })

  output$pairs_plot <- renderUI({
    req(input$main_tabs == "pairs_tab")
    pairs <- significant_pairs()
    if (is.null(pairs) || nrow(pairs) == 0) {
      return(tags$p(
        "No variable pairs exceed the threshold to display bivariate plots. Please adjust the thresholds or select different variables.",
        style = "color: gray;"
      ))
    }
    df <- filtered_data_for_pairs()
    tabs <- lapply(seq_len(nrow(pairs)), function(i) {
      v1 <- pairs$var1[i]
      v2 <- pairs$var2[i]

      # Get the descriptions
      desc1 <- var_descriptions()$description[var_descriptions()$variable == v1]
      desc2 <- var_descriptions()$description[var_descriptions()$variable == v2]

      plotname <- paste0("plot_", i)
      is_num1 <- is.numeric(df[[v1]])
      is_num2 <- is.numeric(df[[v2]])

      # Create a clean subset without NAs for these variables
      plot_data <- df %>%
        filter(!is.na(.data[[v1]]), !is.na(.data[[v2]]))

      # Numeric vs numeric case
      if (is_num1 && is_num2) {
        # Calculate correlation only with complete cases
        current_cor <- if (nrow(plot_data) > 0) {
          cor(plot_data[[v1]], plot_data[[v2]], use = "complete.obs")
        } else {
          NA
        }

        output[[plotname]] <- renderPlot({
          if (nrow(plot_data) > 0) {
            ggplot(plot_data, aes(x = .data[[v1]], y = .data[[v2]])) +
              geom_jitter(
                alpha = 0.6,
                color = "steelblue",
                width = 0.5,
                height = 0.5
              ) +
              geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1) +
              labs(
                x = desc1,
                y = desc2
              ) +
              scale_x_continuous(labels = label_number(big.mark = ",", decimal.mark = ".")) +
              scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".")) +
              theme_minimal(base_size = 14)
          } else {
            plot.new()
            text(0.5, 0.5, "No valid data available",
              cex = 1.5, adj = 0.5
            )
          }
        })
        nav_panel(paste0(v1, " vs ", v2), plotOutput(plotname, height = "600px"))
      }
      # Categorical vs categorical case (table)
      else if (!is_num1 && !is_num2) {
        output[[plotname]] <- renderUI({
          if (nrow(plot_data) > 0) {
            build_contingency_table(plot_data, v1, v2)
          } else {
            div("No valid data available", style = "padding: 20px; text-align: center;")
          }
        })
        nav_panel(paste0(v1, " vs ", v2), uiOutput(plotname))
      }
      # Mixed case (numeric vs categorical)
      else {
        if (is_num1) {
          num_var <- v1
          cat_var <- v2
          desc_num <- desc1
          desc_cat <- desc2
        } else {
          num_var <- v2
          cat_var <- v1
          desc_num <- desc2
          desc_cat <- desc1
        }

        output[[plotname]] <- renderPlot({
          if (nrow(plot_data) > 0) {
            df_sum <- plot_data |>
              group_by(.data[[cat_var]]) |>
              summarise(
                mean_val = mean(.data[[num_var]], na.rm = TRUE),
                .groups = "drop"
              ) |>
              arrange(mean_val) |>
              mutate({{ cat_var }} := factor(.data[[cat_var]], levels = .data[[cat_var]]))

            ggplot(df_sum, aes(x = .data[[cat_var]], y = mean_val)) +
              geom_col(fill = "steelblue", width = 0.6) +
              geom_text(
                aes(label = format(round(mean_val, 2),
                  big.mark = ",", decimal.mark = "."
                )),
                hjust = 1.1, color = "white", size = 4
              ) +
              labs(
                x = desc_cat,
                y = paste0('Mean of "', desc_num, '"')
              ) +
              scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".")) +
              theme_minimal(base_size = 14) +
              coord_flip()
          } else {
            plot.new()
            text(0.5, 0.5, "No valid data available",
              cex = 1.5, adj = 0.5
            )
          }
        })
        nav_panel(paste0(v1, " vs. ", v2), plotOutput(plotname, height = "600px"))
      }
    })
    tagList(navset_card_tab(id = "bivariate_tabs", !!!tabs))
  })

  # Function to calculate correlation matrix based on variable types
  calculate_correlations <- function(data, threshold_num, threshold_cat) {
    vars <- names(data)
    n <- length(vars)
    cor_matrix <- matrix(0, n, n, dimnames = list(vars, vars))
    cor_type_matrix <- matrix("", n, n, dimnames = list(vars, vars)) # Matrix to store correlation type
    combs <- combn(vars, 2, simplify = FALSE)

    for (pair in combs) {
      v1 <- pair[1]
      v2 <- pair[2]
      is_num1 <- is.numeric(data[[v1]])
      is_num2 <- is.numeric(data[[v2]])
      cor_val <- 0
      cor_type <- ""

      # Get complete cases for these two variables
      complete_cases <- complete.cases(data[[v1]], data[[v2]])
      x <- data[[v1]][complete_cases]
      y <- data[[v2]][complete_cases]
      
      # Numeric vs numeric case
      if (is_num1 && is_num2) {
        if (length(x) > 0 && length(y) > 0) {
          r <- cor(x, y, use = "complete.obs")
          cor_val <- ifelse(r^2 >= threshold_num, r, 0)
          cor_type <- "Pearson's r"
        }
        
      # Categorical vs categorical case
      } else if (!is_num1 && !is_num2) {
        if (length(x) > 0 && length(y) > 0) {
          tbl <- table(x, y)
          if (nrow(tbl) > 1 && ncol(tbl) > 1) { # Need at least 2 categories in each
            chi <- tryCatch(
              {
                chisq.test(tbl, simulate.p.value = TRUE)
              },
              error = function(e) NULL
            )

            if (!is.null(chi)) {
              n_obs <- sum(tbl)
              df_min <- min(nrow(tbl) - 1, ncol(tbl) - 1)
              if (df_min > 0) {
                v_cramer <- sqrt(chi$statistic / (n_obs * df_min))
                cor_val <- ifelse(v_cramer >= threshold_cat, v_cramer, 0)
                cor_type <- "Cramer's V"
              }
            }
          }
        }
        
      # Mixed case (numeric vs categorical)
      } else {
        if (is_num1) {
          num_var <- x
          cat_var <- y
        } else {
          num_var <- y
          cat_var <- x
        }

        if (length(num_var) > 0 && length(cat_var) > 0) {
          means_by_group <- tapply(num_var, cat_var, mean, na.rm = TRUE)
          overall_mean <- mean(num_var, na.rm = TRUE)
          n_groups <- tapply(num_var, cat_var, length)
          bss <- sum(n_groups * (means_by_group - overall_mean)^2, na.rm = TRUE)
          tss <- sum((num_var - overall_mean)^2, na.rm = TRUE)

          if (tss > 0) {
            eta <- sqrt(bss / tss)
            cor_val <- ifelse(eta^2 >= threshold_num, eta, 0)
            cor_type <- "Eta"
          }
        }
      }

      cor_matrix[v1, v2] <- cor_matrix[v2, v1] <- cor_val
      cor_type_matrix[v1, v2] <- cor_type_matrix[v2, v1] <- cor_type
    }

    diag(cor_matrix) <- 1
    list(cor_matrix = cor_matrix, cor_type_matrix = cor_type_matrix)
  }

  make_table <- function(df, columns_defs) {
    reactable(
      df,
      columns = columns_defs,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(25, 50),
      theme = reactableTheme(headerStyle = list(fontWeight = "bold"))
    )
  }

  # Fonction to build contigency table
  build_contingency_table <- function(df, v1, v2) {
    desc1 <- var_descriptions()$description[var_descriptions()$variable == v1]
    desc2 <- var_descriptions()$description[var_descriptions()$variable == v2]

    # Ensure df has the correct columns
    if (!v1 %in% colnames(df) || !v2 %in% colnames(df)) {
      return(div("Invalid variable names for contingency table", style = "color:red"))
    }

    # Create a version of the data with NAs removed for the contingency table
    df_clean <- df[complete.cases(df[, c(v1, v2)]), ]

    if (nrow(df_clean) == 0) {
      return(div("No valid data available", style = "padding: 20px; text-align: center;"))
    }

    tbl <- table(df_clean[[v1]], df_clean[[v2]])
    if (length(tbl) == 0) {
      return(div("No valid data available", style = "padding: 20px; text-align: center;"))
    }

    tbl_with_margins <- addmargins(tbl)

    df_table <- as.data.frame.matrix(tbl_with_margins)
    df_table <- tibble::rownames_to_column(df_table, var = desc1)

    inner_vals <- tbl
    min_val <- min(inner_vals)
    max_val <- max(inner_vals)
    pal <- colorRampPalette(c("#e1f5fe", "#0288d1"))(100)

    column_defs <- lapply(seq_along(df_table), function(j) {
      colname <- names(df_table)[j]
      if (colname == desc1) {
        colDef(name = desc1, minWidth = 150)
      } else {
        colDef(
          name = colname,
          align = "center",
          cell = function(value, index) {
            is_total <- df_table[[desc1]][index] == "Sum" || colname == "Sum"
            val <- as.numeric(value)
            label <- format(val, big.mark = ",", decimal.mark = ".", scientific = FALSE)
            if (is_total) {
              return(label)
            }
            idx <- if (max_val > min_val) {
              max(1, min(100, floor(99 * (val - min_val) / (max_val - min_val)) + 1))
            } else {
              50
            }
            div(style = paste0("background-color:", pal[idx], "; padding:4px;"), label)
          }
        )
      }
    })
    names(column_defs) <- names(df_table)

    tagList(
      div(class = "reactable-title", desc2),
      make_table(df_table, column_defs)
    )
  }
}

shinyApp(ui, server)

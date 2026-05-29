library(shiny)
library(ggplot2)
library(dplyr)
library(rmarkdown)
library(scales)

# ── Data ──────────────────────────────────────────────────────────────────────
housing <- read.csv("EDA_AmesHousing.csv", stringsAsFactors = FALSE)

num_vars <- c(
  "Sale Price ($)"                   = "SalePrice",
  "Above-Ground Living Area (sq ft)" = "Gr.Liv.Area",
  "Overall Quality (1–10)"      = "Overall.Qual",
  "Year Built"                       = "Year.Built",
  "Bedrooms Above Grade"             = "Bedroom.AbvGr",
  "Full Bathrooms"                   = "Full.Bath",
  "Total Basement Area (sq ft)"      = "Total.Bsmt.SF",
  "Garage Area (sq ft)"              = "Garage.Area"
)

cat_vars <- c(
  "Building Type" = "Bldg.Type",
  "Neighborhood"  = "Neighborhood",
  "Central Air"   = "Central.Air"
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 14px; }
    h2 { font-size: 20px; font-weight: 600; margin-top: 0; }
    h4 { font-size: 15px; font-weight: 600; color: #333; margin-top: 18px; margin-bottom: 4px; }
    .well { background: #f7f7f7; border: 1px solid #e0e0e0; border-radius: 6px; padding: 14px; }
    .rq-box { background: #eef4fb; border-left: 4px solid #2c6fad; padding: 10px 14px;
               border-radius: 0 6px 6px 0; margin-bottom: 4px; font-size: 13px; }
    .stat-box { background: #fff; border: 1px solid #ddd; border-radius: 6px;
                padding: 10px 14px; margin-bottom: 8px; }
    .stat-label { font-size: 12px; color: #777; margin-bottom: 2px; }
    .stat-value { font-size: 22px; font-weight: 600; color: #2c6fad; }
    .nav-tabs > li > a { font-size: 13px; }
    .answer-label { font-size: 11px; color: #888; font-style: italic;
                    margin-top: 2px; margin-bottom: 2px; }
    .answer-input textarea { background: #fffdf5; border: 1px dashed #bbb;
                              border-radius: 4px; font-size: 13px; }
    .answer-input { margin-bottom: 14px; }
    .preview-answer { background: #f9f9f9; border: 1px solid #ddd; border-radius: 4px;
                      padding: 8px 12px; font-size: 13px; white-space: pre-wrap;
                      margin-bottom: 4px; }
    .preview-label { font-weight: 600; font-size: 13px; margin-top: 14px; }
    .download-note { color: #e05c2a; font-style: italic; font-size: 13px; }
  "))),

  titlePanel("Ames Housing Data — Descriptive Analytics Explorer"),

  tabsetPanel(

    # ── TAB 1: Dataset Overview ──────────────────────────────────────────────
    tabPanel("Dataset Overview",
      br(),
      fluidRow(
        column(8,
          h2("What is this dataset?"),
          p("This dataset contains information on", strong("2,930 residential home sales"),
            "in Ames, Iowa from 2006–2010. Each row is one house sale.",
            "There are 82 variables describing each property."),
          p("Before we can analyze data, we need to understand its structure.",
            "Use the controls on the right to explore the dataset."),
          br(),
          h4("Research Questions"),

          div(class = "rq-box",
              "RQ 1. How many observations and variables does this dataset have?
               What does one row represent?"),
          div(class = "answer-label", "Your answer:"),
          div(class = "answer-input",
              textAreaInput("ans1", NULL, rows = 3, width = "100%",
                            placeholder = "Type your answer here...")),

          div(class = "rq-box",
              "RQ 2. Look at the first few rows. Can you identify one numerical
               variable and one categorical variable?"),
          div(class = "answer-label", "Your answer:"),
          div(class = "answer-input",
              textAreaInput("ans2", NULL, rows = 3, width = "100%",
                            placeholder = "Type your answer here...")),

          div(class = "rq-box",
              "RQ 3. What does the Sale Price variable tell us?
               What are reasonable minimum and maximum values?"),
          div(class = "answer-label", "Your answer:"),
          div(class = "answer-input",
              textAreaInput("ans3", NULL, rows = 3, width = "100%",
                            placeholder = "Type your answer here...")),

          br(),
          h4("First 10 rows of selected variables"),
          tableOutput("previewTable")
        ),
        column(4,
          wellPanel(
            h4("Choose variables to preview"),
            checkboxGroupInput("previewVars", label = NULL,
                               choices  = c(num_vars, cat_vars),
                               selected = c("SalePrice", "Gr.Liv.Area", "Overall.Qual",
                                            "Year.Built", "Bldg.Type", "Neighborhood"))
          ),
          br(),
          wellPanel(
            h4("Dataset dimensions"),
            div(class = "stat-box",
                div(class = "stat-label", "Rows (observations)"),
                div(class = "stat-value", nrow(housing))),
            div(class = "stat-box",
                div(class = "stat-label", "Columns (variables)"),
                div(class = "stat-value", ncol(housing)))
          )
        )
      )
    ),

    # ── TAB 2: Histogram ─────────────────────────────────────────────────────
    tabPanel("Histogram",
      br(),
      fluidRow(
        column(4,
          wellPanel(
            h4("Variable"),
            selectInput("histVar", NULL, choices = num_vars, selected = "SalePrice"),
            h4("Number of bins"),
            sliderInput("histBins", NULL, min = 5, max = 60, value = 30, step = 1),
            h4("Filter by building type"),
            checkboxGroupInput("histBldg", NULL,
                               choices  = sort(unique(housing$Bldg.Type)),
                               selected = sort(unique(housing$Bldg.Type)))
          ),
          br(),
          wellPanel(
            h4("Summary statistics"),
            uiOutput("histStats")
          )
        ),
        column(8,
          plotOutput("histPlot", height = "380px"),
          br(),
          h4("Research Questions"),

          div(class = "rq-box",
              "RQ 4. Set the variable to Sale Price and bins to 30.
               Describe the shape of the distribution. Is it symmetric or skewed?
               What does that tell you about typical home prices in Ames?"),
          div(class = "answer-label", "Your answer:"),
          div(class = "answer-input",
              textAreaInput("ans4", NULL, rows = 3, width = "100%",
                            placeholder = "Type your answer here...")),

          div(class = "rq-box",
              "RQ 5. Compare the mean and median Sale Price shown in the summary panel.
               Which is larger? Why might they differ? Which better represents a
               'typical' home price?"),
          div(class = "answer-label", "Your answer:"),
          div(class = "answer-input",
              textAreaInput("ans5", NULL, rows = 3, width = "100%",
                            placeholder = "Type your answer here...")),

          div(class = "rq-box",
              "RQ 6. Switch to 'Year Built'. Try different bin widths.
               In which decade were the most homes built?
               How does bin width change your interpretation?"),
          div(class = "answer-label", "Your answer:"),
          div(class = "answer-input",
              textAreaInput("ans6", NULL, rows = 3, width = "100%",
                            placeholder = "Type your answer here..."))
        )
      )
    ),

    # ── TAB 3: Boxplot ───────────────────────────────────────────────────────
    tabPanel("Boxplot",
      br(),
      fluidRow(
        column(4,
          wellPanel(
            h4("Numerical variable (y-axis)"),
            selectInput("boxNum", NULL, choices = num_vars, selected = "SalePrice"),
            h4("Group by (x-axis)"),
            selectInput("boxCat", NULL, choices = cat_vars, selected = "Bldg.Type"),
            checkboxInput("boxPoints", "Show individual data points", value = FALSE),
            checkboxInput("boxMean",   "Show mean (×)", value = TRUE)
          ),
          br(),
          wellPanel(
            h4("Five-number summary"),
            uiOutput("boxStats")
          )
        ),
        column(8,
          plotOutput("boxPlot", height = "400px"),
          br(),
          h4("Research Questions"),

          div(class = "rq-box",
              "RQ 7. Group Sale Price by Building Type. Which type has the highest
               median price? Which has the most variability (widest box)?"),
          div(class = "answer-label", "Your answer:"),
          div(class = "answer-input",
              textAreaInput("ans7", NULL, rows = 3, width = "100%",
                            placeholder = "Type your answer here...")),

          div(class = "rq-box",
              "RQ 8. Notice the difference between the mean (×) and median
               (center line) in each group. In which group is the difference largest?
               What does that tell you about outliers in that group?"),
          div(class = "answer-label", "Your answer:"),
          div(class = "answer-input",
              textAreaInput("ans8", NULL, rows = 3, width = "100%",
                            placeholder = "Type your answer here...")),

          div(class = "rq-box",
              "RQ 9. Switch grouping to Neighborhood. Which neighborhood has the
               highest median sale price? Which has the most outliers?"),
          div(class = "answer-label", "Your answer:"),
          div(class = "answer-input",
              textAreaInput("ans9", NULL, rows = 3, width = "100%",
                            placeholder = "Type your answer here..."))
        )
      )
    ),

    # ── TAB 4: Scatterplot ───────────────────────────────────────────────────
    tabPanel("Scatterplot",
      br(),
      fluidRow(
        column(4,
          wellPanel(
            h4("X-axis variable"),
            selectInput("scatX", NULL, choices = num_vars, selected = "Gr.Liv.Area"),
            h4("Y-axis variable"),
            selectInput("scatY", NULL, choices = num_vars, selected = "SalePrice"),
            h4("Color points by"),
            selectInput("scatCol", NULL,
                        choices = c("None" = "none", cat_vars),
                        selected = "none"),
            checkboxInput("scatLine", "Add trend line", value = TRUE)
          ),
          br(),
          wellPanel(
            h4("Correlation"),
            uiOutput("scatCorr")
          )
        ),
        column(8,
          plotOutput("scatPlot", height = "400px"),
          br(),
          h4("Research Questions"),

          div(class = "rq-box",
              "RQ 10. Plot Sale Price (y) vs. Above-Ground Living Area (x).
               Describe the relationship. Is it positive or negative?
               Strong or weak? Are there any unusual points?"),
          div(class = "answer-label", "Your answer:"),
          div(class = "answer-input",
              textAreaInput("ans10", NULL, rows = 3, width = "100%",
                            placeholder = "Type your answer here...")),

          div(class = "rq-box",
              "RQ 11. Color the points by Building Type. Do different building
               types cluster in different regions of the plot?
               What does this suggest about how building type relates to
               both size and price?"),
          div(class = "answer-label", "Your answer:"),
          div(class = "answer-input",
              textAreaInput("ans11", NULL, rows = 3, width = "100%",
                            placeholder = "Type your answer here...")),

          div(class = "rq-box",
              "RQ 12. Try Overall Quality on the x-axis with Sale Price on y.
               Is quality or living area a stronger predictor of price?
               Use the correlation value and the trend line to support your answer."),
          div(class = "answer-label", "Your answer:"),
          div(class = "answer-input",
              textAreaInput("ans12", NULL, rows = 3, width = "100%",
                            placeholder = "Type your answer here..."))
        )
      )
    ),

    # ── TAB 5: Variable Type Classifier ──────────────────────────────────────
    tabPanel("Variable Types",
      br(),
      fluidRow(
        column(12,
          h2("Classify the Variables"),
          p("From your readings, variables are classified as numerical (continuous or discrete)
            or categorical (nominal, ordinal, or binary).
            Use the controls below to practice classifying key variables in this dataset."),
          br()
        )
      ),
      fluidRow(
        column(6,
          h4("Select a variable to explore"),
          selectInput("typeVar", NULL,
                      choices = c(names(num_vars), names(cat_vars))),
          br(),
          wellPanel(
            h4("Variable summary"),
            uiOutput("typeSummary")
          )
        ),
        column(6,
          h4("Your classification"),
          radioButtons("typeClass", "Variable type:",
                       choices = c(
                         "Continuous numerical",
                         "Discrete numerical",
                         "Nominal categorical",
                         "Ordinal categorical",
                         "Binary categorical"
                       )),
          actionButton("typeCheck", "Check my answer", class = "btn-primary"),
          br(), br(),
          uiOutput("typeFeedback"),
          br(),
          div(class = "rq-box",
              "RQ 13. Classify all 8 numeric variables listed in the dropdown.
               For each one, write a one-sentence data dictionary entry that includes
               the variable name, type, units (if applicable), and expected range."),
          div(class = "answer-label", "Your answer:"),
          div(class = "answer-input",
              textAreaInput("ans13", NULL, rows = 9, width = "100%",
                            placeholder = "Type your answer here..."))
        )
      )
    ),

    # ── TAB 6: Submit & Download ─────────────────────────────────────────────
    tabPanel("Submit & Download",
      br(),
      fluidRow(
        column(8, offset = 2,
          h2("Download Your Report"),
          p("When you have completed all research questions, enter your name below and",
            "click", strong("Download PDF Report."), "Your responses to all 13 questions",
            "will be compiled into a PDF along with the charts from each section."),
          p(class = "download-note",
            "Instructor solutions to each research question will be revealed in the",
            "downloaded PDF — they are not visible in the app."),
          br(),
          wellPanel(
            h4("Student Information"),
            textInput("studentName", "Full Name", placeholder = "Enter your full name"),
            br(),
            downloadButton("downloadReport", "Download PDF Report",
                           class = "btn-primary btn-lg")
          ),
          br(),
          h4("Response Preview"),
          p(style = "font-size:13px; color:#777;",
            "Review your answers below before downloading."),
          uiOutput("answerPreview")
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  correct_types <- list(
    "Sale Price ($)"                   = "Continuous numerical",
    "Above-Ground Living Area (sq ft)" = "Continuous numerical",
    "Overall Quality (1–10)"      = "Ordinal categorical",
    "Year Built"                       = "Discrete numerical",
    "Bedrooms Above Grade"             = "Discrete numerical",
    "Full Bathrooms"                   = "Discrete numerical",
    "Total Basement Area (sq ft)"      = "Continuous numerical",
    "Garage Area (sq ft)"              = "Continuous numerical",
    "Building Type"                    = "Nominal categorical",
    "Neighborhood"                     = "Nominal categorical",
    "Central Air"                      = "Binary categorical"
  )

  # ── Tab 1 ─────────────────────────────────────────────────────────────────
  output$previewTable <- renderTable({
    req(input$previewVars)
    cols <- intersect(input$previewVars, names(housing))
    if (length(cols) == 0) return(NULL)
    head(housing[, cols, drop = FALSE], 10)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ── Tab 2: Histogram ──────────────────────────────────────────────────────
  hist_data <- reactive({
    req(input$histBldg)
    housing %>% filter(Bldg.Type %in% input$histBldg)
  })

  hist_plot_obj <- reactive({
    d    <- hist_data()
    col  <- input$histVar
    lbl  <- names(num_vars)[num_vars == col]
    vals <- d[[col]]
    req(length(vals) > 0)

    ggplot(d, aes(x = .data[[col]])) +
      geom_histogram(bins = input$histBins,
                     fill = "#2c6fad", color = "white", alpha = 0.85) +
      geom_vline(aes(xintercept = mean(vals, na.rm = TRUE),  color = "Mean"),
                 linewidth = 1, linetype = "dashed") +
      geom_vline(aes(xintercept = median(vals, na.rm = TRUE), color = "Median"),
                 linewidth = 1, linetype = "solid") +
      scale_color_manual(name = "", values = c("Mean" = "#e05c2a", "Median" = "#2ca05a")) +
      scale_x_continuous(labels = scales::comma) +
      labs(x = lbl, y = "Number of homes",
           caption = paste("n =", scales::comma(sum(!is.na(vals))), "homes")) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  output$histPlot <- renderPlot({ hist_plot_obj() })

  output$histStats <- renderUI({
    d    <- hist_data()
    col  <- input$histVar
    vals <- d[[col]]
    req(length(vals) > 0)
    tagList(
      div(class = "stat-box",
          div(class = "stat-label", "Mean"),
          div(class = "stat-value", scales::comma(round(mean(vals, na.rm = TRUE), 1)))),
      div(class = "stat-box",
          div(class = "stat-label", "Median"),
          div(class = "stat-value", scales::comma(round(median(vals, na.rm = TRUE), 1)))),
      div(class = "stat-box",
          div(class = "stat-label", "Std deviation"),
          div(class = "stat-value", scales::comma(round(sd(vals, na.rm = TRUE), 1)))),
      div(class = "stat-box",
          div(class = "stat-label", "Min / Max"),
          div(class = "stat-value",
              paste0(scales::comma(min(vals, na.rm = TRUE)),
                     " / ",
                     scales::comma(max(vals, na.rm = TRUE)))))
    )
  })

  # ── Tab 3: Boxplot ────────────────────────────────────────────────────────
  box_plot_obj <- reactive({
    col   <- input$boxNum
    grp   <- input$boxCat
    lbl_y <- names(num_vars)[num_vars == col]
    lbl_x <- names(cat_vars)[cat_vars == grp]

    p <- ggplot(housing, aes(x = reorder(.data[[grp]], .data[[col]], median, na.rm = TRUE),
                             y = .data[[col]])) +
      geom_boxplot(fill = "#2c6fad", alpha = 0.7,
                   outlier.color = "#e05c2a", outlier.size = 1.5) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = lbl_x, y = lbl_y,
           caption = paste("Groups ordered by median", lbl_y)) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))

    if (input$boxPoints)
      p <- p + geom_jitter(width = 0.15, alpha = 0.15, size = 0.8, color = "#555")

    if (input$boxMean) {
      means <- housing %>%
        group_by(.data[[grp]]) %>%
        summarise(m = mean(.data[[col]], na.rm = TRUE))
      p <- p + geom_point(data = means, aes(x = .data[[grp]], y = m),
                          shape = 4, size = 3.5, color = "#e05c2a", stroke = 1.5)
    }
    p
  })

  output$boxPlot <- renderPlot({ box_plot_obj() })

  output$boxStats <- renderUI({
    col  <- input$boxNum
    vals <- housing[[col]]
    qs   <- quantile(vals, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)
    fmt  <- function(x) scales::comma(round(x, 0))
    tagList(
      div(class = "stat-box",
          div(class = "stat-label", "Minimum"),
          div(class = "stat-value", fmt(qs[1]))),
      div(class = "stat-box",
          div(class = "stat-label", "Q1 (25th percentile)"),
          div(class = "stat-value", fmt(qs[2]))),
      div(class = "stat-box",
          div(class = "stat-label", "Median (Q2)"),
          div(class = "stat-value", fmt(qs[3]))),
      div(class = "stat-box",
          div(class = "stat-label", "Q3 (75th percentile)"),
          div(class = "stat-value", fmt(qs[4]))),
      div(class = "stat-box",
          div(class = "stat-label", "Maximum"),
          div(class = "stat-value", fmt(qs[5]))),
      div(class = "stat-box",
          div(class = "stat-label", "IQR"),
          div(class = "stat-value", fmt(qs[4] - qs[2])))
    )
  })

  # ── Tab 4: Scatterplot ────────────────────────────────────────────────────
  scat_plot_obj <- reactive({
    x_col <- input$scatX
    y_col <- input$scatY
    lbl_x <- names(num_vars)[num_vars == x_col]
    lbl_y <- names(num_vars)[num_vars == y_col]

    p <- ggplot(housing, aes(x = .data[[x_col]], y = .data[[y_col]]))

    if (input$scatCol != "none") {
      p <- p + geom_point(aes(color = .data[[input$scatCol]]), alpha = 0.5, size = 1.2) +
        labs(color = names(cat_vars)[cat_vars == input$scatCol])
    } else {
      p <- p + geom_point(color = "#2c6fad", alpha = 0.4, size = 1.2)
    }

    if (input$scatLine)
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "#e05c2a", linewidth = 1.1)

    p +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = lbl_x, y = lbl_y,
           caption = paste("n =", scales::comma(nrow(housing)), "homes")) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  output$scatPlot <- renderPlot({ scat_plot_obj() })

  output$scatCorr <- renderUI({
    x_vals <- housing[[input$scatX]]
    y_vals <- housing[[input$scatY]]
    r      <- cor(x_vals, y_vals, use = "complete.obs")
    strength  <- dplyr::case_when(abs(r) >= 0.7 ~ "strong",
                                  abs(r) >= 0.4 ~ "moderate",
                                  TRUE ~ "weak")
    direction <- ifelse(r >= 0, "positive", "negative")
    tagList(
      div(class = "stat-box",
          div(class = "stat-label", "Pearson r"),
          div(class = "stat-value", round(r, 3))),
      p(style = "font-size:13px; color:#555; margin-top:6px;",
        paste("This is a", strength, direction, "linear relationship."))
    )
  })

  # ── Tab 5: Variable types ─────────────────────────────────────────────────
  output$typeSummary <- renderUI({
    v_name <- input$typeVar
    col    <- c(num_vars, cat_vars)[v_name]
    vals   <- housing[[col]]
    if (is.numeric(vals)) {
      tagList(
        p(strong("Type detected by R: "), "numeric"),
        p(strong("Unique values: "), length(unique(vals))),
        p(strong("Range: "),
          paste(scales::comma(min(vals, na.rm = TRUE)),
                "to",
                scales::comma(max(vals, na.rm = TRUE))))
      )
    } else {
      lvls <- sort(unique(vals))
      tagList(
        p(strong("Type detected by R: "), "character"),
        p(strong("Unique categories: "), length(lvls)),
        p(strong("Values: "), paste(head(lvls, 8), collapse = ", "),
          if (length(lvls) > 8) "..." else "")
      )
    }
  })

  output$typeFeedback <- renderUI({
    req(input$typeCheck)
    isolate({
      v_name   <- input$typeVar
      selected <- input$typeClass
      correct  <- correct_types[[v_name]]
      if (is.null(correct)) return(NULL)
      if (selected == correct) {
        div(style = "background:#eafaf1; border-left:4px solid #2ca05a;
                     padding:10px 14px; border-radius:0 6px 6px 0;",
            tags$b("Correct! "), correct)
      } else {
        div(style = "background:#fdf3ee; border-left:4px solid #e05c2a;
                     padding:10px 14px; border-radius:0 6px 6px 0;",
            tags$b("Not quite. "),
            paste("The correct classification is:", correct))
      }
    })
  })

  # ── Tab 6: Answer preview ─────────────────────────────────────────────────
  rq_labels <- c(
    "RQ 1.  How many observations and variables?",
    "RQ 2.  Identify a numerical and a categorical variable.",
    "RQ 3.  What does Sale Price tell us?",
    "RQ 4.  Describe the shape of the Sale Price distribution.",
    "RQ 5.  Compare mean and median Sale Price.",
    "RQ 6.  Year Built — effect of bin width.",
    "RQ 7.  Sale Price by Building Type: median and variability.",
    "RQ 8.  Mean vs. median difference across boxplot groups.",
    "RQ 9.  Sale Price by Neighborhood.",
    "RQ 10. Sale Price vs. Living Area relationship.",
    "RQ 11. Building type clusters in the scatterplot.",
    "RQ 12. Quality vs. living area as predictors.",
    "RQ 13. Variable classification and data dictionary."
  )

  output$answerPreview <- renderUI({
    items <- lapply(seq_along(rq_labels), function(i) {
      ans <- input[[paste0("ans", i)]]
      if (is.null(ans) || nchar(trimws(ans)) == 0) ans <- "(no answer provided)"
      tagList(
        div(class = "preview-label", rq_labels[i]),
        div(class = "preview-answer", ans)
      )
    })
    do.call(tagList, items)
  })

  # ── Download handler ──────────────────────────────────────────────────────
  output$downloadReport <- downloadHandler(
    filename = function() {
      nm <- gsub("[^A-Za-z0-9]", "_", trimws(input$studentName))
      if (nchar(nm) == 0) nm <- "student"
      paste0("MA153X_LSN2_", nm, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Save current plot state to temp PNGs
      hist_png <- tempfile(fileext = ".png")
      box_png  <- tempfile(fileext = ".png")
      scat_png <- tempfile(fileext = ".png")

      ggplot2::ggsave(hist_png, plot = hist_plot_obj(), width = 8, height = 4,   dpi = 150)
      ggplot2::ggsave(box_png,  plot = box_plot_obj(),  width = 8, height = 4.5, dpi = 150)
      ggplot2::ggsave(scat_png, plot = scat_plot_obj(), width = 8, height = 4.5, dpi = 150)

      # Collect answers
      ans_list <- setNames(
        lapply(paste0("ans", 1:13), function(id) {
          v <- input[[id]]
          if (is.null(v) || nchar(trimws(v)) == 0) "(No answer provided.)" else v
        }),
        paste0("ans", 1:13)
      )

      params <- c(
        list(
          student_name = if (nchar(trimws(input$studentName)) > 0)
                           input$studentName else "Unknown Student",
          hist_img = hist_png,
          box_img  = box_png,
          scat_img = scat_png
        ),
        ans_list
      )

      # Copy template to tempdir so auxiliary files don't land in the app folder
      tmp_rmd <- file.path(tempdir(), "report.Rmd")
      file.copy("report_template.Rmd", tmp_rmd, overwrite = TRUE)

      rmarkdown::render(
        tmp_rmd,
        output_file = file,
        params      = params,
        envir       = new.env(parent = globalenv()),
        quiet       = TRUE
      )
    }
  )
}

shinyApp(ui, server)

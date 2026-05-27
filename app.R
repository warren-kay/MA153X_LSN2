library(shiny)
library(ggplot2)
library(dplyr)

# ── Data ──────────────────────────────────────────────────────────────────────
housing <- read.csv("EDA_AmesHousing.csv", stringsAsFactors = FALSE)

# Friendly display names for numeric variables
num_vars <- c(
  "Sale Price ($)"         = "SalePrice",
  "Above-Ground Living Area (sq ft)" = "Gr.Liv.Area",
  "Overall Quality (1–10)" = "Overall.Qual",
  "Year Built"             = "Year.Built",
  "Bedrooms Above Grade"   = "Bedroom.AbvGr",
  "Full Bathrooms"         = "Full.Bath",
  "Total Basement Area (sq ft)" = "Total.Bsmt.SF",
  "Garage Area (sq ft)"    = "Garage.Area"
)

cat_vars <- c(
  "Building Type"  = "Bldg.Type",
  "Neighborhood"   = "Neighborhood",
  "Central Air"    = "Central.Air"
)

# Short code-glossary used in Tab 5 captions
bldg_glossary <- paste(
  "1Fam = single-family detached;",
  "2fmCon = two-family conversion;",
  "Duplex = duplex;",
  "Twnhs = townhouse interior unit;",
  "TwnhsE = townhouse end unit."
)

# Each RQ has a unique id; the download handler iterates over this list.
rq_text <- list(
  rq01 = "RQ 1. Look at the Dataset Dimensions panel on the right. What does one row represent in this dataset? Why might a dataset describing houses have so many variables per observation?",
  rq02 = "RQ 2. Look at the first 10 rows. Identify one numerical variable and one categorical variable. What distinguishes them — both in how their values look in the table, and in what kinds of summaries (mean, count, etc.) make sense for each?",
  rq03 = "RQ 3. Look at the Sale Price summary panel on this tab. What is the actual range of sale prices in this dataset? Are the minimum and maximum reasonable for residential homes? Why might extreme values exist?",
  rq04 = "RQ 4. Set the variable to Sale Price and bins to 30. Describe the shape of the distribution. Is it symmetric or skewed? What does that tell you about typical home prices in Ames?",
  rq05 = "RQ 5. Compare the mean and median Sale Price shown in the summary panel. Which is larger? Why might they differ? Which better represents a 'typical' home price?",
  rq06 = "RQ 6. Switch to 'Year Built'. Try different numbers of bins (the panel shows the implied bin width). In which decade were the most homes built? How does bin count change your interpretation?",
  rq07 = "RQ 7. Uncheck every building type except '1Fam' (single-family). How does the Sale Price distribution shape change versus the full dataset? What does that suggest about whether building type drives the overall shape?",
  rq08 = "RQ 8. Group Sale Price by Building Type. Which type has the highest median price? Which has the most variability (widest box)?",
  rq09 = "RQ 9. Notice the difference between the mean (×) and median (center line) in each group. In which group is the difference largest? What does that tell you about outliers in that group? (Outliers are points beyond 1.5·IQR from the box edges.)",
  rq10 = "RQ 10. Switch grouping to Neighborhood. Which neighborhood has the highest median sale price? Which has the most outliers?",
  rq11 = "RQ 11. Look at the n above each box. Some groups have far more homes than others. How does the sample size per group affect how much you should trust the median? Name one group whose median you'd treat with caution and why.",
  rq12 = "RQ 12. Plot Sale Price (y) vs. Above-Ground Living Area (x). Describe the relationship in your own words — direction, strength, any unusual points. Then click 'Show interpretation' to compare your reading with the app's automatic interpretation.",
  rq13 = "RQ 13. Color the points by Building Type. Do different building types cluster in different regions of the plot? What does this suggest about how building type relates to both size and price?",
  rq14 = "RQ 14. Look at the Correlation Table. Which numerical variable has the strongest correlation with Sale Price? Is that the same variable you'd have predicted from the scatterplot alone? What does this tell you about the value of comparing correlations rather than eyeballing one plot at a time?",
  rq15 = "RQ 15. Classify all 11 variables in the dropdown. For each one, write a one-sentence data dictionary entry: name, type, units (if applicable), and expected range. Use the example shown above the dropdown as your template.",
  rq16 = "RQ 16. Bridging question. Overall Quality is recorded as an integer 1–10. The app lets you plot it as a histogram on Tab 2 and compute a Pearson correlation with Sale Price on Tab 4 — yet here on Tab 5 you classified it as ordinal categorical. When is it OK to treat an ordinal variable as numerical? What information might you lose by doing so? (Hint: think about what '5' minus '4' actually means in quality scores.)",
  rq17 = "RQ 17. Synthesis. Write one paragraph (~5–7 sentences) summarizing what you learned about the Ames housing market from this lesson. Address: (a) typical home characteristics, (b) what appears to drive Sale Price, and (c) one finding that surprised you. Cite at least two specific numbers from the explorer (e.g., median price, strongest correlation)."
)

# Helper: render one RQ block (prompt box + small text area for the answer)
rq_block <- function(id) {
  tagList(
    div(class = "rq-box", rq_text[[id]]),
    textAreaInput(paste0(id, "_ans"), label = NULL, placeholder = "Your answer…",
                  width = "100%", rows = 3, resize = "vertical")
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 14px; }
    h2 { font-size: 20px; font-weight: 600; margin-top: 0; }
    h4 { font-size: 15px; font-weight: 600; color: #333; margin-top: 18px; margin-bottom: 4px; }
    .well { background: #f7f7f7; border: 1px solid #e0e0e0; border-radius: 6px; padding: 14px; }
    .rq-box { background: #eef4fb; border-left: 4px solid #2c6fad; padding: 10px 14px;
               border-radius: 0 6px 6px 0; margin-bottom: 6px; font-size: 13px; }
    .stat-box { background: #fff; border: 1px solid #ddd; border-radius: 6px;
                padding: 10px 14px; margin-bottom: 8px; }
    .stat-label { font-size: 12px; color: #777; margin-bottom: 2px; }
    .stat-value { font-size: 22px; font-weight: 600; color: #2c6fad; }
    .nav-tabs > li > a { font-size: 13px; }
    .note-box { background: #fffbe6; border-left: 4px solid #d4a017; padding: 8px 12px;
                border-radius: 0 6px 6px 0; font-size: 12px; color: #5a4500; margin-bottom: 10px; }
    .form-group { margin-bottom: 14px; }
    textarea.form-control { font-size: 13px; }
  "))),

  titlePanel("Ames Housing Data — Descriptive Analytics Explorer"),

  tabsetPanel(
    id = "mainTabs",

    # ── TAB 1: Dataset Overview ──────────────────────────────────────────────
    tabPanel("Dataset Overview",
             br(),
             fluidRow(
               column(8,
                      h2("What is this dataset?"),
                      p("This dataset contains records of residential home sales in",
                        strong("Ames, Iowa from 2006–2010."),
                        "Each row is one house sale, and each column records something about that property —",
                        "its size, age, location, condition, and sale price."),
                      p("Before we can analyze data, we need to understand its structure.",
                        "Use the controls on the right to explore the dataset, then answer the research questions below."),
                      br(),
                      h4("Research Questions"),
                      rq_block("rq01"),
                      rq_block("rq02"),
                      rq_block("rq03"),
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
                            div(class = "stat-value", nrow(housing))
                        ),
                        div(class = "stat-box",
                            div(class = "stat-label", "Columns (variables)"),
                            div(class = "stat-value", ncol(housing))
                        )
                      ),
                      br(),
                      wellPanel(
                        h4("Sale Price summary"),
                        uiOutput("salePriceSummary")
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
                        selectInput("histVar", label = NULL, choices = num_vars,
                                    selected = "SalePrice"),
                        h4("Number of bins"),
                        sliderInput("histBins", label = NULL, min = 5, max = 60,
                                    value = 30, step = 1),
                        div(class = "note-box",
                            "Implied bin width: ", strong(textOutput("histBinWidth", inline = TRUE))),
                        h4("Filter by building type"),
                        checkboxGroupInput("histBldg", label = NULL,
                                           choices  = sort(unique(housing$Bldg.Type)),
                                           selected = sort(unique(housing$Bldg.Type)))
                      ),
                      br(),
                      wellPanel(
                        h4("Summary statistics"),
                        uiOutput("histStats")
                      ),
                      br(),
                      div(class = "note-box",
                          strong("Skew vocabulary:"), br(),
                          "• Symmetric → mean ≈ median.", br(),
                          "• Right-skewed → mean > median (long right tail of high values).", br(),
                          "• Left-skewed → mean < median (long left tail of low values)."
                      )
               ),
               column(8,
                      plotOutput("histPlot", height = "380px"),
                      br(),
                      h4("Research Questions"),
                      rq_block("rq04"),
                      rq_block("rq05"),
                      rq_block("rq06"),
                      rq_block("rq07")
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
                        selectInput("boxNum", label = NULL, choices = num_vars,
                                    selected = "SalePrice"),
                        h4("Group by (x-axis)"),
                        selectInput("boxCat", label = NULL, choices = cat_vars,
                                    selected = "Bldg.Type"),
                        checkboxInput("boxPoints", "Show individual data points", value = FALSE),
                        checkboxInput("boxMean",   "Show mean (×)", value = TRUE)
                      ),
                      br(),
                      wellPanel(
                        h4("Five-number summary"),
                        uiOutput("boxStats")
                      ),
                      br(),
                      div(class = "note-box",
                          strong("Outlier rule:"), " Orange dots are values beyond Q1 − 1.5·IQR or Q3 + 1.5·IQR.",
                          " The number above each box is the group's sample size (n)."
                      )
               ),
               column(8,
                      plotOutput("boxPlot", height = "440px"),
                      br(),
                      h4("Research Questions"),
                      rq_block("rq08"),
                      rq_block("rq09"),
                      rq_block("rq10"),
                      rq_block("rq11")
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
                        selectInput("scatX", label = NULL, choices = num_vars,
                                    selected = "Gr.Liv.Area"),
                        h4("Y-axis variable"),
                        selectInput("scatY", label = NULL, choices = num_vars,
                                    selected = "SalePrice"),
                        h4("Color points by"),
                        selectInput("scatCol", label = NULL,
                                    choices = c("None" = "none", cat_vars),
                                    selected = "none"),
                        checkboxInput("scatLine", "Add trend line", value = TRUE)
                      ),
                      br(),
                      wellPanel(
                        h4("Correlation"),
                        uiOutput("scatCorr"),
                        actionButton("revealCorr", "Show interpretation",
                                     class = "btn-default btn-sm",
                                     style = "margin-top:8px;")
                      ),
                      br(),
                      wellPanel(
                        h4("Correlation table — every numerical variable vs Sale Price"),
                        tableOutput("corrTable")
                      )
               ),
               column(8,
                      plotOutput("scatPlot", height = "400px"),
                      br(),
                      h4("Research Questions"),
                      rq_block("rq12"),
                      rq_block("rq13"),
                      rq_block("rq14")
               )
             )
    ),

    # ── TAB 5: Variable Type Classifier ──────────────────────────────────────
    tabPanel("Variable Types",
             br(),
             fluidRow(
               column(12,
                      h2("Classify the Variables"),
                      p("From your readings, variables are classified as numerical (continuous or discrete)",
                        "or categorical (nominal, ordinal, or binary).",
                        "Use the table below to practice classifying key variables in this dataset."),
                      div(class = "note-box",
                          strong("Data-dictionary entry — example template:"), br(),
                          "SalePrice — continuous numerical, US dollars,",
                          " expected range roughly $13,000 to $760,000",
                          " (the actual sale price recorded for each home in Ames, 2006–2010)."
                      ),
                      div(class = "note-box",
                          strong("Building Type codes used in this dataset:"), br(),
                          bldg_glossary, br(),
                          "Neighborhood values are 3–6 letter abbreviations for Ames subdivisions",
                          " (e.g., NAmes = North Ames, CollgCr = College Creek)."
                      ),
                      br()
               )
             ),
             fluidRow(
               column(6,
                      h4("Select a variable to explore"),
                      selectInput("typeVar", label = NULL,
                                  choices = c(names(num_vars), names(cat_vars))),
                      br(),
                      wellPanel(
                        h4("Variable summary"),
                        uiOutput("typeSummary")
                      )
               ),
               column(6,
                      h4("Your classification"),
                      radioButtons("typeClass", label = "Variable type:",
                                   choices = c(
                                     "Continuous numerical",
                                     "Discrete numerical",
                                     "Nominal categorical",
                                     "Ordinal categorical",
                                     "Binary categorical"
                                   )
                      ),
                      actionButton("typeCheck", "Check my answer", class = "btn-primary"),
                      br(), br(),
                      uiOutput("typeFeedback"),
                      br(),
                      rq_block("rq15"),
                      rq_block("rq16")
               )
             )
    ),

    # ── TAB 6: Synthesis & Submission ─────────────────────────────────────────
    tabPanel("Synthesis",
             br(),
             fluidRow(
               column(8, offset = 2,
                      h2("Putting it together"),
                      p("You've explored the Ames housing data through five lenses: structure,",
                        "distributions, group comparisons, bivariate relationships, and variable typing.",
                        "Pull those findings together into a short narrative."),
                      br(),
                      div(class = "rq-box", rq_text[["rq17"]]),
                      textAreaInput("rq17_ans", label = NULL,
                                    placeholder = "Write your one-paragraph synthesis here…",
                                    width = "100%", rows = 10, resize = "vertical"),
                      br(),
                      h4("Submit your responses"),
                      p("When you have finished answering the questions across every tab,",
                        "click below to download a text file containing every research question and",
                        "the answer you typed. Save the file and submit it according to your",
                        "instructor's directions."),
                      textInput("studentName", "Your name (optional, appears at top of file):",
                                width = "100%", placeholder = "Last, First"),
                      downloadButton("downloadAnswers", "Download my responses",
                                     class = "btn-primary")
               )
             )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # correct answers + brief explanations for the variable classifier
  correct_types <- list(
    "Sale Price ($)" = list(
      type = "Continuous numerical",
      reason = "Sale prices are measured on a continuous monetary scale — any non-negative dollar amount is in principle possible. We treat it as continuous because differences are meaningful and the scale has a true zero."
    ),
    "Above-Ground Living Area (sq ft)" = list(
      type = "Continuous numerical",
      reason = "Area is measured on a continuous scale (square feet). Any non-negative real value is possible; the precision is limited only by how carefully the measurement is taken."
    ),
    "Overall Quality (1–10)" = list(
      type = "Ordinal categorical",
      reason = "Quality scores have a meaningful order (10 is better than 5) but the gaps between adjacent scores aren't necessarily equal — the difference between a '4' and a '5' isn't the same kind of distance as between '$140k' and '$150k'."
    ),
    "Year Built" = list(
      type = "Discrete numerical",
      reason = "Years are integer counts of calendar years — countable, not measured on a continuous scale. You can't be built in 1972.4."
    ),
    "Bedrooms Above Grade" = list(
      type = "Discrete numerical",
      reason = "A count of rooms — must be a non-negative integer. Differences (3 − 2 = 1 bedroom) are meaningful."
    ),
    "Full Bathrooms" = list(
      type = "Discrete numerical",
      reason = "A count of bathrooms — integer-valued and countable. (Half-baths are tracked separately in another column.)"
    ),
    "Total Basement Area (sq ft)" = list(
      type = "Continuous numerical",
      reason = "Like above-ground area, basement area is a continuous measurement in square feet."
    ),
    "Garage Area (sq ft)" = list(
      type = "Continuous numerical",
      reason = "Continuous measurement in square feet. Note that homes without garages will have a value of 0."
    ),
    "Building Type" = list(
      type = "Nominal categorical",
      reason = "The categories (1Fam, Duplex, TwnhsE, …) have no inherent order — a townhouse isn't 'more' or 'less' than a duplex."
    ),
    "Neighborhood" = list(
      type = "Nominal categorical",
      reason = "Neighborhood labels are unordered categories. Even if some neighborhoods are more expensive than others, the labels themselves don't carry an order."
    ),
    "Central Air" = list(
      type = "Binary categorical",
      reason = "Only two possible values (Y, N). Could also be called a Boolean or nominal variable with two levels."
    )
  )

  # ── Tab 1: Dataset preview ────────────────────────────────────────────────
  output$previewTable <- renderTable({
    req(input$previewVars)
    cols <- intersect(input$previewVars, names(housing))
    if (length(cols) == 0) return(NULL)
    head(housing[, cols, drop = FALSE], 10)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$salePriceSummary <- renderUI({
    sp <- housing$SalePrice
    tagList(
      div(class = "stat-box",
          div(class = "stat-label", "Minimum"),
          div(class = "stat-value", paste0("$", scales::comma(min(sp, na.rm=TRUE))))),
      div(class = "stat-box",
          div(class = "stat-label", "Median"),
          div(class = "stat-value", paste0("$", scales::comma(median(sp, na.rm=TRUE))))),
      div(class = "stat-box",
          div(class = "stat-label", "Mean"),
          div(class = "stat-value", paste0("$", scales::comma(round(mean(sp, na.rm=TRUE)))))),
      div(class = "stat-box",
          div(class = "stat-label", "Maximum"),
          div(class = "stat-value", paste0("$", scales::comma(max(sp, na.rm=TRUE)))))
    )
  })

  # ── Tab 2: Histogram ──────────────────────────────────────────────────────
  hist_data <- reactive({
    req(input$histBldg)
    housing %>% filter(Bldg.Type %in% input$histBldg)
  })

  output$histBinWidth <- renderText({
    d    <- hist_data()
    vals <- d[[input$histVar]]
    req(length(vals) > 0, input$histBins)
    rng <- diff(range(vals, na.rm = TRUE))
    bw  <- rng / input$histBins
    # Round to a sensible number of significant figures for display
    if (bw >= 100) {
      scales::comma(round(bw))
    } else if (bw >= 1) {
      format(round(bw, 1), big.mark = ",")
    } else {
      signif(bw, 2)
    }
  })

  output$histPlot <- renderPlot({
    d    <- hist_data()
    col  <- input$histVar
    lbl  <- names(num_vars)[num_vars == col]
    vals <- d[[col]]
    req(length(vals) > 0)

    ggplot(d, aes(x = .data[[col]])) +
      geom_histogram(bins = input$histBins,
                     fill = "#2c6fad", color = "white", alpha = 0.85) +
      geom_vline(aes(xintercept = mean(vals, na.rm = TRUE),
                     color = "Mean"), linewidth = 1, linetype = "dashed") +
      geom_vline(aes(xintercept = median(vals, na.rm = TRUE),
                     color = "Median"), linewidth = 1, linetype = "solid") +
      scale_color_manual(name = "", values = c("Mean" = "#e05c2a", "Median" = "#2ca05a")) +
      scale_x_continuous(labels = scales::comma) +
      labs(x = lbl, y = "Number of homes",
           caption = paste("n =", scales::comma(sum(!is.na(vals))), "homes")) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  output$histStats <- renderUI({
    d    <- hist_data()
    col  <- input$histVar
    vals <- d[[col]]
    req(length(vals) > 0)

    tagList(
      div(class = "stat-box",
          div(class = "stat-label", "Mean"),
          div(class = "stat-value",
              scales::comma(round(mean(vals, na.rm = TRUE), 1)))),
      div(class = "stat-box",
          div(class = "stat-label", "Median"),
          div(class = "stat-value",
              scales::comma(round(median(vals, na.rm = TRUE), 1)))),
      div(class = "stat-box",
          div(class = "stat-label", "Std deviation"),
          div(class = "stat-value",
              scales::comma(round(sd(vals, na.rm = TRUE), 1)))),
      div(class = "stat-box",
          div(class = "stat-label", "Min / Max"),
          div(class = "stat-value",
              paste0(scales::comma(min(vals, na.rm = TRUE)),
                     " / ",
                     scales::comma(max(vals, na.rm = TRUE)))))
    )
  })

  # ── Tab 3: Boxplot ────────────────────────────────────────────────────────
  output$boxPlot <- renderPlot({
    col     <- input$boxNum
    grp     <- input$boxCat
    lbl_y   <- names(num_vars)[num_vars == col]
    lbl_x   <- names(cat_vars)[cat_vars == grp]

    # Pre-compute group order (by median of the y variable) so labels align.
    group_order <- housing %>%
      group_by(.data[[grp]]) %>%
      summarise(med = median(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
      arrange(med) %>%
      pull(.data[[grp]])

    d <- housing %>%
      mutate(.grp = factor(.data[[grp]], levels = group_order))

    # Group sample sizes for the n-labels above each box
    group_n <- d %>%
      group_by(.grp) %>%
      summarise(
        n      = sum(!is.na(.data[[col]])),
        top    = quantile(.data[[col]], 0.98, na.rm = TRUE),
        .groups = "drop"
      )

    y_top <- max(d[[col]], na.rm = TRUE)

    p <- ggplot(d, aes(x = .grp, y = .data[[col]])) +
      geom_boxplot(fill = "#2c6fad", alpha = 0.7, outlier.color = "#e05c2a",
                   outlier.size = 1.5) +
      geom_text(data = group_n,
                aes(x = .grp, y = y_top * 1.04,
                    label = paste0("n=", scales::comma(n))),
                size = 3.2, color = "#444", inherit.aes = FALSE) +
      scale_y_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0.02, 0.10))) +
      labs(x = lbl_x, y = lbl_y,
           caption = paste("Groups ordered by median", lbl_y,
                           "· outliers (orange) are beyond 1.5·IQR")) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))

    if (input$boxPoints) {
      p <- p + geom_jitter(width = 0.15, alpha = 0.15, size = 0.8, color = "#555")
    }
    if (input$boxMean) {
      means <- d %>%
        group_by(.grp) %>%
        summarise(m = mean(.data[[col]], na.rm = TRUE), .groups = "drop")
      p <- p + geom_point(data = means,
                          aes(x = .grp, y = m),
                          shape = 4, size = 3.5, color = "#e05c2a", stroke = 1.5,
                          inherit.aes = FALSE)
    }
    p
  })

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
  output$scatPlot <- renderPlot({
    x_col  <- input$scatX
    y_col  <- input$scatY
    lbl_x  <- names(num_vars)[num_vars == x_col]
    lbl_y  <- names(num_vars)[num_vars == y_col]

    p <- ggplot(housing, aes(x = .data[[x_col]], y = .data[[y_col]]))

    if (input$scatCol != "none") {
      p <- p + geom_point(aes(color = .data[[input$scatCol]]),
                          alpha = 0.5, size = 1.2) +
        labs(color = names(cat_vars)[cat_vars == input$scatCol])
    } else {
      p <- p + geom_point(color = "#2c6fad", alpha = 0.4, size = 1.2)
    }

    if (input$scatLine) {
      p <- p + geom_smooth(method = "lm", se = TRUE,
                           color = "#e05c2a", linewidth = 1.1)
    }

    p +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = lbl_x, y = lbl_y,
           caption = paste("n =", scales::comma(nrow(housing)), "homes")) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  # Reveal/hide button for the auto-interpretation of Pearson r
  corr_interp_shown <- reactiveVal(FALSE)

  observeEvent(input$revealCorr, {
    corr_interp_shown(!corr_interp_shown())
    updateActionButton(session, "revealCorr",
                       label = if (corr_interp_shown()) "Hide interpretation"
                       else "Show interpretation")
  })

  # Reset the toggle whenever the student changes variables — they should
  # form their own interpretation of the new pair before peeking.
  observeEvent(c(input$scatX, input$scatY), {
    corr_interp_shown(FALSE)
    updateActionButton(session, "revealCorr", label = "Show interpretation")
  }, ignoreInit = TRUE)

  output$scatCorr <- renderUI({
    x_vals <- housing[[input$scatX]]
    y_vals <- housing[[input$scatY]]
    r      <- cor(x_vals, y_vals, use = "complete.obs")

    interp <- NULL
    if (corr_interp_shown()) {
      strength <- dplyr::case_when(
        abs(r) >= 0.7 ~ "strong",
        abs(r) >= 0.4 ~ "moderate",
        TRUE          ~ "weak"
      )
      direction <- ifelse(r >= 0, "positive", "negative")
      interp <- p(style = "font-size:13px; color:#555; margin-top:6px;",
                  "Auto-interpretation: this is a ", strong(strength),
                  " ", strong(direction), " linear relationship.")
    }

    tagList(
      div(class = "stat-box",
          div(class = "stat-label", "Pearson r"),
          div(class = "stat-value", round(r, 3))),
      interp
    )
  })

  output$corrTable <- renderTable({
    sp <- housing$SalePrice
    rows <- lapply(num_vars, function(col) {
      if (col == "SalePrice") return(NULL)
      data.frame(
        Variable = names(num_vars)[num_vars == col],
        `Pearson r vs Sale Price` = round(
          cor(housing[[col]], sp, use = "complete.obs"), 3
        ),
        check.names = FALSE
      )
    })
    out <- do.call(rbind, rows)
    out[order(-abs(out$`Pearson r vs Sale Price`)), ]
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

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
          paste(scales::comma(min(vals, na.rm=TRUE)),
                "to",
                scales::comma(max(vals, na.rm=TRUE))))
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
      entry    <- correct_types[[v_name]]
      if (is.null(entry)) return(NULL)

      correct <- entry$type
      reason  <- entry$reason

      if (selected == correct) {
        div(style = "background:#eafaf1; border-left:4px solid #2ca05a;
                      padding:10px 14px; border-radius:0 6px 6px 0;",
            tags$b("Correct! "), correct, br(),
            tags$em("Why: "), reason)
      } else {
        div(style = "background:#fdf3ee; border-left:4px solid #e05c2a;
                      padding:10px 14px; border-radius:0 6px 6px 0;",
            tags$b("Not quite. "),
            "The correct classification is: ", tags$b(correct), br(),
            tags$em("Why: "), reason)
      }
    })
  })

  # ── Tab 6: Download all answers ───────────────────────────────────────────
  output$downloadAnswers <- downloadHandler(
    filename = function() {
      sn <- gsub("[^A-Za-z0-9_-]+", "_",
                 trimws(if (is.null(input$studentName)) "" else input$studentName))
      stem <- if (nchar(sn) > 0) paste0("MA153X_LSN2_", sn) else "MA153X_LSN2_responses"
      paste0(stem, ".txt")
    },
    content = function(file) {
      lines <- character()
      add <- function(...) lines <<- c(lines, paste0(...))

      add("MA153X — Lesson 2 — Research Question Responses")
      add("================================================")
      if (!is.null(input$studentName) && nzchar(trimws(input$studentName))) {
        add("Student: ", input$studentName)
      }
      add("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      add("")

      for (id in names(rq_text)) {
        ans_id <- paste0(id, "_ans")
        ans <- input[[ans_id]]
        if (is.null(ans) || !nzchar(trimws(ans))) ans <- "(no answer)"
        add(rq_text[[id]])
        add("")
        add("Answer:")
        add(ans)
        add("")
        add("------------------------------------------------")
        add("")
      }
      writeLines(lines, file)
    }
  )
}

shinyApp(ui, server)

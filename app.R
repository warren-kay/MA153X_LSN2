library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

# ── Data ──────────────────────────────────────────────────────────────────────
housing <- read.csv("EDA_AmesHousing.csv", stringsAsFactors = FALSE)

num_vars <- c(
  "Sale Price ($)"                   = "SalePrice",
  "Above-Ground Living Area (sq ft)" = "Gr.Liv.Area",
  "Overall Quality (1–10)"           = "Overall.Qual",
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

# ── RQ helper ─────────────────────────────────────────────────────────────────
# Pairs a research-question box with its response textarea. Defined outside
# ui/server per project convention so both can reference it.
rq_block <- function(rq_id, rq_text_str) {
  tagList(
    div(class = "rq-box", rq_text_str),
    div(
      style = "margin: 0 0 16px 0;",
      textAreaInput(rq_id, label = NULL, value = "", rows = 3,
                    placeholder = "Type your response here...", width = "100%")
    )
  )
}

# ── Research question text (shared by UI and PDF export) ──────────────────────
rq_text <- list(
  tab1 = c(
    "RQ 1. How many observations and variables does this dataset have? What does one row represent?",
    "RQ 2. Look at the first few rows. Can you identify one numerical variable and one categorical variable?",
    "RQ 3. What does the Sale Price variable tell us? What are reasonable minimum and maximum values?"
  ),
  tab2 = c(
    "RQ 4. Set the variable to Sale Price and bins to 30. Describe the shape of the distribution. Is it symmetric or skewed? What does that tell you about typical home prices in Ames?",
    "RQ 5. Compare the mean and median Sale Price shown in the summary panel. Which is larger? Why might they differ? Which better represents a 'typical' home price?",
    "RQ 6. Switch to 'Year Built'. Try different bin widths. In which decade were the most homes built? How does bin width change your interpretation?"
  ),
  tab3 = c(
    "RQ 7. Group Sale Price by Building Type. Which type has the highest median price? Which has the most variability (widest box)?",
    "RQ 8. Notice the difference between the mean (×) and median (center line) in each group. In which group is the difference largest? What does that tell you about outliers in that group?",
    "RQ 9. Switch grouping to Neighborhood. Which neighborhood has the highest median sale price? Which has the most outliers?"
  ),
  tab4 = c(
    "RQ 10. Plot Sale Price (y) vs. Above-Ground Living Area (x). Describe the relationship. Is it positive or negative? Strong or weak? Are there any unusual points?",
    "RQ 11. Color the points by Building Type. Do different building types cluster in different regions of the plot? What does this suggest about how building type relates to both size and price?",
    "RQ 12. Try Overall Quality on the x-axis with Sale Price on y. Is quality or living area a stronger predictor of price? Use the correlation value and the trend line to support your answer."
  ),
  tab5 = c(
    "RQ 13. Classify all 8 numeric variables listed in the dropdown. For each one, write a one-sentence data dictionary entry that includes the variable name, type, units (if applicable), and expected range."
  )
)

# ── Sample answers — never rendered in the UI, PDF export only ────────────────
sample_answers <- list(
  tab1 = list(
    paste(
      "The dataset has 2,930 observations (rows) and 82 variables (columns).",
      "One row represents a single residential home sale — a complete record",
      "of all measured attributes for one property at the time it was sold."
    ),
    paste(
      "Numerical variables take quantitative values on which arithmetic is meaningful.",
      "Examples include Sale Price (in dollars) or Above-Ground Living Area (in sq ft).",
      "Categorical variables place homes into named groups with no inherent numerical ordering.",
      "Examples include Building Type (labels such as 1Fam or Duplex) and Neighborhood."
    ),
    paste(
      "Sale Price records the total amount paid by the buyer, in U.S. dollars.",
      "Given that this dataset covers Ames, Iowa from 2006–2010, reasonable values",
      "range from roughly $12,000 for distressed properties to about $760,000 for",
      "high-end homes. The vast majority of sales fall between $100,000 and $300,000."
    )
  ),
  tab2 = list(
    paste(
      "The Sale Price distribution is right-skewed (positively skewed): most homes are",
      "priced between $100,000 and $250,000, but a long tail extends toward higher values.",
      "This tells us that a typical home in Ames costs roughly $130,000–$180,000,",
      "but a minority of premium properties push prices well above that range."
    ),
    paste(
      "The mean Sale Price is larger than the median. They differ because the distribution",
      "is right-skewed — a small number of very expensive homes pull the arithmetic",
      "mean upward without affecting the median. The median is a better measure of the",
      "typical home price because it is resistant to extreme high-end values and represents",
      "the actual middle of the price distribution."
    ),
    paste(
      "With moderate bin widths, the histogram shows the 2000s (2000–2009) as the",
      "decade with the most homes built, followed by the 1990s. Very narrow bins reveal",
      "year-by-year variation but can look noisy, making trends harder to see. Very wide",
      "bins smooth over fluctuations and highlight broad decade-level patterns but hide",
      "finer detail. The right bin width depends on the question being asked."
    )
  ),
  tab3 = list(
    paste(
      "Single-family homes (1Fam) and Townhouse End Units (TwnhsE) tend to have the highest",
      "median sale prices. Two-family conversions (2fmCon) and Duplexes typically have the",
      "lowest medians. Building types with taller boxes (larger IQR) — such as 1Fam",
      "— show the most variability in price, reflecting the wide range of single-family",
      "home sizes and conditions in the market."
    ),
    paste(
      "Single-family homes (1Fam) typically show the largest gap between the mean",
      "(× marker) and median (center line), because this group contains the",
      "highest-priced outliers. When the mean is noticeably above the median, it signals",
      "that the group’s distribution is right-skewed — a few very expensive",
      "properties are pulling the group average above what most homes in that group",
      "actually sold for."
    ),
    paste(
      "Stone Brook (StoneBr) and Northridge Heights (NridgHt) consistently rank among",
      "the highest-median-price neighborhoods. Neighborhoods with the most outlier dots",
      "beyond the whiskers are typically larger, more diverse neighborhoods like North Ames",
      "(NAmes) or College Creek (CollgCr), where a wide mix of property types creates",
      "occasional extreme prices relative to the neighborhood median."
    )
  ),
  tab4 = list(
    paste(
      "There is a strong, positive linear relationship between above-ground living area and",
      "sale price (Pearson r ≈ 0.71): larger homes tend to command higher prices.",
      "The scatter increases at larger home sizes. A few unusual points appear — some",
      "very large homes sell at unexpectedly low prices (possibly due to poor condition),",
      "and there are a couple of extremely large properties that may be influential outliers."
    ),
    paste(
      "When colored by Building Type, the groups occupy distinct regions: single-family",
      "homes (1Fam) span the full range of sizes and prices; duplexes and two-family",
      "conversions cluster at smaller sizes and lower prices; townhouses occupy intermediate",
      "positions. This suggests building type acts as a confounder — part of the",
      "size–price relationship is driven by the type of property, since different",
      "building types are designed to different scales and market segments."
    ),
    paste(
      "Overall Quality (r ≈ 0.80) is a stronger predictor of sale price than",
      "Above-Ground Living Area (r ≈ 0.71). The scatterplot for quality vs. price",
      "shows a steep, consistent trend with less scatter around the line. This makes",
      "practical sense: buyers pay a premium for high-quality finishes and construction,",
      "and a well-built smaller home often commands a higher price than a larger but",
      "lower-quality property."
    )
  ),
  tab5 = list(
    paste(
      "Sale Price: Continuous numerical — total dollar amount paid; ranges from ~$12,789 to ~$755,000.",
      "Above-Ground Living Area: Continuous numerical — finished living space above grade in sq ft; ranges from 334 to 5,642 sq ft.",
      "Overall Quality (1–10): Ordinal categorical — rated scale from 1 (Very Poor) to 10 (Very Excellent); values are ordered but intervals are not guaranteed equal.",
      "Year Built: Discrete numerical — calendar year originally constructed; ranges from 1872 to 2010.",
      "Bedrooms Above Grade: Discrete numerical — count of bedrooms above basement level; ranges from 0 to 8.",
      "Full Bathrooms: Discrete numerical — count of full bathrooms; ranges from 0 to 4.",
      "Total Basement Area: Continuous numerical — total basement floor area in sq ft; ranges from 0 to 6,110 sq ft.",
      "Garage Area: Continuous numerical — floor area of the garage in sq ft; ranges from 0 to 1,418 sq ft.",
      sep = "\n"
    )
  )
)

# All 13 response-input IDs, in tab order — used for the completion counter
# and the PDF response-collection loop. Tab 5 has only one RQ, so a uniform
# N_TABS * N_RQS grid does not apply here.
ALL_RQ_IDS <- c(
  "rq_1_1", "rq_1_2", "rq_1_3",
  "rq_2_1", "rq_2_2", "rq_2_3",
  "rq_3_1", "rq_3_2", "rq_3_3",
  "rq_4_1", "rq_4_2", "rq_4_3",
  "rq_5_1"
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    # JavaScript handler: receives base64 PDF bytes from R and triggers a
    # browser download via a Blob (Shinylive has no server, so downloadHandler
    # will not work).
    tags$script(HTML("
      Shiny.addCustomMessageHandler('trigger_download', function(msg) {
        var binaryString = atob(msg.b64);
        var bytes = new Uint8Array(binaryString.length);
        for (var i = 0; i < binaryString.length; i++) {
          bytes[i] = binaryString.charCodeAt(i);
        }
        var blob = new Blob([bytes], { type: msg.mime });
        var url  = URL.createObjectURL(blob);
        var a    = document.createElement('a');
        a.href = url; a.download = msg.filename;
        document.body.appendChild(a); a.click();
        document.body.removeChild(a);
        setTimeout(function() { URL.revokeObjectURL(url); }, 2000);
      });
    ")),
    tags$style(HTML("
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
      textarea.form-control { border-radius: 4px; border: 1px solid #c8d8e8;
                               background: #f9fbfd; font-size: 13px; }
    "))
  ),

  titlePanel("Ames Housing Data — Descriptive Analytics Explorer"),

  tabsetPanel(

    # ── TAB 1: Dataset Overview ──────────────────────────────────────────────
    tabPanel("Dataset Overview",
      br(),
      fluidRow(
        column(8,
          h2("What is this dataset?"),
          p("This dataset contains information on residential home sales in",
            "Ames, Iowa from 2006–2010. Each variable describes some aspect",
            "of a property or the terms of its sale."),
          p("Before we can analyze data, we need to understand its structure.",
            "Use the controls on the right and the preview table below to",
            "explore the dataset before answering the research questions."),
          br(),
          h4("Research Questions"),

          rq_block("rq_1_1", rq_text$tab1[1]),
          rq_block("rq_1_2", rq_text$tab1[2]),
          rq_block("rq_1_3", rq_text$tab1[3]),

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
            div(style = "font-size:12px; color:#666; margin-top:-8px; margin-bottom:14px;",
                textOutput("histBinWidth", inline = TRUE)),
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

          rq_block("rq_2_1", rq_text$tab2[1]),
          rq_block("rq_2_2", rq_text$tab2[2]),
          rq_block("rq_2_3", rq_text$tab2[3])
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

          rq_block("rq_3_1", rq_text$tab3[1]),
          rq_block("rq_3_2", rq_text$tab3[2]),
          rq_block("rq_3_3", rq_text$tab3[3])
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

          rq_block("rq_4_1", rq_text$tab4[1]),
          rq_block("rq_4_2", rq_text$tab4[2]),
          rq_block("rq_4_3", rq_text$tab4[3])
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
          selectInput("typeVar", NULL, choices = c(names(num_vars), names(cat_vars))),
          br(),
          wellPanel(
            h4("Variable summary"),
            uiOutput("typeSummary")
          )
        ),
        column(6,
          h4("Your classification"),
          radioButtons("typeClass", "Variable type:",
                       choices = c("Continuous numerical", "Discrete numerical",
                                   "Nominal categorical", "Ordinal categorical",
                                   "Binary categorical")),
          actionButton("typeCheck", "Check my answer", class = "btn-primary"),
          br(), br(),
          uiOutput("typeFeedback"),
          br(),
          h4("Research Questions"),

          rq_block("rq_5_1", rq_text$tab5[1])
        )
      )
    ),

    # ── TAB 6: Export Responses ───────────────────────────────────────────────
    tabPanel("6. Export Responses",
      fluidPage(
        br(),
        h2("Export Your Responses to PDF"),
        p("Complete the research questions in Tabs 1–5, then click the button below."),
        wellPanel(
          fluidRow(
            column(5,
              h4("Your Information (optional)"),
              textInput("student_name", "Name:",    placeholder = "Last, First"),
              textInput("section",      "Section:", placeholder = "e.g., A1")
            ),
            column(4,
              h4("Progress"),
              uiOutput("completion_summary")
            ),
            column(3,
              h4("Download"),
              br(),
              actionButton("export_btn", "Generate & Download PDF",
                           class = "btn-primary",
                           style = "width:100%; font-size:14px; white-space:normal;"),
              br(), br(),
              uiOutput("export_status")
            )
          )
        ),
        div(style = "color:#555; font-size:13px; background:#fff8e1;
                     border-left:4px solid #f0ad4e; padding:10px 14px;
                     border-radius:0 6px 6px 0; margin-top:8px;",
          strong("Note:"), " Sample answers are not visible anywhere in this app.",
          " They appear only in the downloaded PDF, printed after your typed responses.")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  correct_types <- list(
    "Sale Price ($)"                   = "Continuous numerical",
    "Above-Ground Living Area (sq ft)" = "Continuous numerical",
    "Overall Quality (1–10)"           = "Ordinal categorical",
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
      geom_histogram(bins = input$histBins, fill = "#2c6fad", color = "white", alpha = 0.85) +
      geom_vline(aes(xintercept = mean(vals, na.rm = TRUE),   color = "Mean"),
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

  output$histBinWidth <- renderText({
    d    <- hist_data()
    col  <- input$histVar
    vals <- d[[col]]
    req(length(vals) > 1)
    bins  <- input$histBins
    rng   <- range(vals, na.rm = TRUE)
    width <- if (bins > 1) (rng[2] - rng[1]) / (bins - 1) else diff(rng)
    width_fmt <- if (width >= 100) scales::comma(round(width))
                 else if (width >= 1) scales::comma(round(width, 1))
                 else scales::comma(round(width, 3))
    paste0("Bin width ≈ ", width_fmt, " per bin")
  })

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
                     " / ", scales::comma(max(vals, na.rm = TRUE)))))
    )
  })

  # ── Tab 3: Boxplot ────────────────────────────────────────────────────────
  box_plot_obj <- reactive({
    col   <- input$boxNum
    grp   <- input$boxCat
    lbl_y <- names(num_vars)[num_vars == col]
    lbl_x <- names(cat_vars)[cat_vars == grp]
    p <- ggplot(housing,
                aes(x = reorder(.data[[grp]], .data[[col]], median, na.rm = TRUE),
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
      div(class = "stat-box", div(class = "stat-label", "Minimum"),
          div(class = "stat-value", fmt(qs[1]))),
      div(class = "stat-box", div(class = "stat-label", "Q1 (25th percentile)"),
          div(class = "stat-value", fmt(qs[2]))),
      div(class = "stat-box", div(class = "stat-label", "Median (Q2)"),
          div(class = "stat-value", fmt(qs[3]))),
      div(class = "stat-box", div(class = "stat-label", "Q3 (75th percentile)"),
          div(class = "stat-value", fmt(qs[4]))),
      div(class = "stat-box", div(class = "stat-label", "Maximum"),
          div(class = "stat-value", fmt(qs[5]))),
      div(class = "stat-box", div(class = "stat-label", "IQR"),
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
    r         <- cor(housing[[input$scatX]], housing[[input$scatY]], use = "complete.obs")
    strength  <- dplyr::case_when(abs(r) >= 0.7 ~ "strong",
                                  abs(r) >= 0.4 ~ "moderate", TRUE ~ "weak")
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
        p(strong("Range: "), paste(scales::comma(min(vals, na.rm = TRUE)),
                                   "to", scales::comma(max(vals, na.rm = TRUE))))
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
      correct <- correct_types[[input$typeVar]]
      if (is.null(correct)) return(NULL)
      guess <- input$typeClass

      if (guess == correct) {
        div(style = "background:#eafaf1; border-left:4px solid #2ca05a;
                     padding:10px 14px; border-radius:0 6px 6px 0;",
            tags$b("Correct! "), "That classification fits this variable well.")
      } else {
        numeric_types     <- c("Continuous numerical", "Discrete numerical")
        categorical_types <- c("Nominal categorical", "Ordinal categorical", "Binary categorical")
        guess_is_numeric   <- guess   %in% numeric_types
        correct_is_numeric <- correct %in% numeric_types

        hint <- if (guess_is_numeric != correct_is_numeric) {
          paste("Look again at the 'Type detected by R' and the values shown",
                "on the left. Does this variable represent a quantity you",
                "could do arithmetic with (like adding or averaging), or",
                "does it sort observations into named groups?")
        } else if (correct_is_numeric) {
          paste("You're on the right track — this is numerical. Now look",
                "more closely at the values: can this variable take on any",
                "value in its range, including fractions or decimals, or",
                "can it only take specific whole-number counts with gaps",
                "between the possible values?")
        } else {
          paste("You're on the right track — this is categorical. Now look",
                "more closely at the categories: how many distinct categories",
                "are there, and do those categories have a natural order or",
                "ranking, or are they just separate, unordered labels?")
        }

        div(style = "background:#fdf3ee; border-left:4px solid #e05c2a;
                     padding:10px 14px; border-radius:0 6px 6px 0;",
            tags$b("Not quite yet — think about this: "), hint)
      }
    })
  })

  # ── Tab 6: Export Responses ───────────────────────────────────────────────
  output$completion_summary <- renderUI({
    filled <- sum(sapply(ALL_RQ_IDS, function(id) {
      v <- input[[id]]; !is.null(v) && nchar(trimws(v)) > 0
    }))
    div(class = "stat-box",
      div(class = "stat-label", "Questions Answered"),
      div(class = "stat-value", paste0(filled, " / ", length(ALL_RQ_IDS)))
    )
  })

  export_msg <- reactiveVal("")
  output$export_status <- renderUI({
    msg <- export_msg()
    if (nchar(msg) == 0) return(NULL)
    div(style = "color:#2c6fad; font-size:13px; margin-top:4px;", msg)
  })

  observeEvent(input$export_btn, {
    export_msg("Building PDF...")

    # Drawing helpers
    write_block <- function(txt, x, y, width = 84, cex = 0.82,
                             col = "black", font = 1, lh = 0.043) {
      paragraphs <- strsplit(txt, "\n", fixed = TRUE)[[1]]
      if (length(paragraphs) == 0) paragraphs <- ""
      for (para in paragraphs) {
        lines <- strwrap(para, width = width)
        if (length(lines) == 0) lines <- ""
        for (ln in lines) {
          text(x, y, ln, adj = 0, cex = cex, col = col, font = font); y <- y - lh
        }
      }
      y - 0.008
    }
    new_page <- function() {
      plot.new()
      par(mar = c(0.3, 0.3, 0.3, 0.3))
      plot.window(xlim = c(0, 1), ylim = c(0, 1))
    }
    section_header <- function(title) {
      new_page()
      text(0.5, 0.975, "MA153X — Lesson 2: Descriptive Analytics Explorer",
           adj = 0.5, cex = 0.72, col = "gray50")
      text(0.5, 0.940, title, adj = 0.5, cex = 1.22, font = 2)
      0.895
    }
    qa_pages <- function(tab_title, qs, rs, ans) {
      y <- section_header(tab_title)
      for (i in seq_along(qs)) {
        if (y < 0.20) { y <- section_header(paste0(tab_title, " (cont.)")) }
        y <- write_block(qs[i], 0.02, y, cex = 0.84, col = "#1a3a6e", font = 2, lh = 0.043)
        resp <- if (nchar(trimws(rs[i])) == 0) "(No response entered.)" else rs[i]
        y <- write_block(paste0("Your response: ", resp),     0.04, y, cex = 0.79, col = "#222222", lh = 0.039)
        y <- write_block(paste0("Sample answer: ", ans[[i]]), 0.04, y, cex = 0.79, col = "#1a5c2a", lh = 0.039)
        y <- y - 0.022
      }
    }

    # Collect responses grouped by tab, in RQ order
    get_resp <- function(id) { v <- input[[id]]; if (is.null(v)) "" else v }
    resp_tab1 <- sapply(c("rq_1_1", "rq_1_2", "rq_1_3"), get_resp)
    resp_tab2 <- sapply(c("rq_2_1", "rq_2_2", "rq_2_3"), get_resp)
    resp_tab3 <- sapply(c("rq_3_1", "rq_3_2", "rq_3_3"), get_resp)
    resp_tab4 <- sapply(c("rq_4_1", "rq_4_2", "rq_4_3"), get_resp)
    resp_tab5 <- sapply(c("rq_5_1"),                     get_resp)

    tmp <- tempfile(fileext = ".pdf")
    tryCatch({
      pdf(tmp, width = 8.5, height = 11, title = "Lesson 2 Responses")

      # Cover page
      new_page()
      text(0.5, 0.72, "MA153X Data-Driven Modeling",  adj = 0.5, cex = 1.9, font = 2)
      text(0.5, 0.63, "Lesson 2: EDA I — Descriptive Analytics Explorer", adj = 0.5, cex = 1.4)
      text(0.5, 0.55, "Research Questions & Responses", adj = 0.5, cex = 1.15)
      nm <- trimws(input$student_name); sc <- trimws(input$section)
      if (nchar(nm) > 0) text(0.5, 0.44, paste("Student:", nm), adj = 0.5, cex = 1.0)
      if (nchar(sc) > 0) text(0.5, 0.37, paste("Section:", sc), adj = 0.5, cex = 1.0)
      text(0.5, 0.28, paste("Date:", format(Sys.Date(), "%d %B %Y")), adj = 0.5, cex = 1.0)

      # Tab 1: Dataset Overview — no plot
      qa_pages("Tab 1: Dataset Overview", rq_text$tab1, resp_tab1, sample_answers$tab1)

      # Tab 2: Histogram
      qa_pages("Tab 2: Histogram", rq_text$tab2, resp_tab2, sample_answers$tab2)
      print(hist_plot_obj())

      # Tab 3: Boxplot
      qa_pages("Tab 3: Boxplot", rq_text$tab3, resp_tab3, sample_answers$tab3)
      print(box_plot_obj())

      # Tab 4: Scatterplot
      qa_pages("Tab 4: Scatterplot", rq_text$tab4, resp_tab4, sample_answers$tab4)
      print(scat_plot_obj())

      # Tab 5: Variable Types — no plot
      qa_pages("Tab 5: Variable Types", rq_text$tab5, resp_tab5, sample_answers$tab5)

      dev.off()

      # Base64-encode and send to browser
      pdf_bytes <- readBin(tmp, what = "raw", n = file.info(tmp)$size)
      b64       <- gsub("\n", "", jsonlite::base64_enc(pdf_bytes))
      nm_clean  <- trimws(input$student_name)
      filename  <- if (nchar(nm_clean) > 0)
        paste0("LSN2_", gsub("[^A-Za-z0-9]", "_", nm_clean), ".pdf")
      else "LSN2_Responses.pdf"

      session$sendCustomMessage("trigger_download",
        list(b64 = b64, filename = filename, mime = "application/pdf"))
      export_msg("Done! Check your downloads folder.")

    }, error = function(e) {
      try(dev.off(), silent = TRUE)
      export_msg(paste("Error:", conditionMessage(e)))
    })
  })
}

shinyApp(ui, server)

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

# ── Helpers ───────────────────────────────────────────────────────────────────

# Render a ggplot object to a base64-encoded PNG data URI
plot_to_base64 <- function(plot_obj, width = 800, height = 450) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  png(tmp, width = width, height = height, res = 120)
  print(plot_obj)
  dev.off()
  raw_bytes <- readBin(tmp, "raw", n = file.info(tmp)$size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(raw_bytes))
}

# Escape user text for safe embedding in HTML
html_escape <- function(x) {
  if (is.null(x) || nchar(trimws(x)) == 0) return("<em style='color:#aaa'>No answer provided.</em>")
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub('"',  "&quot;", x, fixed = TRUE)
  x <- gsub("'",  "&#39;",  x, fixed = TRUE)
  x <- gsub("\n", "<br>",   x, fixed = TRUE)
  x
}

# Build one RQ block (question / student answer / solution)
rq_html <- function(num, question, answer, solution) {
  paste0(
    '<div class="rq-box"><strong>RQ ', num, '.</strong> ', question, '</div>',
    '<div class="label">Your answer</div>',
    '<div class="answer-box">', html_escape(answer), '</div>',
    '<div class="label">Instructor solution</div>',
    '<div class="solution-box">', solution, '</div>'
  )
}

# Assemble the full HTML report
build_report_html <- function(student_name, answers, hist_uri, box_uri, scat_uri, sols) {
  date_str <- format(Sys.Date(), "%B %d, %Y")
  nm_safe  <- html_escape(student_name)

  css <- paste0(
    "body{font-family:Arial,sans-serif;max-width:820px;margin:0 auto;padding:28px;",
    "line-height:1.55;font-size:14px}",
    "h1{color:#2c6fad;margin-bottom:4px;font-size:22px}",
    ".meta{color:#777;font-size:13px;margin-bottom:28px}",
    ".sec{font-size:17px;font-weight:700;margin-top:36px;margin-bottom:14px;",
    "border-bottom:2px solid #2c6fad;padding-bottom:4px;color:#2c6fad}",
    ".rq-box{background:#eef4fb;border-left:4px solid #2c6fad;padding:10px 14px;",
    "margin-bottom:4px;border-radius:0 6px 6px 0}",
    ".answer-box{background:#fffdf5;border:1px dashed #bbb;padding:10px 14px;",
    "margin-bottom:4px;border-radius:4px;min-height:36px}",
    ".solution-box{background:#eafaf1;border-left:4px solid #2ca05a;padding:10px 14px;",
    "margin-bottom:22px;border-radius:0 6px 6px 0}",
    ".label{font-size:11px;color:#888;font-style:italic;margin-top:6px;margin-bottom:2px}",
    "img{max-width:100%;margin:14px 0;border:1px solid #ddd;border-radius:4px}",
    ".print-note{background:#fff8e1;border:1px solid #ffe082;padding:10px 14px;",
    "border-radius:4px;font-size:13px;margin-bottom:24px}",
    "@media print{.print-note{display:none}body{max-width:100%}@page{margin:1.8cm}}"
  )

  paste0(
    '<!DOCTYPE html><html lang="en"><head>',
    '<meta charset="UTF-8">',
    '<title>MA153X Lesson 2 — ', nm_safe, '</title>',
    '<style>', css, '</style></head><body>',

    '<h1>MA153X — Lesson 2: Descriptive Analytics Explorer</h1>',
    '<div class="meta">Student: <strong>', nm_safe, '</strong>',
    ' &nbsp;|&nbsp; Downloaded: ', date_str, '</div>',
    '<div class="print-note">',
    '<strong>To save as PDF:</strong> use your browser’s ',
    '<em>File → Print → Save as PDF</em> (or press Ctrl+P / Cmd+P).',
    '</div>',

    # Section 1
    '<div class="sec">Section 1: Dataset Overview</div>',
    rq_html(1, "How many observations and variables does this dataset have? What does one row represent?",
            answers$ans1, sols$rq1),
    rq_html(2, "Look at the first few rows. Can you identify one numerical variable and one categorical variable?",
            answers$ans2, sols$rq2),
    rq_html(3, "What does the Sale Price variable tell us? What are reasonable minimum and maximum values?",
            answers$ans3, sols$rq3),

    # Section 2
    '<div class="sec">Section 2: Histogram</div>',
    '<img src="', hist_uri, '" alt="Histogram (state at download)">',
    rq_html(4, "Set the variable to Sale Price and bins to 30. Describe the shape of the distribution. Is it symmetric or skewed? What does that tell you about typical home prices in Ames?",
            answers$ans4, sols$rq4),
    rq_html(5, "Compare the mean and median Sale Price. Which is larger? Why might they differ? Which better represents a ‘typical’ home price?",
            answers$ans5, sols$rq5),
    rq_html(6, "Switch to ‘Year Built’. Try different bin widths. In which decade were the most homes built? How does bin width change your interpretation?",
            answers$ans6, sols$rq6),

    # Section 3
    '<div class="sec">Section 3: Boxplot</div>',
    '<img src="', box_uri, '" alt="Boxplot (state at download)">',
    rq_html(7, "Group Sale Price by Building Type. Which type has the highest median price? Which has the most variability (widest box)?",
            answers$ans7, sols$rq7),
    rq_html(8, "Notice the difference between the mean (×) and median (center line) in each group. In which group is the difference largest? What does that tell you about outliers in that group?",
            answers$ans8, sols$rq8),
    rq_html(9, "Switch grouping to Neighborhood. Which neighborhood has the highest median sale price? Which has the most outliers?",
            answers$ans9, sols$rq9),

    # Section 4
    '<div class="sec">Section 4: Scatterplot</div>',
    '<img src="', scat_uri, '" alt="Scatterplot (state at download)">',
    rq_html(10, "Plot Sale Price (y) vs. Above-Ground Living Area (x). Describe the relationship. Is it positive or negative? Strong or weak? Are there any unusual points?",
             answers$ans10, sols$rq10),
    rq_html(11, "Color the points by Building Type. Do different building types cluster in different regions of the plot? What does this suggest about how building type relates to both size and price?",
             answers$ans11, sols$rq11),
    rq_html(12, "Try Overall Quality on the x-axis with Sale Price on y. Is quality or living area a stronger predictor of price? Use the correlation value and the trend line to support your answer.",
             answers$ans12, sols$rq12),

    # Section 5
    '<div class="sec">Section 5: Variable Types</div>',
    rq_html(13, "Classify all 8 numeric variables. For each one, write a one-sentence data dictionary entry that includes the variable name, type, units (if applicable), and expected range.",
             answers$ans13, sols$rq13),

    '</body></html>'
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
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
                        padding: 8px 12px; font-size: 13px; white-space: pre-wrap; margin-bottom: 4px; }
      .preview-label { font-weight: 600; font-size: 13px; margin-top: 14px; }
      .download-note { color: #e05c2a; font-style: italic; font-size: 13px; }
    ")),
    # JavaScript handler: receives HTML string from R and triggers browser download
    tags$script(HTML("
      Shiny.addCustomMessageHandler('downloadHTML', function(data) {
        var blob = new Blob([data.content], {type: 'text/html;charset=utf-8'});
        var url  = URL.createObjectURL(blob);
        var a    = document.createElement('a');
        a.href     = url;
        a.download = data.filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        setTimeout(function() { URL.revokeObjectURL(url); }, 1000);
      });
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
            "click", strong("Download Report."),
            "Your responses to all 13 questions will be compiled into an HTML file",
            "along with the charts from each section."),
          p(class = "download-note",
            "Instructor solutions will be revealed in the downloaded file.",
            "To convert to PDF, open the file in your browser and press Ctrl+P (Cmd+P on Mac),",
            "then choose Save as PDF."),
          br(),
          wellPanel(
            h4("Student Information"),
            textInput("studentName", "Full Name", placeholder = "Enter your full name"),
            br(),
            actionButton("downloadReport", "Download Report",
                         class = "btn-primary btn-lg"),
            uiOutput("downloadStatus")
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

  # Instructor solutions — defined server-side only, never rendered in the UI
  solutions <- list(

    rq1 = paste(
      "The dataset has 2,930 observations (rows) and 82 variables (columns).",
      "One row represents a single residential home sale &mdash; a complete record",
      "of all measured attributes for one property at the time it was sold."
    ),

    rq2 = paste(
      "Numerical variables take quantitative values on which arithmetic is meaningful.",
      "Examples include Sale Price (in dollars) or Above-Ground Living Area (in sq ft).",
      "Categorical variables place homes into named groups with no inherent numerical ordering.",
      "Examples include Building Type (labels such as 1Fam or Duplex) and Neighborhood."
    ),

    rq3 = paste(
      "Sale Price records the total amount paid by the buyer, in U.S. dollars.",
      "Given that this dataset covers Ames, Iowa from 2006&ndash;2010, reasonable values",
      "range from roughly $12,000 for distressed properties to about $760,000 for",
      "high-end homes. The vast majority of sales fall between $100,000 and $300,000."
    ),

    rq4 = paste(
      "The Sale Price distribution is right-skewed (positively skewed): most homes are",
      "priced between $100,000 and $250,000, but a long tail extends toward higher values.",
      "This tells us that a typical home in Ames costs roughly $130,000&ndash;$180,000,",
      "but a minority of premium properties push prices well above that range."
    ),

    rq5 = paste(
      "The mean Sale Price is larger than the median. They differ because the distribution",
      "is right-skewed &mdash; a small number of very expensive homes pull the arithmetic",
      "mean upward without affecting the median. The median is a better measure of the",
      "typical home price because it is resistant to extreme high-end values and represents",
      "the actual middle of the price distribution."
    ),

    rq6 = paste(
      "With moderate bin widths, the histogram shows the 2000s (2000&ndash;2009) as the",
      "decade with the most homes built, followed by the 1990s. Very narrow bins reveal",
      "year-by-year variation but can look noisy, making trends harder to see. Very wide",
      "bins smooth over fluctuations and highlight broad decade-level patterns but hide",
      "finer detail. The right bin width depends on the question being asked."
    ),

    rq7 = paste(
      "Single-family homes (1Fam) and Townhouse End Units (TwnhsE) tend to have the highest",
      "median sale prices. Two-family conversions (2fmCon) and Duplexes typically have the",
      "lowest medians. Building types with taller boxes (larger IQR) &mdash; such as 1Fam",
      "&mdash; show the most variability in price, reflecting the wide range of single-family",
      "home sizes and conditions in the market."
    ),

    rq8 = paste(
      "Single-family homes (1Fam) typically show the largest gap between the mean",
      "(&times; marker) and median (center line), because this group contains the",
      "highest-priced outliers. When the mean is noticeably above the median, it signals",
      "that the group&rsquo;s distribution is right-skewed &mdash; a few very expensive",
      "properties are pulling the group average above what most homes in that group",
      "actually sold for."
    ),

    rq9 = paste(
      "Stone Brook (StoneBr) and Northridge Heights (NridgHt) consistently rank among",
      "the highest-median-price neighborhoods. Neighborhoods with the most outlier dots",
      "beyond the whiskers are typically larger, more diverse neighborhoods like North Ames",
      "(NAmes) or College Creek (CollgCr), where a wide mix of property types creates",
      "occasional extreme prices relative to the neighborhood median."
    ),

    rq10 = paste(
      "There is a strong, positive linear relationship between above-ground living area and",
      "sale price (Pearson r &approx; 0.71): larger homes tend to command higher prices.",
      "The scatter increases at larger home sizes. A few unusual points appear &mdash; some",
      "very large homes sell at unexpectedly low prices (possibly due to poor condition),",
      "and there are a couple of extremely large properties that may be influential outliers."
    ),

    rq11 = paste(
      "When colored by Building Type, the groups occupy distinct regions: single-family",
      "homes (1Fam) span the full range of sizes and prices; duplexes and two-family",
      "conversions cluster at smaller sizes and lower prices; townhouses occupy intermediate",
      "positions. This suggests building type acts as a confounder &mdash; part of the",
      "size&ndash;price relationship is driven by the type of property, since different",
      "building types are designed to different scales and market segments."
    ),

    rq12 = paste(
      "Overall Quality (r &approx; 0.80) is a stronger predictor of sale price than",
      "Above-Ground Living Area (r &approx; 0.71). The scatterplot for quality vs. price",
      "shows a steep, consistent trend with less scatter around the line. This makes",
      "practical sense: buyers pay a premium for high-quality finishes and construction,",
      "and a well-built smaller home often commands a higher price than a larger but",
      "lower-quality property."
    ),

    rq13 = paste(
      "<strong>Sale Price:</strong> Continuous numerical &mdash; total dollar amount paid;",
      "ranges from ~$12,789 to ~$755,000.<br>",
      "<strong>Above-Ground Living Area:</strong> Continuous numerical &mdash; finished",
      "living space above grade in sq ft; ranges from 334 to 5,642 sq ft.<br>",
      "<strong>Overall Quality (1&ndash;10):</strong> Ordinal categorical &mdash; rated",
      "scale from 1 (Very Poor) to 10 (Very Excellent); values are ordered but intervals",
      "are not guaranteed equal.<br>",
      "<strong>Year Built:</strong> Discrete numerical &mdash; calendar year originally",
      "constructed; ranges from 1872 to 2010.<br>",
      "<strong>Bedrooms Above Grade:</strong> Discrete numerical &mdash; count of bedrooms",
      "above basement level; ranges from 0 to 8.<br>",
      "<strong>Full Bathrooms:</strong> Discrete numerical &mdash; count of full bathrooms;",
      "ranges from 0 to 4.<br>",
      "<strong>Total Basement Area:</strong> Continuous numerical &mdash; total basement",
      "floor area in sq ft; ranges from 0 to 6,110 sq ft.<br>",
      "<strong>Garage Area:</strong> Continuous numerical &mdash; floor area of the garage",
      "in sq ft; ranges from 0 to 1,418 sq ft."
    )
  )

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
      if (input$typeClass == correct) {
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

  # ── Download: build HTML and push to browser ──────────────────────────────
  output$downloadStatus <- renderUI(NULL)

  observeEvent(input$downloadReport, {
    output$downloadStatus <- renderUI(
      p(style = "color:#777; font-size:13px; margin-top:10px;",
        "Building report, please wait...")
    )

    # Render plots to base64 data URIs
    hist_uri <- plot_to_base64(hist_plot_obj())
    box_uri  <- plot_to_base64(box_plot_obj())
    scat_uri <- plot_to_base64(scat_plot_obj())

    # Collect answers
    answers <- setNames(
      lapply(paste0("ans", 1:13), function(id) {
        v <- input[[id]]
        if (is.null(v) || nchar(trimws(v)) == 0) "" else v
      }),
      paste0("ans", 1:13)
    )

    nm <- if (nchar(trimws(input$studentName)) > 0) input$studentName else "Student"

    html_content <- build_report_html(nm, answers, hist_uri, box_uri, scat_uri, solutions)

    filename <- paste0(
      "MA153X_LSN2_",
      gsub("[^A-Za-z0-9]", "_", trimws(nm)),
      "_", format(Sys.Date(), "%Y%m%d"), ".html"
    )

    session$sendCustomMessage("downloadHTML",
                              list(content = html_content, filename = filename))

    output$downloadStatus <- renderUI(
      p(style = "color:#2ca05a; font-size:13px; margin-top:10px;",
        paste0("✓ Report downloaded as ", filename,
               ". Open it in your browser and press Ctrl+P to save as PDF."))
    )
  })
}

shinyApp(ui, server)

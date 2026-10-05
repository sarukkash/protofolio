# ============================================================
# INVESTMENT ANALYTICS PRO
# Universal Investment Data Analyzer
# ============================================================

options(
  shiny.maxRequestSize = 1024^3
)

library(shiny)
library(data.table)
library(DT)
library(ggplot2)

# ============================================================
# HELPER FUNCTIONS
# ============================================================

find_column <- function(df, possible_names) {
  
  if (ncol(df) == 0) {
    return(NULL)
  }
  
  original <- names(df)
  clean_names <- tolower(
    gsub(
      "[^a-z0-9]",
      "",
      original
    )
  )
  
  targets <- tolower(
    gsub(
      "[^a-z0-9]",
      "",
      possible_names
    )
  )
  
  # Exact match first
  for (target in targets) {
    
    index <- which(
      clean_names == target
    )
    
    if (length(index) > 0) {
      return(original[index[1]])
    }
  }
  
  # Partial match
  for (target in targets) {
    
    index <- grep(
      target,
      clean_names,
      fixed = TRUE
    )
    
    if (length(index) > 0) {
      return(original[index[1]])
    }
  }
  
  NULL
}


safe_numeric <- function(x) {
  
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  
  x <- as.character(x)
  
  x <- gsub(
    ",",
    "",
    x
  )
  
  x <- gsub(
    "\\$",
    "",
    x
  )
  
  x <- gsub(
    "₹",
    "",
    x
  )
  
  x <- gsub(
    "€",
    "",
    x
  )
  
  x <- gsub(
    "£",
    "",
    x
  )
  
  x <- gsub(
    "[^0-9.\\-]",
    "",
    x
  )
  
  suppressWarnings(
    as.numeric(x)
  )
}


extract_year <- function(x) {
  
  x <- as.character(x)
  
  result <- rep(
    NA_character_,
    length(x)
  )
  
  index <- grepl(
    "[0-9]{4}",
    x
  )
  
  result[index] <- sub(
    ".*?([0-9]{4}).*",
    "\\1",
    x[index]
  )
  
  result
}


format_money <- function(x) {
  
  if (is.na(x) || !is.finite(x)) {
    return("N/A")
  }
  
  if (abs(x) >= 1000000000) {
    
    return(
      paste0(
        "$",
        round(
          x / 1000000000,
          2
        ),
        "B"
      )
    )
  }
  
  if (abs(x) >= 1000000) {
    
    return(
      paste0(
        "$",
        round(
          x / 1000000,
          2
        ),
        "M"
      )
    )
  }
  
  if (abs(x) >= 1000) {
    
    return(
      paste0(
        "$",
        round(
          x / 1000,
          2
        ),
        "K"
      )
    )
  }
  
  paste0(
    "$",
    format(
      round(x, 2),
      big.mark = ","
    )
  )
}


empty_message <- function(message) {
  
  data.frame(
    Message = message,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  tags$head(
    
    tags$title(
      "Investment Analytics Pro"
    ),
    
    tags$style(
      HTML(
        "
        /* ================================
           GLOBAL
        ================================= */
        
        body {
          margin: 0;
          background: #f3f6fa;
          font-family: 'Segoe UI', Arial, sans-serif;
          color: #172033;
        }
        
        .container-fluid {
          padding: 0 25px 40px 25px;
        }
        
        /* ================================
           HEADER
        ================================= */
        
        .app-header {
          background: linear-gradient(
            135deg,
            #071a35,
            #0b3155,
            #087f8c
          );
          color: white;
          padding: 30px 35px;
          margin: 0 -15px 25px -15px;
          box-shadow: 0 5px 20px rgba(0,0,0,0.12);
        }
        
        .app-title {
          font-size: 32px;
          font-weight: 700;
          margin-bottom: 5px;
        }
        
        .app-subtitle {
          font-size: 15px;
          opacity: 0.85;
        }
        
        /* ================================
           SIDEBAR
        ================================= */
        
        .sidebarPanel {
          background: white;
        }
        
        .sidebar-card {
          background: white;
          padding: 20px;
          border-radius: 14px;
          box-shadow: 0 3px 15px rgba(25,45,70,0.08);
          border: 1px solid #e4eaf1;
          margin-bottom: 18px;
        }
        
        .sidebar-title {
          font-weight: 700;
          color: #0b3155;
          font-size: 18px;
          margin-bottom: 15px;
        }
        
        /* ================================
           KPI CARDS
        ================================= */
        
        .kpi-card {
          background: white;
          border-radius: 15px;
          padding: 20px;
          min-height: 125px;
          box-shadow: 0 4px 18px rgba(20,40,70,0.08);
          border: 1px solid #e3eaf2;
          margin-bottom: 20px;
        }
        
        .kpi-title {
          color: #68758a;
          font-size: 13px;
          text-transform: uppercase;
          letter-spacing: 0.7px;
          font-weight: 600;
        }
        
        .kpi-value {
          color: #092d50;
          font-size: 28px;
          font-weight: 750;
          margin-top: 10px;
        }
        
        .kpi-icon {
          float: right;
          font-size: 25px;
          color: #078b91;
        }
        
        /* ================================
           CONTENT CARDS
        ================================= */
        
        .content-card {
          background: white;
          border-radius: 15px;
          padding: 22px;
          margin-bottom: 22px;
          border: 1px solid #e1e8ef;
          box-shadow: 0 3px 15px rgba(20,40,70,0.06);
        }
        
        .section-title {
          font-size: 21px;
          font-weight: 700;
          color: #092d50;
          margin-bottom: 15px;
        }
        
        .section-description {
          color: #718096;
          font-size: 13px;
          margin-bottom: 18px;
        }
        
        /* ================================
           TABS
        ================================= */
        
        .nav-tabs {
          border-bottom: 1px solid #dce4ed;
          margin-bottom: 20px;
        }
        
        .nav-tabs > li > a {
          color: #526174;
          font-weight: 600;
          border: none !important;
          padding: 14px 16px;
        }
        
        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:hover {
          color: #087f8c;
          background: white;
          border-bottom: 3px solid #087f8c !important;
        }
        
        /* ================================
           BUTTONS
        ================================= */
        
        .btn-primary {
          background: #087f8c !important;
          border-color: #087f8c !important;
          border-radius: 8px;
          font-weight: 600;
        }
        
        .btn-success {
          background: #16805c !important;
          border-color: #16805c !important;
          border-radius: 8px;
        }
        
        .btn-info {
          background: #286aa6 !important;
          border-color: #286aa6 !important;
          border-radius: 8px;
        }
        
        /* ================================
           INPUTS
        ================================= */
        
        .form-control,
        .selectize-input {
          border-radius: 8px !important;
          border: 1px solid #d5dee8 !important;
        }
        
        /* ================================
           ALERTS
        ================================= */
        
        .info-box {
          background: #eef8fa;
          border-left: 4px solid #087f8c;
          padding: 15px;
          border-radius: 8px;
          margin-bottom: 15px;
        }
        
        .success-box {
          background: #edf9f3;
          border-left: 4px solid #16805c;
          padding: 15px;
          border-radius: 8px;
        }
        
        /* ================================
           TABLE
        ================================= */
        
        .dataTables_wrapper {
          font-size: 13px;
        }
        
        /* ================================
           FOOTER
        ================================= */
        
        .footer {
          text-align: center;
          color: #7a8797;
          padding: 25px;
          font-size: 13px;
        }
        "
      )
    )
  ),
  
  
  # ==========================================================
  # HEADER
  # ==========================================================
  
  div(
    class = "app-header",
    
    div(
      class = "app-title",
      "Investment Analytics Pro"
    ),
    
    div(
      class = "app-subtitle",
      "Universal Investment Data Analysis & Business Intelligence Dashboard"
    )
  ),
  
  
  sidebarLayout(
    
    # ========================================================
    # SIDEBAR
    # ========================================================
    
    sidebarPanel(
      
      width = 3,
      
      div(
        class = "sidebar-card",
        
        div(
          class = "sidebar-title",
          "DATA CENTER"
        ),
        
        fileInput(
          "file",
          "Upload CSV Dataset",
          accept = c(
            ".csv",
            "text/csv"
          )
        ),
        
        actionButton(
          "load",
          "Load Dataset",
          class = "btn-primary",
          width = "100%"
        ),
        
        br(),
        br(),
        
        uiOutput(
          "load_status"
        )
      ),
      
      
      div(
        class = "sidebar-card",
        
        div(
          class = "sidebar-title",
          "FILTERS"
        ),
        
        selectInput(
          "investor",
          "Investor",
          choices = "All"
        ),
        
        selectInput(
          "company",
          "Company",
          choices = "All"
        ),
        
        selectInput(
          "round",
          "Funding Round",
          choices = "All"
        ),
        
        selectInput(
          "year",
          "Year",
          choices = "All"
        ),
        
        selectInput(
          "industry",
          "Industry",
          choices = "All"
        ),
        
        selectInput(
          "country",
          "Country",
          choices = "All"
        ),
        
        actionButton(
          "clear_filters",
          "Clear Filters",
          class = "btn-info",
          width = "100%"
        )
      ),
      
      
      div(
        class = "sidebar-card",
        
        div(
          class = "sidebar-title",
          "EXPORT"
        ),
        
        downloadButton(
          "download",
          "Download Filtered CSV",
          class = "btn-success",
          width = "100%"
        ),
        
        br(),
        br(),
        
        downloadButton(
          "download_summary",
          "Download Summary",
          class = "btn-info",
          width = "100%"
        )
      )
      
    ),
    
    
    # ========================================================
    # MAIN PANEL
    # ========================================================
    
    mainPanel(
      
      width = 9,
      
      tabsetPanel(
        
        # ====================================================
        # DASHBOARD
        # ====================================================
        
        tabPanel(
          "Dashboard",
          
          br(),
          
          fluidRow(
            
            column(
              3,
              div(
                class = "kpi-card",
                div(
                  class = "kpi-icon",
                  "●"
                ),
                div(
                  class = "kpi-title",
                  "Total Records"
                ),
                div(
                  class = "kpi-value",
                  textOutput("total_records")
                )
              )
            ),
            
            column(
              3,
              div(
                class = "kpi-card",
                div(
                  class = "kpi-icon",
                  "◆"
                ),
                div(
                  class = "kpi-title",
                  "Investors"
                ),
                div(
                  class = "kpi-value",
                  textOutput("unique_investors")
                )
              )
            ),
            
            column(
              3,
              div(
                class = "kpi-card",
                div(
                  class = "kpi-icon",
                  "■"
                ),
                div(
                  class = "kpi-title",
                  "Companies"
                ),
                div(
                  class = "kpi-value",
                  textOutput("unique_companies")
                )
              )
            ),
            
            column(
              3,
              div(
                class = "kpi-card",
                div(
                  class = "kpi-icon",
                  "▲"
                ),
                div(
                  class = "kpi-title",
                  "Funding Rounds"
                ),
                div(
                  class = "kpi-value",
                  textOutput("funding_rounds")
                )
              )
            )
          ),
          
          
          fluidRow(
            
            column(
              4,
              div(
                class = "kpi-card",
                div(
                  class = "kpi-title",
                  "Total Funding"
                ),
                div(
                  class = "kpi-value",
                  textOutput("total_funding")
                )
              )
            ),
            
            column(
              4,
              div(
                class = "kpi-card",
                div(
                  class = "kpi-title",
                  "Average Investment"
                ),
                div(
                  class = "kpi-value",
                  textOutput("average_funding")
                )
              )
            ),
            
            column(
              4,
              div(
                class = "kpi-card",
                div(
                  class = "kpi-title",
                  "Data Completeness"
                ),
                div(
                  class = "kpi-value",
                  textOutput("completeness")
                )
              )
            )
          ),
          
          
          div(
            class = "content-card",
            
            div(
              class = "section-title",
              "Investment Activity"
            ),
            
            div(
              class = "section-description",
              "Year-wise investment activity after applying the selected filters."
            ),
            
            plotOutput(
              "dashboard_year_plot",
              height = "420px"
            )
          ),
          
          
          fluidRow(
            
            column(
              6,
              
              div(
                class = "content-card",
                
                div(
                  class = "section-title",
                  "Top Investors"
                ),
                
                DTOutput(
                  "dashboard_investors"
                )
              )
            ),
            
            column(
              6,
              
              div(
                class = "content-card",
                
                div(
                  class = "section-title",
                  "Top Companies"
                ),
                
                DTOutput(
                  "dashboard_companies"
                )
              )
            )
          ),
          
          
          div(
            class = "content-card",
            
            div(
              class = "section-title",
              "Dataset Information"
            ),
            
            verbatimTextOutput(
              "dataset_info"
            )
          )
        ),
        
        
        # ====================================================
        # INVESTOR
        # ====================================================
        
        tabPanel(
          "Investor Analysis",
          
          br(),
          
          div(
            class = "content-card",
            
            div(
              class = "section-title",
              "Investor Performance"
            ),
            
            div(
              class = "section-description",
              "Compare investor activity, companies funded and total investment."
            ),
            
            plotOutput(
              "investor_plot",
              height = "500px"
            )
          ),
          
          div(
            class = "content-card",
            
            DTOutput(
              "investor_table"
            )
          )
        ),
        
        
        # ====================================================
        # COMPANY
        # ====================================================
        
        tabPanel(
          "Company Analysis",
          
          br(),
          
          div(
            class = "content-card",
            
            div(
              class = "section-title",
              "Company Investment Analysis"
            ),
            
            plotOutput(
              "company_plot",
              height = "500px"
            )
          ),
          
          div(
            class = "content-card",
            
            DTOutput(
              "company_table"
            )
          )
        ),
        
        
        # ====================================================
        # FUNDING
        # ====================================================
        
        tabPanel(
          "Funding Analysis",
          
          br(),
          
          div(
            class = "content-card",
            
            div(
              class = "section-title",
              "Funding Round Performance"
            ),
            
            plotOutput(
              "round_plot",
              height = "500px"
            )
          ),
          
          div(
            class = "content-card",
            
            DTOutput(
              "round_table"
            )
          )
        ),
        
        
        # ====================================================
        # YEAR
        # ====================================================
        
        tabPanel(
          "Year Analysis",
          
          br(),
          
          div(
            class = "content-card",
            
            div(
              class = "section-title",
              "Investment Growth Over Time"
            ),
            
            plotOutput(
              "year_plot",
              height = "500px"
            )
          ),
          
          div(
            class = "content-card",
            
            DTOutput(
              "year_table"
            )
          )
        ),
        
        
        # ====================================================
        # INDUSTRY
        # ====================================================
        
        tabPanel(
          "Industry Analysis",
          
          br(),
          
          div(
            class = "content-card",
            
            div(
              class = "section-title",
              "Industry Investment Analysis"
            ),
            
            plotOutput(
              "industry_plot",
              height = "500px"
            )
          ),
          
          div(
            class = "content-card",
            
            DTOutput(
              "industry_table"
            )
          )
        ),
        
        
        # ====================================================
        # COUNTRY
        # ====================================================
        
        tabPanel(
          "Country Analysis",
          
          br(),
          
          div(
            class = "content-card",
            
            div(
              class = "section-title",
              "Country Investment Analysis"
            ),
            
            plotOutput(
              "country_plot",
              height = "500px"
            )
          ),
          
          div(
            class = "content-card",
            
            DTOutput(
              "country_table"
            )
          )
        ),
        
        
        # ====================================================
        # DATA QUALITY
        # ====================================================
        
        tabPanel(
          "Data Quality",
          
          br(),
          
          div(
            class = "content-card",
            
            div(
              class = "section-title",
              "Data Quality Report"
            ),
            
            DTOutput(
              "quality_table"
            )
          ),
          
          div(
            class = "content-card",
            
            div(
              class = "section-title",
              "Detected Columns"
            ),
            
            verbatimTextOutput(
              "detected_columns"
            )
          )
        ),
        
        
        # ====================================================
        # RECORDS
        # ====================================================
        
        tabPanel(
          "Investment Records",
          
          br(),
          
          div(
            class = "content-card",
            
            div(
              class = "section-title",
              "Investment Records"
            ),
            
            div(
              class = "section-description",
              "Search, sort and filter the uploaded investment records."
            ),
            
            DTOutput(
              "data_table"
            )
          )
        ),
        
        
        # ====================================================
        # PREDICTION
        # ====================================================
        
        tabPanel(
          "Prediction",
          
          br(),
          
          div(
            class = "content-card",
            
            div(
              class = "section-title",
              "Funding Prediction"
            ),
            
            div(
              class = "section-description",
              "Simple linear regression based on year and investment activity."
            ),
            
            uiOutput(
              "prediction_ui"
            ),
            
            br(),
            
            actionButton(
              "predict",
              "Predict Funding",
              class = "btn-primary"
            ),
            
            br(),
            br(),
            
            verbatimTextOutput(
              "prediction_result"
            )
          )
        )
      ),
      
      
      div(
        class = "footer",
        "Investment Analytics Pro | Built with R Shiny"
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # DATA STORAGE
  # ==========================================================
  
  investment_data <- reactiveVal(NULL)
  
  
  # ==========================================================
  # LOAD DATA
  # ==========================================================
  
  observeEvent(
    input$load,
    
    {
      
      req(input$file)
      
      tryCatch(
        
        {
          
          data <- fread(
            input$file$datapath,
            showProgress = FALSE,
            encoding = "UTF-8"
          )
          
          
          # --------------------------------------------------
          # Clean names
          # --------------------------------------------------
          
          names(data) <- trimws(
            names(data)
          )
          
          names(data) <- make.unique(
            names(data)
          )
          
          
          # --------------------------------------------------
          # Empty rows
          # --------------------------------------------------
          
          if (nrow(data) > 0) {
            
            keep_rows <- apply(
              data,
              1,
              function(x) {
                !all(
                  is.na(x) |
                    trimws(
                      as.character(x)
                    ) == ""
                )
              }
            )
            
            data <- data[
              keep_rows
            ]
          }
          
          
          # --------------------------------------------------
          # Detect columns
          # --------------------------------------------------
          
          investor_col <- find_column(
            data,
            c(
              "investor_name",
              "investor",
              "investor_object_id",
              "investor_id",
              "investorid"
            )
          )
          
          
          company_col <- find_column(
            data,
            c(
              "company_name",
              "company",
              "funded_company",
              "funded_object_id",
              "company_id",
              "companyid"
            )
          )
          
          
          round_col <- find_column(
            data,
            c(
              "funding_round_type",
              "funding_type",
              "round_type",
              "funding_round_id",
              "funding_round",
              "round"
            )
          )
          
          
          amount_col <- find_column(
            data,
            c(
              "funding_amount",
              "investment_amount",
              "amount",
              "funding",
              "investment",
              "raised_amount",
              "total_funding",
              "funding_usd"
            )
          )
          
          
          date_col <- find_column(
            data,
            c(
              "created_at",
              "created",
              "date",
              "funding_date",
              "investment_date",
              "updated_at"
            )
          )
          
          
          industry_col <- find_column(
            data,
            c(
              "industry",
              "sector",
              "category",
              "market"
            )
          )
          
          
          country_col <- find_column(
            data,
            c(
              "country",
              "country_name",
              "location",
              "nation"
            )
          )
          
          
          # --------------------------------------------------
          # Standard internal columns
          # --------------------------------------------------
          
          if (!is.null(investor_col)) {
            
            data[, `_investor` := as.character(
              get(investor_col)
            )]
            
          } else {
            
            data[, `_investor` := "Unknown"]
          }
          
          
          if (!is.null(company_col)) {
            
            data[, `_company` := as.character(
              get(company_col)
            )]
            
          } else {
            
            data[, `_company` := "Unknown"]
          }
          
          
          if (!is.null(round_col)) {
            
            data[, `_round` := as.character(
              get(round_col)
            )]
            
          } else {
            
            data[, `_round` := "Unknown"]
          }
          
          
          if (!is.null(amount_col)) {
            
            data[, `_amount` := safe_numeric(
              get(amount_col)
            )]
            
          } else {
            
            data[, `_amount` := NA_real_]
          }
          
          
          if (!is.null(date_col)) {
            
            date_values <- as.character(
              data[[date_col]]
            )
            
            data[, `_year` := extract_year(
              date_values
            )]
            
          } else {
            
            year_col <- find_column(
              data,
              c(
                "year",
                "funding_year",
                "investment_year"
              )
            )
            
            if (!is.null(year_col)) {
              
              data[, `_year` := as.character(
                get(year_col)
              )]
              
            } else {
              
              data[, `_year` := NA_character_]
            }
          }
          
          
          if (!is.null(industry_col)) {
            
            data[, `_industry` := as.character(
              get(industry_col)
            )]
            
          } else {
            
            data[, `_industry` := "Unknown"]
          }
          
          
          if (!is.null(country_col)) {
            
            data[, `_country` := as.character(
              get(country_col)
            )]
            
          } else {
            
            data[, `_country` := "Unknown"]
          }
          
          
          # --------------------------------------------------
          # Replace empty values
          # --------------------------------------------------
          
          for (
            column in c(
              "_investor",
              "_company",
              "_round",
              "_industry",
              "_country"
            )
          ) {
            
            data[
              is.na(get(column)) |
                trimws(
                  get(column)
                ) == "",
              (column) := "Unknown"
            ]
          }
          
          
          # --------------------------------------------------
          # Store
          # --------------------------------------------------
          
          investment_data(
            data
          )
          
          
          # --------------------------------------------------
          # Update filters
          # --------------------------------------------------
          
          updateSelectInput(
            session,
            "investor",
            choices = c(
              "All",
              sort(
                unique(
                  data$`_investor`
                )
              )
            ),
            selected = "All"
          )
          
          
          updateSelectInput(
            session,
            "company",
            choices = c(
              "All",
              sort(
                unique(
                  data$`_company`
                )
              )
            ),
            selected = "All"
          )
          
          
          updateSelectInput(
            session,
            "round",
            choices = c(
              "All",
              sort(
                unique(
                  data$`_round`
                )
              )
            ),
            selected = "All"
          )
          
          
          years <- sort(
            unique(
              data$`_year`
            )
          )
          
          years <- years[
            !is.na(years)
          ]
          
          updateSelectInput(
            session,
            "year",
            choices = c(
              "All",
              years
            ),
            selected = "All"
          )
          
          
          updateSelectInput(
            session,
            "industry",
            choices = c(
              "All",
              sort(
                unique(
                  data$`_industry`
                )
              )
            ),
            selected = "All"
          )
          
          
          updateSelectInput(
            session,
            "country",
            choices = c(
              "All",
              sort(
                unique(
                  data$`_country`
                )
              )
            ),
            selected = "All"
          )
          
          
          showNotification(
            paste(
              format(
                nrow(data),
                big.mark = ","
              ),
              "records loaded successfully."
            ),
            type = "message",
            duration = 5
          )
          
        },
        
        error = function(e) {
          
          showNotification(
            paste(
              "Error loading file:",
              e$message
            ),
            type = "error",
            duration = 10
          )
        }
      )
    }
  )
  
  
  # ==========================================================
  # LOAD STATUS
  # ==========================================================
  
  output$load_status <- renderUI({
    
    data <- investment_data()
    
    if (is.null(data)) {
      
      return(
        div(
          class = "info-box",
          "No dataset loaded yet."
        )
      )
    }
    
    div(
      class = "success-box",
      paste(
        "Dataset loaded:",
        format(
          nrow(data),
          big.mark = ","
        ),
        "records"
      )
    )
  })
  
  
  # ==========================================================
  # CLEAR FILTERS
  # ==========================================================
  
  observeEvent(
    input$clear_filters,
    
    {
      
      updateSelectInput(
        session,
        "investor",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "company",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "round",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "year",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "industry",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "country",
        selected = "All"
      )
    }
  )
  
  
  # ==========================================================
  # FILTERED DATA
  # ==========================================================
  
  filtered_data <- reactive({
    
    data <- investment_data()
    
    req(data)
    
    result <- copy(data)
    
    
    if (
      !is.null(input$investor) &&
      input$investor != "All"
    ) {
      
      result <- result[
        `_investor` == input$investor
      ]
    }
    
    
    if (
      !is.null(input$company) &&
      input$company != "All"
    ) {
      
      result <- result[
        `_company` == input$company
      ]
    }
    
    
    if (
      !is.null(input$round) &&
      input$round != "All"
    ) {
      
      result <- result[
        `_round` == input$round
      ]
    }
    
    
    if (
      !is.null(input$year) &&
      input$year != "All"
    ) {
      
      result <- result[
        `_year` == input$year
      ]
    }
    
    
    if (
      !is.null(input$industry) &&
      input$industry != "All"
    ) {
      
      result <- result[
        `_industry` == input$industry
      ]
    }
    
    
    if (
      !is.null(input$country) &&
      input$country != "All"
    ) {
      
      result <- result[
        `_country` == input$country
      ]
    }
    
    
    result
  })
  
  
  # ==========================================================
  # DASHBOARD KPI
  # ==========================================================
  
  output$total_records <- renderText({
    
    data <- filtered_data()
    
    format(
      nrow(data),
      big.mark = ","
    )
  })
  
  
  output$unique_investors <- renderText({
    
    data <- filtered_data()
    
    format(
      uniqueN(
        data$`_investor`
      ),
      big.mark = ","
    )
  })
  
  
  output$unique_companies <- renderText({
    
    data <- filtered_data()
    
    format(
      uniqueN(
        data$`_company`
      ),
      big.mark = ","
    )
  })
  
  
  output$funding_rounds <- renderText({
    
    data <- filtered_data()
    
    format(
      uniqueN(
        data$`_round`
      ),
      big.mark = ","
    )
  })
  
  
  output$total_funding <- renderText({
    
    data <- filtered_data()
    
    amount <- sum(
      data$`_amount`,
      na.rm = TRUE
    )
    
    if (
      all(
        is.na(
          data$`_amount`
        )
      )
    ) {
      return("N/A")
    }
    
    format_money(
      amount
    )
  })
  
  
  output$average_funding <- renderText({
    
    data <- filtered_data()
    
    if (
      all(
        is.na(
          data$`_amount`
        )
      )
    ) {
      return("N/A")
    }
    
    format_money(
      mean(
        data$`_amount`,
        na.rm = TRUE
      )
    )
  })
  
  
  output$completeness <- renderText({
    
    data <- filtered_data()
    
    if (
      nrow(data) == 0
    ) {
      return("0%")
    }
    
    total_cells <- nrow(data) * ncol(data)
    
    missing_cells <- sum(
      is.na(data)
    )
    
    percent <- (
      1 -
        missing_cells /
        total_cells
    ) * 100
    
    paste0(
      round(
        percent,
        1
      ),
      "%"
    )
  })
  
  
  # ==========================================================
  # DATASET INFORMATION
  # ==========================================================
  
  output$dataset_info <- renderPrint({
    
    data <- filtered_data()
    
    cat(
      "INVESTMENT ANALYTICS REPORT\n"
    )
    
    cat(
      "===========================\n\n"
    )
    
    cat(
      "Rows:",
      format(
        nrow(data),
        big.mark = ","
      ),
      "\n"
    )
    
    cat(
      "Columns:",
      ncol(data),
      "\n"
    )
    
    cat(
      "Investors:",
      uniqueN(
        data$`_investor`
      ),
      "\n"
    )
    
    cat(
      "Companies:",
      uniqueN(
        data$`_company`
      ),
      "\n"
    )
    
    cat(
      "Funding Rounds:",
      uniqueN(
        data$`_round`
      ),
      "\n"
    )
    
    if (
      !all(
        is.na(
          data$`_amount`
        )
      )
    ) {
      
      cat(
        "Total Funding:",
        format_money(
          sum(
            data$`_amount`,
            na.rm = TRUE
          )
        ),
        "\n"
      )
    }
  })
  
  
  # ==========================================================
  # DASHBOARD YEAR CHART
  # ==========================================================
  
  output$dashboard_year_plot <- renderPlot({
    
    data <- filtered_data()
    
    data <- data[
      !is.na(
        data$`_year`
      )
    ]
    
    if (
      nrow(data) == 0
    ) {
      return(
        ggplot() +
          theme_void() +
          annotate(
            "text",
            x = 1,
            y = 1,
            label = "No year information available"
          )
      )
    }
    
    result <- data[
      ,
      .(
        Investments = .N
      ),
      by = `_year`
    ]
    
    setorder(
      result,
      `_year`
    )
    
    ggplot(
      result,
      aes(
        x = `_year`,
        y = Investments,
        group = 1
      )
    ) +
      
      geom_line(
        linewidth = 1.2,
        color = "#087f8c"
      ) +
      
      geom_point(
        size = 3,
        color = "#0b3155"
      ) +
      
      labs(
        title = "Investment Activity by Year",
        x = "Year",
        y = "Investments"
      ) +
      
      theme_minimal(
        base_size = 14
      )
  })
  
  
  # ==========================================================
  # DASHBOARD TOP INVESTORS
  # ==========================================================
  
  output$dashboard_investors <- renderDT({
    
    data <- filtered_data()
    
    result <- data[
      ,
      .(
        Investments = .N,
        Companies = uniqueN(
          `_company`
        )
      ),
      by = `_investor`
    ]
    
    setorder(
      result,
      -Investments
    )
    
    result <- head(
      result,
      10
    )
    
    names(result)[1] <- "Investor"
    
    datatable(
      result,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        searching = FALSE,
        lengthChange = FALSE,
        scrollX = TRUE
      )
    )
  })
  
  
  # ==========================================================
  # DASHBOARD TOP COMPANIES
  # ==========================================================
  
  output$dashboard_companies <- renderDT({
    
    data <- filtered_data()
    
    result <- data[
      ,
      .(
        Investments = .N,
        Investors = uniqueN(
          `_investor`
        )
      ),
      by = `_company`
    ]
    
    setorder(
      result,
      -Investments
    )
    
    result <- head(
      result,
      10
    )
    
    names(result)[1] <- "Company"
    
    datatable(
      result,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        searching = FALSE,
        lengthChange = FALSE,
        scrollX = TRUE
      )
    )
  })
  
  
  # ==========================================================
  # INVESTOR ANALYSIS
  # ==========================================================
  
  investor_summary <- reactive({
    
    data <- filtered_data()
    
    result <- data[
      ,
      .(
        Investments = .N,
        Companies = uniqueN(
          `_company`
        ),
        Funding_Rounds = uniqueN(
          `_round`
        ),
        Total_Funding = sum(
          `_amount`,
          na.rm = TRUE
        ),
        Average_Funding = mean(
          `_amount`,
          na.rm = TRUE
        )
      ),
      by = `_investor`
    ]
    
    setorder(
      result,
      -Investments
    )
    
    result
  })
  
  
  output$investor_table <- renderDT({
    
    result <- investor_summary()
    
    names(result)[1] <- "Investor"
    
    datatable(
      head(
        result,
        100
      ),
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })
  
  
  output$investor_plot <- renderPlot({
    
    result <- head(
      investor_summary(),
      10
    )
    
    req(
      nrow(result) > 0
    )
    
    ggplot(
      result,
      aes(
        x = reorder(
          `_investor`,
          Investments
        ),
        y = Investments
      )
    ) +
      
      geom_col(
        fill = "#087f8c"
      ) +
      
      coord_flip() +
      
      labs(
        title = "Top 10 Investors",
        x = "Investor",
        y = "Number of Investments"
      ) +
      
      theme_minimal(
        base_size = 14
      )
  })
  
  
  # ==========================================================
  # COMPANY ANALYSIS
  # ==========================================================
  
  company_summary <- reactive({
    
    data <- filtered_data()
    
    result <- data[
      ,
      .(
        Investments = .N,
        Investors = uniqueN(
          `_investor`
        ),
        Funding_Rounds = uniqueN(
          `_round`
        ),
        Total_Funding = sum(
          `_amount`,
          na.rm = TRUE
        ),
        Average_Funding = mean(
          `_amount`,
          na.rm = TRUE
        )
      ),
      by = `_company`
    ]
    
    setorder(
      result,
      -Investments
    )
    
    result
  })
  
  
  output$company_table <- renderDT({
    
    result <- company_summary()
    
    names(result)[1] <- "Company"
    
    datatable(
      head(
        result,
        100
      ),
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })
  
  
  output$company_plot <- renderPlot({
    
    result <- head(
      company_summary(),
      10
    )
    
    req(
      nrow(result) > 0
    )
    
    ggplot(
      result,
      aes(
        x = reorder(
          `_company`,
          Investments
        ),
        y = Investments
      )
    ) +
      
      geom_col(
        fill = "#286aa6"
      ) +
      
      coord_flip() +
      
      labs(
        title = "Top 10 Funded Companies",
        x = "Company",
        y = "Investments"
      ) +
      
      theme_minimal(
        base_size = 14
      )
  })
  
  
  # ==========================================================
  # FUNDING ROUND
  # ==========================================================
  
  round_summary <- reactive({
    
    data <- filtered_data()
    
    result <- data[
      ,
      .(
        Investments = .N,
        Investors = uniqueN(
          `_investor`
        ),
        Companies = uniqueN(
          `_company`
        ),
        Total_Funding = sum(
          `_amount`,
          na.rm = TRUE
        )
      ),
      by = `_round`
    ]
    
    setorder(
      result,
      -Investments
    )
    
    result
  })
  
  
  output$round_table <- renderDT({
    
    result <- round_summary()
    
    names(result)[1] <- "Funding Round"
    
    datatable(
      head(
        result,
        100
      ),
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })
  
  
  output$round_plot <- renderPlot({
    
    result <- head(
      round_summary(),
      10
    )
    
    req(
      nrow(result) > 0
    )
    
    ggplot(
      result,
      aes(
        x = reorder(
          `_round`,
          Investments
        ),
        y = Investments
      )
    ) +
      
      geom_col(
        fill = "#16805c"
      ) +
      
      coord_flip() +
      
      labs(
        title = "Funding Round Activity",
        x = "Funding Round",
        y = "Investments"
      ) +
      
      theme_minimal(
        base_size = 14
      )
  })
  
  
  # ==========================================================
  # YEAR ANALYSIS
  # ==========================================================
  
  year_summary <- reactive({
    
    data <- filtered_data()
    
    data <- data[
      !is.na(
        `_year`
      )
    ]
    
    result <- data[
      ,
      .(
        Investments = .N,
        Investors = uniqueN(
          `_investor`
        ),
        Companies = uniqueN(
          `_company`
        ),
        Funding_Rounds = uniqueN(
          `_round`
        ),
        Total_Funding = sum(
          `_amount`,
          na.rm = TRUE
        )
      ),
      by = `_year`
    ]
    
    setorder(
      result,
      `_year`
    )
    
    result
  })
  
  
  output$year_table <- renderDT({
    
    result <- year_summary()
    
    names(result)[1] <- "Year"
    
    datatable(
      result,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })
  
  
  output$year_plot <- renderPlot({
    
    result <- year_summary()
    
    req(
      nrow(result) > 0
    )
    
    ggplot(
      result,
      aes(
        x = `_year`,
        y = Investments,
        group = 1
      )
    ) +
      
      geom_line(
        linewidth = 1.2,
        color = "#087f8c"
      ) +
      
      geom_point(
        size = 3,
        color = "#0b3155"
      ) +
      
      labs(
        title = "Investment Activity by Year",
        x = "Year",
        y = "Investments"
      ) +
      
      theme_minimal(
        base_size = 14
      )
  })
  
  
  # ==========================================================
  # INDUSTRY ANALYSIS
  # ==========================================================
  
  industry_summary <- reactive({
    
    data <- filtered_data()
    
    result <- data[
      ,
      .(
        Investments = .N,
        Investors = uniqueN(
          `_investor`
        ),
        Companies = uniqueN(
          `_company`
        ),
        Total_Funding = sum(
          `_amount`,
          na.rm = TRUE
        )
      ),
      by = `_industry`
    ]
    
    setorder(
      result,
      -Investments
    )
    
    result
  })
  
  
  output$industry_table <- renderDT({
    
    result <- industry_summary()
    
    names(result)[1] <- "Industry"
    
    datatable(
      result,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })
  
  
  output$industry_plot <- renderPlot({
    
    result <- head(
      industry_summary(),
      15
    )
    
    req(
      nrow(result) > 0
    )
    
    ggplot(
      result,
      aes(
        x = reorder(
          `_industry`,
          Investments
        ),
        y = Investments
      )
    ) +
      
      geom_col(
        fill = "#7a55a3"
      ) +
      
      coord_flip() +
      
      labs(
        title = "Investment Activity by Industry",
        x = "Industry",
        y = "Investments"
      ) +
      
      theme_minimal(
        base_size = 14
      )
  })
  
  
  # ==========================================================
  # COUNTRY ANALYSIS
  # ==========================================================
  
  country_summary <- reactive({
    
    data <- filtered_data()
    
    result <- data[
      ,
      .(
        Investments = .N,
        Investors = uniqueN(
          `_investor`
        ),
        Companies = uniqueN(
          `_company`
        ),
        Total_Funding = sum(
          `_amount`,
          na.rm = TRUE
        )
      ),
      by = `_country`
    ]
    
    setorder(
      result,
      -Investments
    )
    
    result
  })
  
  
  output$country_table <- renderDT({
    
    result <- country_summary()
    
    names(result)[1] <- "Country"
    
    datatable(
      result,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })
  
  
  output$country_plot <- renderPlot({
    
    result <- head(
      country_summary(),
      15
    )
    
    req(
      nrow(result) > 0
    )
    
    ggplot(
      result,
      aes(
        x = reorder(
          `_country`,
          Investments
        ),
        y = Investments
      )
    ) +
      
      geom_col(
        fill = "#d57a2a"
      ) +
      
      coord_flip() +
      
      labs(
        title = "Investment Activity by Country",
        x = "Country",
        y = "Investments"
      ) +
      
      theme_minimal(
        base_size = 14
      )
  })
  
  
  # ==========================================================
  # DATA QUALITY
  # ==========================================================
  
  output$quality_table <- renderDT({
    
    data <- filtered_data()
    
    result <- data.frame(
      
      Column = names(data),
      
      Type = sapply(
        data,
        function(x) {
          class(x)[1]
        }
      ),
      
      Missing = sapply(
        data,
        function(x) {
          sum(
            is.na(x)
          )
        }
      ),
      
      Missing_Percent = round(
        sapply(
          data,
          function(x) {
            mean(
              is.na(x)
            ) * 100
          }
        ),
        2
      ),
      
      Unique = sapply(
        data,
        function(x) {
          uniqueN(x)
        }
      ),
      
      stringsAsFactors = FALSE
    )
    
    datatable(
      result,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
  
  
  # ==========================================================
  # DETECTED COLUMNS
  # ==========================================================
  
  output$detected_columns <- renderPrint({
    
    data <- investment_data()
    
    req(data)
    
    cat(
      "AUTOMATIC COLUMN DETECTION\n"
    )
    
    cat(
      "==========================\n\n"
    )
    
    cat(
      "Investor Column : ",
      find_column(
        data,
        c(
          "investor_name",
          "investor",
          "investor_object_id",
          "investor_id"
        )
      ),
      "\n"
    )
    
    cat(
      "Company Column  : ",
      find_column(
        data,
        c(
          "company_name",
          "company",
          "funded_object_id",
          "company_id"
        )
      ),
      "\n"
    )
    
    cat(
      "Round Column    : ",
      find_column(
        data,
        c(
          "funding_round_type",
          "funding_round_id",
          "funding_round",
          "round"
        )
      ),
      "\n"
    )
    
    cat(
      "Amount Column   : ",
      find_column(
        data,
        c(
          "funding_amount",
          "investment_amount",
          "amount",
          "funding",
          "investment"
        )
      ),
      "\n"
    )
    
    cat(
      "Date Column     : ",
      find_column(
        data,
        c(
          "created_at",
          "date",
          "funding_date",
          "investment_date"
        )
      ),
      "\n"
    )
    
    cat(
      "Industry Column : ",
      find_column(
        data,
        c(
          "industry",
          "sector",
          "category"
        )
      ),
      "\n"
    )
    
    cat(
      "Country Column  : ",
      find_column(
        data,
        c(
          "country",
          "country_name",
          "location"
        )
      ),
      "\n"
    )
  })
  
  
  # ==========================================================
  # ALL RECORDS
  # ==========================================================
  
  output$data_table <- renderDT({
    
    data <- filtered_data()
    
    # Don't show internal helper columns
    visible_columns <- setdiff(
      names(data),
      c(
        "_investor",
        "_company",
        "_round",
        "_amount",
        "_year",
        "_industry",
        "_country"
      )
    )
    
    display_data <- data[
      ,
      ..visible_columns
    ]
    
    datatable(
      display_data,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 25,
        lengthMenu = c(
          10,
          25,
          50,
          100
        ),
        scrollX = TRUE,
        deferRender = TRUE,
        processing = TRUE
      )
    )
  })
  
  
  # ==========================================================
  # PREDICTION UI
  # ==========================================================
  
  output$prediction_ui <- renderUI({
    
    data <- investment_data()
    
    req(data)
    
    years <- suppressWarnings(
      as.numeric(
        data$`_year`
      )
    )
    
    years <- years[
      is.finite(years)
    ]
    
    if (
      length(years) < 5 ||
      all(
        is.na(
          data$`_amount`
        )
      )
    ) {
      
      return(
        div(
          class = "info-box",
          "Prediction requires at least 5 valid year records and a numeric funding amount column."
        )
      )
    }
    
    sliderInput(
      "prediction_year",
      "Future Year",
      min = max(
        years,
        na.rm = TRUE
      ) + 1,
      max = max(
        years,
        na.rm = TRUE
      ) + 10,
      value = max(
        years,
        na.rm = TRUE
      ) + 1,
      step = 1
    )
  })
  
  
  # ==========================================================
  # PREDICTION
  # ==========================================================
  
  prediction_value <- eventReactive(
    input$predict,
    
    {
      
      data <- investment_data()
      
      req(data)
      
      model_data <- data[
        !is.na(`_year`) &
          !is.na(`_amount`),
        .(
          Year = as.numeric(`_year`),
          Funding = `_amount`
        )
      ]
      
      model_data <- model_data[
        is.finite(Year) &
          is.finite(Funding)
      ]
      
      if (
        nrow(model_data) < 5
      ) {
        
        return(
          list(
            success = FALSE,
            message = "Not enough valid data for prediction."
          )
        )
      }
      
      yearly <- model_data[
        ,
        .(
          Funding = sum(
            Funding,
            na.rm = TRUE
          )
        ),
        by = Year
      ]
      
      if (
        nrow(yearly) < 2
      ) {
        
        return(
          list(
            success = FALSE,
            message = "At least two different years are required."
          )
        )
      }
      
      model <- lm(
        Funding ~ Year,
        data = yearly
      )
      
      future_year <- input$prediction_year
      
      prediction <- predict(
        model,
        newdata = data.frame(
          Year = future_year
        )
      )
      
      list(
        success = TRUE,
        year = future_year,
        prediction = prediction,
        model = model
      )
    }
  )
  
  
  output$prediction_result <- renderPrint({
    
    result <- prediction_value()
    
    if (
      !result$success
    ) {
      
      cat(
        result$message
      )
      
      return()
    }
    
    cat(
      "FUNDING PREDICTION\n"
    )
    
    cat(
      "==================\n\n"
    )
    
    cat(
      "Predicted Year : ",
      result$year,
      "\n"
    )
    
    cat(
      "Predicted Funding : ",
      format_money(
        result$prediction
      ),
      "\n\n"
    )
    
    cat(
      "Method : Linear Regression\n"
    )
    
    cat(
      "\nNote: This is a simple educational prediction and should not be treated as a financial forecast."
    )
  })
  
  
  # ==========================================================
  # DOWNLOAD FILTERED DATA
  # ==========================================================
  
  output$download <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "filtered_investments_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      data <- filtered_data()
      
      visible_columns <- setdiff(
        names(data),
        c(
          "_investor",
          "_company",
          "_round",
          "_amount",
          "_year",
          "_industry",
          "_country"
        )
      )
      
      fwrite(
        data[
          ,
          ..visible_columns
        ],
        file
      )
    }
  )
  
  
  # ==========================================================
  # DOWNLOAD SUMMARY
  # ==========================================================
  
  output$download_summary <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "investment_summary_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      data <- filtered_data()
      
      summary_data <- data[
        ,
        .(
          Investments = .N,
          Companies = uniqueN(
            `_company`
          ),
          Investors = uniqueN(
            `_investor`
          ),
          Funding_Rounds = uniqueN(
            `_round`
          ),
          Total_Funding = sum(
            `_amount`,
            na.rm = TRUE
          ),
          Average_Funding = mean(
            `_amount`,
            na.rm = TRUE
          )
        )
      ]
      
      fwrite(
        summary_data,
        file
      )
    }
  )
  
}


# ============================================================
# RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
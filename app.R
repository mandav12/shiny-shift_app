library(shiny)
library(dplyr)
library(tidyr)
library(DT)

# Function to prepare transition table
t_prepare_transition <- function(data, s_parcat1 = NULL, s_param = NULL) {
  cat_levels <- c("NORMAL", "LOW", "HIGH", "Missing")
  
  # Filter based on selected PARCAT1 and PARAM
  if (!is.null(s_parcat1) && s_parcat1 != "All") {
    data <- data %>% filter(PARCAT1 == s_parcat1)
  }
  if (!is.null(s_param) && s_param != "All") {
    data <- data %>% filter(PARAM == s_param)
  }
  
  # Baseline
  baseline <- data %>%
    filter(ABLFL == "Y") %>%
    distinct(USUBJID, PARAM, TRT01A, BNRIND, PARCAT1)
  
  # Max post-baseline by AVAL
  post_max <- data %>%
    filter(ABLFL != "Y", !is.na(AVAL)) %>%
    group_by(USUBJID, PARAM, TRT01A) %>%
    slice_max(order_by = AVAL, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    distinct(USUBJID, PARAM, TRT01A, ANRIND)
  
  # Combine
  combined <- baseline %>%
    left_join(post_max, by = c("USUBJID", "PARAM", "TRT01A")) %>%
    mutate(
      BNRIND = ifelse(is.na(BNRIND) | BNRIND == "", "Missing", BNRIND),
      ANRIND = ifelse(is.na(ANRIND) | ANRIND == "", "Missing", ANRIND),
      BNRIND = factor(BNRIND, levels = cat_levels),
      ANRIND = factor(ANRIND, levels = cat_levels)
    )
  
  # If nothing to summarize, return fallback
  if (nrow(combined) == 0 || all(is.na(combined$BNRIND)) || all(is.na(combined$ANRIND))) {
    return(data.frame(Note = "No baseline or post-baseline data available for this category."))
  }
  
  # Baseline totals
  baseline_totals <- combined %>%
    group_by(PARAM, TRT01A, BNRIND, PARCAT1) %>%
    summarise(N_baseline = n(), .groups = "drop")
  
  # Transition summary
  summary_table <- combined %>%
    group_by(PARAM, TRT01A, BNRIND, ANRIND, PARCAT1) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(PARAM, TRT01A) %>%
    mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
    ungroup() %>%
    left_join(baseline_totals, by = c("PARAM", "TRT01A", "BNRIND", "PARCAT1")) %>%
    complete(
      PARAM, TRT01A, PARCAT1,
      BNRIND = factor(cat_levels, levels = cat_levels),
      ANRIND = factor(cat_levels, levels = cat_levels),
      fill = list(Count = 0, Percent = 0, N_baseline = 0)
    )
  
  # Pivot to wide format
  transition_table <- summary_table %>%
    mutate(Count_Percent = paste0(Count, " (", Percent, ")")) %>%
    select(PARAM, TRT01A, PARCAT1, BNRIND, N_baseline, ANRIND, Count_Percent) %>%
    pivot_wider(
      names_from = ANRIND,
      values_from = Count_Percent,
      values_fill = list(Count_Percent = "0 (0)")
    ) %>%
    select(PARAM, TRT01A, PARCAT1, BNRIND, N_baseline, all_of(cat_levels)) %>%
    arrange(PARAM, TRT01A, PARCAT1, BNRIND)
  
  return(transition_table)
}

# UI
ui <- fluidPage(
  titlePanel("Shift from Baseline to Max Post-Baseline Normal Range (Safety Population)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("parcat1", "PARCAT1 Category:", choices = NULL),
      selectInput("param", "Parameter:", choices = NULL),
      selectInput("treatment", "Treatment:", choices = NULL)
    ),
    mainPanel(
      uiOutput("current_filters"),
      DTOutput("shift_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Update PARCAT1 choices
  observe({
    parcat1s <- sort(unique(as.character(adlb$PARCAT1)))
    updateSelectInput(session, "parcat1", choices = c("All", parcat1s), selected = "All")
  })
  
  # Update PARAM choices based on PARCAT1
  observeEvent(input$parcat1, {
    params <- adlb %>%
      filter(if (input$parcat1 != "All") PARCAT1 == input$parcat1 else TRUE) %>%
      pull(PARAM) %>%
      unique() %>%
      sort()
    updateSelectInput(session, "param", choices = c("All", as.character(params)), selected = "All")
  })
  
  # Reactive full table
  full_tbl <- reactive({
    tbl <- t_prepare_transition(adlb, input$parcat1, input$param)
    if (!"Note" %in% names(tbl)) {
      trts <- sort(unique(as.character(tbl$TRT01A)))
      updateSelectInput(session, "treatment", choices = c("All", trts), selected = "All")
    }
    tbl
  })
  
  # Output selected filters
  output$current_filters <- renderUI({
    tagList(
      h4("Selected Filters"),
      p(strong("PARCAT1:"), input$parcat1),
      p(strong("PARAM:"), input$param),
      p(strong("Treatment:"), input$treatment)
    )
  })
  
  # Render final table
  output$shift_table <- renderDT({
    tbl <- full_tbl()
    
    if ("Note" %in% names(tbl)) {
      return(datatable(tbl, options = list(dom = 't'), rownames = FALSE))
    }
    
    if (input$treatment != "All") {
      tbl <- filter(tbl, TRT01A == input$treatment)
    }
    
    # Clean list columns and pad display
    tbl <- tbl %>%
      mutate(across(where(is.list), ~ as.character(.))) %>%
      group_by(PARAM, TRT01A, PARCAT1) %>%
      mutate(
        PARAM = if_else(row_number() == 1, as.character(PARAM), ""),
        TRT01A = if_else(row_number() == 1, as.character(TRT01A), ""),
        PARCAT1 = if_else(row_number() == 1, as.character(PARCAT1), "")
      ) %>%
      ungroup()
    
    datatable(tbl, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
}

# Run the app
shinyApp(ui, server)


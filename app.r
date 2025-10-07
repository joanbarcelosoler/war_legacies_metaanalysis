## ===================================+===================================+==========
## Study  : Attitudinal and Behavioral Legacies of Wartime Violence: A Meta-Analysis
## Task   : Present plots, study details, and dynamic chart
## Script : Creates legacy Shiny App
## Date   : 26-09-2025 
## ===================================+====================================+==========

rm(list = ls())

## ... Loading packages

library(shiny)
library(shinyWidgets)
library(shinyMatrix)
library(tidyverse)
library(readr)
library(bslib)
library(metafor)
library(showtext)
library(rsconnect)
library(extrafont)


## ... Loading data  

metadata <- read_csv("metadata.csv")

reports <- read_csv("reports.csv")

metadata <- metadata %>% rename("Author_yr" = "authoryear")

metadata <- left_join(metadata, reports, by = "Author_yr")

file.attributes <- haven::read_dta("updated.attributes.review.dta") %>%
  select(Author_yr, ExposureLagMean) %>%
  distinct(Author_yr, .keep_all = TRUE) %>%
  mutate(
    ExposureLag = case_when(
      ExposureLagMean <= 5 ~ "Upto 5 years",
      ExposureLagMean > 5 & ExposureLagMean <= 20 ~ "More than 5 years and upto 20 years",
      ExposureLagMean > 20 ~ "More than 20 years",
      TRUE ~ NA_character_  
    )
  )

metadata <- left_join(metadata, file.attributes, by = "Author_yr")

## ... Setting up the User Interface object


ui <- fluidPage(theme = bs_theme(
      version = 4, bootswatch = "flatly",
      primary = "black",
      secondary = "#00843d",
      success = "#66a182",
      base_font = font_google("Merriweather"),
      font_scale = 0.70),
  
  div(style = "text-align: center;", 
      titlePanel("Attitudinal and Behavioral Legacies of Wartime Violence: A Meta-Analysis")),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      pickerInput(
        inputId = "outcome",
        label = h4("Outcome Category"),
        choices = list(
          "Altruism" = "altruism",
          "Antagonism towards peace" = "antipeace",
          "Authoritarian attitudes" = "authoritarian",
          "Generalized trust" = "generalizedtrust",
          "Group identification" = "groupid",
          "Social group participation" = "groups",
          "Group-based voting" = "groupvoting",
          "Hawkish security preferences" = "hawkish",
          "Ingroup trust" = "ingrouptrust",
          "Institutional mistrust" = "instmistrust",
          "Intergroup distrust, bias, or discrimination" = "intergroup",
          "Community leadership" = "leadership",
          "Normative prosociality" = "normative",
          "Political intolerance" = "politicalintolerance",
          "Poltical interest" = "politicalinterest",
          "Political participation" = "politicalparticipation",
          "Support for punitive justice" = "punitive",
          "Rightwing attitudes" = "rightwing_ideology",
          "Social intolerance" = "socialintolerance",
          "Threat perception" = "threat",
          "Voting" = "voting",
          "Antagonism towards wartime enemies" = "warenemies",
          "Extreme ideology" = "xtrideology"
        ),
        selected = "altruism",
        options = list(`actions-box` = TRUE),
        multiple = FALSE),
      pickerInput(
        inputId = "model",
        label = h4("Model Specification"),
        choices = list(
          "Full Controls" = "Full",
          "Bivariate/No Controls" = "Bivariate",
          "Quasi-Experimental" = "Quasi-Experimental"),
        selected = "Full",
        options = list(`actions-box` = TRUE),
        multiple = FALSE),
        pickerInput(
          inputId = "country",
          label = h4("Excluded Country"),
          choices = c("NONE","Bosnia-Herzegovina","Colombia","Croatia","Israel","Kosovo","Sierra Leone","Spain","Uganda","Ukraine"),
          selected = "NONE",
          options = list(`actions-box` = TRUE),
        multiple = FALSE),
      pickerInput(
        inputId = "cleavage",
        label = h4("Conflict Cleavage"),
        choices = list("ALL", "Territoral" = "1", "Government" = "2"),
        selected = "ALL",
        options = list(`actions-box` = TRUE),
        multiple = FALSE),
      pickerInput(
        inputId = "exposureLag",
        label = h4("Exposure Lag"),
        choices = list("ALL", 
                       "Upto 5 years",
                       "More than 5 years and upto 20 years", 
                       "More than 20 years"),
        selected = "ALL",
        options = list(`actions-box` = TRUE),
        multiple = FALSE),
      conditionalPanel(
        condition = "input.main_tabs == 'Dynamic Plot'",
        
        numericInput("yi_val","Effect Size:", value = 0.1, min = 0, step = 0.001),
        sliderInput("value_slider_yi", "Adjust Value:", value = 0.1, min = -1, max = 1, step = 0.0001),
        
        numericInput("se_val","Standard Error:", value = 0.1, min = 0, step = 0.001),
        sliderInput("value_slider_se", "Adjust Value:", value = 0.1, min = 0, max = 1, step = 0.0001),
        
        numericInput("n_val","Sample Size:", value = 1000, min = 0, step = 100),
        sliderInput("value_slider_n", "Adjust Value:", value = 1000, min = 0, max = 10000, step = 200)
        )
    ),
        
    mainPanel(
      tabsetPanel(id = "main_tabs",
                  type = "tabs",
                  tabPanel("About", icon = icon("info-circle"),
                           br(),
                           div(class = "about",
                               p("This is the dynamic page to interact with the results from the study on the effects of wartime violence on polical attitudes and behaviors:"),
                               tags$ul(
                                 tags$li("Barceló, J (2025).",
                                         a("Attitudinal and Behavioral Legacies of Wartime Violence: A Meta-Analysis.",
                                           href = "https://drive.google.com/file/d/1E4LfDwmtSM6SwjMj5iYPsWogzhpuA1Sa/view",
                                           target = "_blank", rel = "noopener noreferrer"),
                                         em("American Poltical Science Review.")) ),
                               p("This paper presents a meta-analysis of 172 quantitative studies across more than 50 countries, assessing the effects of wartime violence on 22 outcomes spanning four broad areas: (a) civic and political engagement, prosociality, and trust; (b) attitudinal hardening toward wartime enemies; (c) identification with one’s own wartime-aligned group; and (d) generalized attitudinal hardening."),
                               hr(),
                               p(style = "text-align: right",
                                 icon("copyright"), a("Joan Barceló", href = "https://www.joanbarcelo.com/", target = "_blank", rel = "noopener noreferrer"), "2025"))),
                  tabPanel("Manuscripts", icon = icon("list-alt"),
                             p("All primary studies that meet your predefined criteria are listed below. For more information about the criteria for inclusion and exclusion of studies please refer to Appendix A."),
                           DT::dataTableOutput("metadata_dt")),
                  tabPanel("Plot", icon = icon("chart-bar"),
                           p("The solid vertical red line indicates the pooled-effect estimate obtained from running a three-level meta analytic model. The dashed lines mark out the confidence intervals (95%) for the estimate."),
                           helpText(p("Note:The graph needs a few moments to load."), style = "font-size:12px"),
                           plotOutput("metaplot")),
                  tabPanel("Dynamic Plot", icon = icon("hand-pointer"),
                           p("With this dynamic plot you can reestimate the model to check the change hypothetical effect estimates might have on the pooled effect for each outcome."),
                           plotOutput("dynamic_metaplot")
                  )
      )
    )
  )
)

## ... Setting up the server object

server <- function(input, output, session) {

  currentData <- reactive({

      if (is.null(input$country) || identical(input$country, "NONE")) {
        excluded_countries <- character(0)  
      } else {
        excluded_countries <- input$country  
      }
      
      if (is.null(input$cleavage) || identical(input$cleavage, "ALL")) {
        selected_cleavages <- unique(metadata$UCDPConflictCleavage)  
      } else {
        selected_cleavages <- input$cleavage
      }
    
    if (is.null(input$exposureLag) || identical(input$exposureLag, "ALL")) {
      selected_exposureLags <- unique(metadata$ExposureLag)  
    } else {
      selected_exposureLags <- input$exposureLag
    }
      
      metadata %>%
        filter(
          model_type %in% input$model,
          outcome %in% input$outcome,
          UCDPConflictCleavage %in% selected_cleavages,
          ExposureLag %in% selected_exposureLags,
        !(Country %in% excluded_countries)  
        ) %>%
        mutate(row_id = row_number())
    })
    
  
  all_fit <- reactive({
    req(currentData())

    if (nrow(currentData()) < 2) return(NULL)
    rma.mv(yi, vi,
           random = ~ 1 | Author_yr/row_id,
           data   = currentData(), test = "t")
  })

  all_df <- reactive({
    fit <- all_fit()
    if (is.null(fit)) return(data.frame(coef=NA, ci.lb95=NA, ci.ub95=NA))
    data.frame(
      coef     = unname(fit$beta),
      ci.lb95  = fit$ci.lb,
      ci.ub95  = fit$ci.ub
    )
  })
  
  currentEst  <- reactive({ all_df()$coef })
  currentEstL <- reactive({ all_df()$ci.lb95 })
  currentEstU <- reactive({ all_df()$ci.ub95 })
  
  output$metadata_dt <- DT::renderDataTable({
    req(currentData())
    reports %>%
      filter(Author_yr %in% currentData()$Author_yr) %>%
      arrange(Author_yr) %>%
      select(Authors, Year, Publication, Manuscript.Title, Conflict, Country, Study.ID)
  })

  output$metaplot <- renderPlot({
    df_current <- currentData()
    if (!is.null(df_current) && nrow(df_current) > 2) {
      df <- df_current %>%
        arrange(coef) %>%
        mutate(
          id = seq.int(nrow(df_current)),
          coef_lower = coef - 1.96 * se,
          coef_upper = coef + 1.96 * se)
      
      est  <- currentEst()
      estL <- currentEstL()
      estU <- currentEstU()
      
      ggplot(df, aes(x = id, y = coef)) +
        geom_point(size = 0.8, color = "darkgreen") +
        geom_hline(yintercept = 0, color = "black", ) +
        geom_linerange(aes(ymin = coef_lower, ymax = coef_upper), color = "darkgreen", alpha = 0.6) +
        geom_hline(yintercept = est,  color = "#d13b3b") +
        geom_hline(yintercept = estL, color = "#d13b3b", linetype = "dashed", alpha = 0.4) +
        geom_hline(yintercept = estU, color = "#d13b3b", linetype = "dashed", alpha = 0.4) +
        coord_flip() +
        scale_y_continuous(limits = c(-1, 1)) +
        theme_linedraw() +
        theme(
          text = element_text(size = 12, family = "Merriweather"),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.length.x = unit(0.3, "cm"),
          axis.text.x = element_text(size = 12, family = "Merriweather"),
          axis.title.x = element_text(size = 12, family = "Merriweather"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
        ) +
        ylab("Standardized Effect Size") +
        annotate("text", x = 1, y = est,
                 label = round(est, 3),
                 family = "Merriweather",
                 size = 12/.pt,
                 color = "#d13b3b", hjust = -0.15)
    } else {
      ggplot(data = data.frame(x = 10, y = 10)) +
        annotate("text", x = 5, y = 5,
                 label = "No suitable data for display have been selected") +
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank()) +
        xlab("") + ylab("")
    }
  })

  observeEvent(input$value_slider_se, {
    updateNumericInput(session, "se_val", value = input$value_slider_se)
  })
  
  observeEvent(input$value_slider_n, {
    updateNumericInput(session, "n_val", value = input$value_slider_n)
  })
  
  observeEvent(input$value_slider_yi, {
    updateNumericInput(session, "yi_val", value = input$value_slider_yi)
  })
  
  observeEvent(input$yi_val, {
    updateSliderInput(session, "value_slider_yi", value = input$yi_val)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$se_val, {
    updateSliderInput(session, "value_slider_se", value = input$se_val)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$n_val, {
    updateSliderInput(session, "value_slider_n", value = input$n_val)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  test_data <- reactive({
    req(input$yi_val, input$se_val, input$n_val)
    
    cd <- currentData()
    if (is.null(cd) || nrow(cd) == 0 || !("row_id" %in% names(cd))) {
      new_row_id <- 1
    } else {
      new_row_id <- max(cd$row_id, na.rm = TRUE) + 1
    }
    
    vi_user <- as.numeric(input$se_val)^2
    n_user <- as.numeric(input$n_val)
    
    data.frame(
      yi         = as.numeric(input$yi_val),
      vi         = vi_user,
      n          = n_user,
      coef       = as.numeric(input$yi_val),
      se         = as.numeric(input$se_val),
      Author_yr  = "USERGEN",
      outcome    = input$outcome,
      model_type = input$model,
      row_id     = new_row_id,
      stringsAsFactors = FALSE
    )
  })

  dynamicData <- reactive({

    bind_rows(currentData(), test_data()) %>%
      mutate(
        yi   = as.numeric(yi),
        vi   = as.numeric(vi),
        coef = as.numeric(coef),
        se   = as.numeric(se),
        n    = as.numeric(n),
        row_id = as.numeric(row_id),
        is_user = (Author_yr == "USERGEN")  
      )
  })

  dynamicData.meta <- reactive({
    df <- dynamicData()
    req(df)
    df_es <- escalc(measure = "COR", ri = coef, sdi = se, ni = n, data = df, vtype = "LS")
    df_es
  })
  
  dynamicFit <- reactive({
    df <- dynamicData.meta()
    req(df)
    if (nrow(df) < 2) return(NULL)
    rma.mv(yi, vi,
           random = ~ 1 | Author_yr/row_id,
           data = df,
           test = "t")
  })

  dynamicFit_att <- reactive({
    fit <- dynamicFit()
    if (is.null(fit)) return(data.frame(coef = NA, ci.lb = NA, ci.ub = NA))
    data.frame(
      coef     = unname(fit$beta),
      ci.lb    = fit$ci.lb,
      ci.ub    = fit$ci.ub
    )
  })
  
  output$dynamic_metaplot <- renderPlot({
    
    if(nrow(currentData()) > 2) {
      
      fit <- dynamicFit_att()
      df  <- dynamicData()
      
      est  <- fit$coef
      estL <- fit$ci.lb
      estU <- fit$ci.ub 
      
      df <- df %>%
        arrange(coef) %>%
        mutate(
          id = seq_len(nrow(df)),
          coef_lower = coef - 1.96*se,
          coef_upper = coef + 1.96*se
        )
      
      ggplot(df, aes(x = id, y = coef)) +
        # line ranges
        geom_linerange(aes(
          ymin = coef_lower,
          ymax = coef_upper,
          color = (Author_yr == "USERGEN")),
          alpha = 0.6) +
        # points
        geom_point(aes(
          color = (Author_yr == "USERGEN"),
          size  = (Author_yr == "USERGEN")
        )) +
        scale_color_manual(
          values = c("TRUE" = "blueviolet", "FALSE" = "darkgreen"),
          guide = "none"
        ) +
        scale_size_manual(
          values = c("TRUE" = 2.5, "FALSE" = 1),
          guide = "none"
        ) +
        geom_hline(yintercept = 0, color = "black") +
        geom_hline(yintercept = est,  color = "#d13b3b") +
        geom_hline(yintercept = estL, color = "#d13b3b", linetype = "dashed", alpha = 0.4) +
        geom_hline(yintercept = estU, color = "#d13b3b", linetype = "dashed", alpha = 0.4) +
        coord_flip() +
        scale_y_continuous(limits = c(-1, 1)) +
        theme_light() +
        theme(
          text = element_text(size = 12, family = "Merriweather"),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.length.x = unit(0.3, "cm"),
          axis.text.x = element_text(size = 12, family = "Merriweather"),
          axis.title.x = element_text(size = 12, family = "Merriweather"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
        ) +
        ylab("Standardized Effect Size") +
        annotate(
          "text", x = 1, y = est,
          label = round(est, 5),
          family = "Merriweather",
          size = 12/.pt,  
          color = "#d13b3b",
          hjust = -0.15
        )
      
    } else {
      ggplot(data = data.frame(x = 10, y = 10)) +
        annotate("text", x = 5, y = 5, 
                 label = "No suitable data for display have been selected") +
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank()) +
        xlab("") + ylab("")
    }
  })
  
}


shinyApp(ui = ui, server = server)





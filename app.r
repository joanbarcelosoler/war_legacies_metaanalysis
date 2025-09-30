## ===================================+===================================+==========
## Study  : Attitudinal and Behavioral Legacies of Wartime Violence: A Meta-Analysis
## Task   : Present plots, study details, and dynamic chart
## Script : Creates legacy Shiny App
## Date   : 26-09-2025 
## ===================================+====================================+==========

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


## ... Loading data  

metadata <- read_csv("metadata.csv")

reports <- read_csv("reports.csv")


## ... Setting up the User Interface object


ui <- fluidPage(theme = bs_theme(
    version = 4, bootswatch = "flatly",
    primary = "black",
    secondary = "#00843d",
    success = "#66a182",
    base_font = font_google("Merriweather"),
    font_scale = 0.75
  ),
  
  div(style = "text-align: center;",
      titlePanel("Attitudinal and Behavioral Legacies of Wartime Violence: A Meta-Analysis")
  ),
  
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
        multiple = FALSE
      ),
      pickerInput(
        inputId = "model",
        label = h4("Model Specification"),
        choices = list(
          "Full Controls" = "Full",
          "Bivariate/No Controls" = "Bivariate",
          "Quasi-Experimental" = "Quasi-Experimental"
        ),
        selected = "Full",
        options = list(`actions-box` = TRUE),
        multiple = FALSE
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'Dynamic Plot'",
        sliderInput("yi_val", "Effect Size:",
                  min = -1, max = 1, value = 0, step = 0.001),
        sliderInput("se_val", "Standard Error:",
                  min = -1, max = 1, value = 0, step = 0.001),
        numericInput("n_val","Sample Size:", value = 100, min = 1, step = 1)
        )
      ),
    
    # Outputs panel
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
                               p("This paper presents a meta-analysis of 172 quantitative studies across more than 50 countries, assessing the effects of wartime violence on 22 outcomes spanning four broad areas:"),
                               strong("(a) civic and political engagement, prosociality, and trust; (b) attitudinal hardening toward wartime enemies; (c) identification with one’s own wartime-aligned group; and (d) generalized attitudinal hardening."),
                               p("The analysis reveals mixed effects on engagement, prosociality, and trust: while violence can increase some forms of participation, it does not promote voting, trust, or altruism. In contrast, wartime violence consistently 
                                 heightens hostility toward formeradversaries and strengthens in-group identification and favoritism. However, there is little evidence of broader attitudinal hardening toward actors not directly involved in the conflict. 
                                 These findings challenge optimistic claims that war fosters cohesion and underscore the need for interventions that reduce intergroup hostility and foster reconciliation.More consistently, the evidence shows that wartime violence intensifies negative attitudes toward former enemies while reinforcing identification with one’s own group. 
                                 This attitudinal hardening manifests in a range of indicators, namely rejection of groups linked to former wartime adversaries, increased threat perceptions toward wartime enemies, and intergroup mistrust, bias, and discrimination. Simultaneously, individuals exposed to violence report higher ingroup trust, stronger group identification, and a greater likelihood of 
                                 voting for group-affiliated parties. Together, these effects reflect a core pattern: wartime violence deepens social divides and heightens group polarization, posing a serious challenge to reconciliation and durable peace."),
                               hr(),
                               p(style = "text-align: right",
                                 icon("copyright"), a("Barceló, J. Soler", href = "https://www.joanbarcelo.com/", target = "_blank", rel = "noopener noreferrer"), "2025")
                           )
                  ),
                  tabPanel("Data", icon = icon("list-alt"),
                           helpText(
                             p(em("Note:"),"All primary studies that meet your predefined criteria are listed below. For more information about the criteria for inclusion and exclusion of studies please refer to Appendix A."),
                             style = "font-size:12px"
                           ),
                           DT::dataTableOutput("metadata_dt")
                  ),
                  tabPanel("Plot", icon = icon("chart-bar"),
                           p("The solid vertical red line indicates the pooled-effect estimate obtained from running a three-level meta analytic model. 
                             The dashed lines mark out the confidence intervals (95%) for the estimate."),
                           helpText(p(em("Note:"),"The graph needs a few moments to load."), style = "font-size:12px"),
                           plotOutput("metaplot")
                  ),
                  tabPanel("Dynamic Plot", icon = icon("hand-pointer"),
                           helpText(p(em("Note:"), "With this dynamic plot, you can check the effect of including hypothetical effect estimates (yi) and variances (vi) using the sliders on the left to re-estimate the model and update the plot."), style = "font-size:12px"),
                           plotOutput("dynamic_metaplot")
                  )
      )
    )
  )
)

## ... Setting up the server object

server <- function(input, output) {
  
  ## ... Run the filtering as a reactive expression
  
  currentData <- reactive({
    metadata %>%
      filter(model_type %in% input$model, outcome %in% input$outcome) %>%
      mutate(row_id = row_number())
  })
  
  ## ... Fit the model
  
  all_fit <- reactive({
    rma.mv(yi, vi,
           random = ~ 1 | authoryear/row_id,
           data   = currentData(), test = "t")
  })
  
  ## ... Extract coefficients and CI
  
  all_df <- reactive({
    data.frame(
      coef     = unname(all_fit()$beta),
      ci.lb95  = all_fit()$ci.lb,
      ci.ub95  = all_fit()$ci.ub
    )
  })
  
  ## ... Helpers for estimates
  
  currentEst  <- reactive({ all_df()$coef })
  currentEstL <- reactive({ all_df()$ci.lb95 })
  currentEstU <- reactive({ all_df()$ci.ub95 })
  
  ## ... Render a data.table
  
  output$metadata_dt <- DT::renderDataTable({
    reports %>% 
      filter(Author_yr %in% currentData()$authoryear) %>%
      arrange(Author_yr) %>%
      select(Authors, Year, Publication, Manuscript.Title, Conflict, Country, Study.ID) 
  })
  
  ## ... Render a plot of overall effects
  
  output$metaplot <- renderPlot({
    if (nrow(currentData()) > 2) {
      df <- currentData() %>%
        arrange(coef) %>%
        mutate(
          id = seq.int(nrow(currentData())),
          coef_lower = coef - 1.96 * se,
          coef_upper = coef + 1.96 * se
        )
      
      est  <- currentEst()
      estL <- currentEstL()
      estU <- currentEstU()
      
      ggplot(df, aes(x = id, y = coef)) +
        geom_point(size = 0.8) +
        geom_hline(yintercept = 0, color = "black") +
        geom_linerange(aes(ymin = coef_lower, ymax = coef_upper),
                       color = "darkgreen", alpha = 0.3) +
        geom_hline(yintercept = est,  color = "#d13b3b") +
        geom_hline(yintercept = estL, color = "#d13b3b", linetype = "dashed", alpha = 0.4) +
        geom_hline(yintercept = estU, color = "#d13b3b", linetype = "dashed", alpha = 0.4) +
        coord_flip() +
        scale_y_continuous(limits = c(-1, 1)) +
        theme_classic() +
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
                 label = "No data subset for display criteria") +
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank()) +
        xlab("") + ylab("")
    }
  })


  test_data <- reactive({
    req(input$yi_val, input$se_val, input$n_val)
    
    new_row_id <- if (nrow(currentData()) == 0) 1 else max(currentData()$row_id, na.rm = TRUE) + 1
    
    vi_user <- as.numeric(input$se_val)^2
    n_user <- as.numeric(input$n_val)
    
    
    data.frame(
      yi         = as.numeric(input$yi_val),
      vi         = vi_user,
      n          = n_user,
      coef       = as.numeric(input$yi_val),
      se         = as.numeric(input$se_val),
      authoryear = "USERGEN",
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
        row_id = as.numeric(row_id)
      )
  })
  
  dynamicData.meta <- reactive({
    escalc(measure = "COR", ri = coef, sdi = se, ni = n, data = dynamicData(), vtype = "LS")
    })
  
  dynamicFit <- reactive({
    df <- dynamicData.meta()
    rma.mv(yi, vi, 
           random = ~ 1 | authoryear/row_id,
           data = df,
           test = "t")
  })
  
  dynamicFit_att <- reactive({
    data.frame(
      coef     = unname(dynamicFit()$beta),
      ci.lb95  = dynamicFit()$ci.lb,
      ci.ub95  = dynamicFit()$ci.ub
    )
  })
  
  ## ... Render a plot for dynamic effect size inputs
  
  output$dynamic_metaplot <- renderPlot({
    
    fit <- dynamicFit_att()
    df  <- dynamicData()
    
    est  <- fit$coef
    estL <- fit$ci.lb
    estU <- fit$ci.ub 
    
    df <- df %>% arrange(coef) %>%
                  mutate(id = seq_len(nrow(df)),
                         coef_lower = coef - 1.96*se,
                         coef_upper = coef + 1.96*se)
    
    ggplot(df, aes(x = id, y = coef)) +
      geom_linerange(aes(ymin = coef_lower, ymax = coef_upper), color = "darkgreen", alpha = 0.3) +
      geom_point(aes(color = authoryear == "USERGEN",size  = authoryear == "USERGEN")) +
      scale_size_manual(values = c("TRUE" = 4, "FALSE" = 1), guide = "none") +
      scale_color_manual(values = c("TRUE" = "orange", "FALSE" = "black"),guide = "none") +
      geom_hline(yintercept = 0, color = "black")+
      geom_hline(yintercept = est,  color = "#d13b3b") +
      geom_hline(yintercept = estL, color = "#d13b3b", linetype = "dashed", alpha = 0.4) +
      geom_hline(yintercept = estU, color = "#d13b3b", linetype = "dashed", alpha = 0.4) +
      coord_flip() +
      scale_y_continuous(limits = c(-1, 1)) +
      theme_classic() +
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
               label = round(est,5),
               family = "Merriweather",
               size = 12/.pt,  
               color = "#d13b3b", hjust = -0.15)
})
}

shinyApp(ui = ui, server = server)






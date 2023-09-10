# Load Packages ----
library(shiny) 
library(shinyalert) 
library(shinyBS)  
library(shinydashboard)  
library(shinyWidgets) 
library(ggplot2)  
library(dplyr) 
library(scales)
library(rmeta)
library(boastUtils)  
library(DT)

enrollmentData <- data.frame(
  Pennsylvania = c(24028, 19143),
  Non = c(16572, 4662),
  row.names = c("University Park", "Commonwealth Campuses")
) 

enrollmentData1 <- data.frame(
  Pennsylvania = c(0.557, 0.443),
  Non = c(0.78, 0.22),
  row.names = c("University Park", "Commonwealth Campuses")
) 

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "yellow",
    ## Create the app header ----
    dashboardHeader(
      title = "Odds Ratio", 
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Odds_Ratio")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ## Create the sidebar/left navigation menu ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Example", tabName = "example", icon = icon("book-open-reader")), 
        menuItem("References", tabName = "references", icon = icon("leanpub"))
        ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    
    # Create the body ----
    dashboardBody(
      tabItems(
        ## Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Odds Ratio"),
          p(" This app explores confidence intervals for odds ratios and 
            their use in the meta-analysis of real data."),
          h2("Instructions"),
          tags$ol(
            tags$li("Click on the prerequisites button to review/learn how to 
                    calculate odds ratio estimates and confidence intervals."),
            tags$li("Explore either the equal sample size or different sample
                    size situation. Then use the slider bars to change the
                    confidence level or sample size(s)."),
            tags$li("After working with the explore section, have a look at the
                    summaries of real data. Note that each line of data represents
                    a different individual experiment.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goPrereq",
              label = "Prerequisites",
              size = "large",
              icon = icon("book"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p("This app was developed and coded by Jingjun Wang and updated by 
            Shravani Samala, Junjie He, and Robert Chappell. Special thanks to
            Neil Hatfield.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Updated: 8/11/2022 by RWC.")
          )
        ),
        ## Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          tags$strong("Odds Ratios"),
          p("An odds ratio, \\(\\theta\\), relates the odds of an event under
            two different conditions. For example if two-thirds of women ate
            vegetables at lunch today (odds of 2 to 1), while only one-third of
            men ate vegetables (odds of 1 to 2), then the odds for women are four
            times as great as the odds for men (so 4 is the odds ratio)."),
          tags$strong("Approximate Confidence Intervals for Odds Ratios"),
          br(),
          fluidRow(
            column(
              width = 8,
              offset = 4,
              tags$table(
                rules = "all",
                border = "1pt",
                align = "left",
                width = "500px",
                targets = "_all",
                tags$thead(
                  tags$tr(
                    tags$th(""),
                    tags$th("Event", style = "text-align: center;"),
                    tags$th("No-Event", style = "text-align: center;"), 
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$th("Treatment", style = "text-align: center;"),
                    tags$td("a"),
                    tags$td("b"), 
                    align = "center"
                  ),
                  tags$tr(
                    tags$th("Control", style = "text-align: center;"),
                    tags$td("c"),
                    tags$td("d"), 
                    align = "center"
                  ), 
                )
              )
            )
          ),
          br(),
          p("The natural estimator of \\(\\theta\\) is the sample cross-product 
            ratio, \\(\\widehat{\\theta}=\\frac{ad}{bc}\\)"),
          p("The properties of \\(\\hat{\\theta}\\) are easily established under
            multinomial sampling, but the same properties will hold under Poisson
            or product-multinomial sampling with either the row totals and/or 
            column
            totals regarded as fixed."),
          p("The log-odds ratio \\(\\log\\hat
            {\\theta}\\)
            has a better normal approximation than \\(\\hat{\\theta}\\) does. 
            Therefore, we usually obtain a confidence interval on the log scale
            (log here means natural log). The estimated variance of \\(\\log\\hat
            {\\theta}\\)
            is easy to remember,"),
          p("\\[\\text{Estimated Variance of }\\log\\widehat{\\theta}=\\frac{1}{a}+\\frac{1}{b}+
             \\frac{1}{c}+\\frac{1}{d}\\]"),
          p("and we get a 95% confidence interval for \\(\\theta\\) by 
            exponentiating the endpoints of"),
          p("\\[\\log\\widehat{\\theta}\\pm1.96\\sqrt{\\frac{1}{a}+\\frac{1}{b}+
             \\frac{1}{c}+\\frac{1}{d}}\\]"),
          p("The final confidence interval for \\(\\theta\\) is"),
          p(style = "font-size: 18px;", 
            "\\[ \\hat{\\theta} = e^{\\log\\widehat{\\theta}\\pm1.96\\sqrt{\\frac{1}{a}+\\frac{1}{b}+
     \\frac{1}{c}+\\frac{1}{d}}} \\]")
          ),
        
        ## Set up an Explore Page----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Residency Status Differences Between Campuses"),
          p("Below are the tables for the counts and percentages of enrollment by
            residency between University Park and the Commonwealth Campuses of 
            Penn State University. Use the slide controls to change the confidence level as well as the sample sizes taken from University Park
            and the Commonwealth campuses. Observe the difference between confidence
            intervals for each sample. Check below the plot to see the sample
            counts, percentages, and odds ratio."),
          br(), 
          fluidRow(
            column(
              width = 6,
              offset = 0,
              DTOutput(outputId = "countTable1"), 
              DTOutput(outputId = "countTable2"),
              br(),
              br(),
              p("Odds of Pennsylvania residents for University Park: 1.45 "),
              p("Odds of Pennsylvania residents for other campuses: 4.11 "),
              p("Odds ratio (θ) : \\(\\frac{1.45}{4.11}= 0.35\\)"),
              br(),
              tabsetPanel(
                type = "tabs",
                id = "tabset",
                tabPanel(
                  "Seperate Sample Sizes",
                  fluid = TRUE,
                  br(),
                  wellPanel(
                  sliderInput(
                    inputId = "dLevel",
                    label = "Confidence Level",
                    min = .50,
                    max = 0.99,
                    value = 0.95,
                    step = 0.01
                  ),
                  sliderInput(
                    inputId = "nSamp1",
                    label = "Sample Size for University Park",
                    min = 30,
                    max = 200,
                    value = 50,
                    step = 5
                  ),
                  sliderInput(
                    inputId = "nSamp2",
                    label = "Sample Size for Other Campuses",
                    min = 30,
                    max = 200,
                    value = 50,
                    step = 5
                  )
                )),
                tabPanel(
                  "Same Sample Size",
                  fluid = TRUE,
                  wellPanel(
                  sliderInput(
                    inputId = "dLevel1",
                    label = "Confidence Level",
                    min = .10,
                    max = 0.99,
                    value = 0.95,
                    step = 0.01
                  ),
                  sliderInput(
                    inputId = "nSamp3",
                    label = "Sample Sizes for both University Park and Other Campuses",
                    min = 30,
                    max = 200,
                    value = 50,
                    step = 5
                  )
                )
              )
            )),
            column(
              width = 6, 
              offset = 0,
              plotOutput("CIplot", height = "600px", click = "plot_click"),
              p("Black vertical line for null theta & green vertical line 
             for true odds ratio. Click on an interval (dot) to show the
             underlying data. The first row in tables below are at University Park,
             and the second at Commonwealth Campuses",
             style="text-align: center;"), 
             fluidRow(
               column(
                 width = 6, 
                 offset = 0,
                tags$strong("Sample Counts"),
                tableOutput("sampleinfotable2")
               ), 
               column(
                 width = 6,
                 offset = 0,
                 tags$strong("Sample Percentages"),
                 tableOutput("sampleinfotable1")
               )
             ),
             uiOutput("sampleinforatioUi"),
             br(),
             bsButton(
               inputId = "newSample", 
               label = "Generate 50 New Samples",
               size = "large",
               icon("retweet"),
               style = "default"),
            ), 
          ),
        ),
        
        ## Set up a Example Page ----
        tabItem(
          tabName = "example",
          withMathJax(),
          h2("Example"),
          tabPanel(h2("example"),
                   tags$ul(tags$li("All datasets below are about the comparsion
                                   of  medical therapy or vaccine effectiveness."),
                           tags$li("Choose the dataset to see the odds ratio and
                                   related 95% confidence interval to check 
                                   which thearpy or vaccine is better.")),
                   br(),
                   fluidRow(
                     column(
                       width = 4,
                       wellPanel(
                         selectInput(
                           inputId = "sets",
                           label = "Choose a Dataset Below",
                           choices = c(
                             "Non-Small Cell Lung Cancer Treatment" = "lungCancer",
                             "Malaria Treatment" = "marlaria",
                             "Vaccines Immunogenicity" = "vaccines"
                           ),
                           selected = "lungCancer"
                         ),
                         width = validateCssUnit("70%")
                       ),
                     ),
                     column(
                       width = 8,
                       offset = 0,
                       uiOutput(outputId = "exampleGraphs"),
                     ),
                   )
          )),
        
        ## Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Attali, D., Edwards, T., Wang, Z. (2020). shinyalerts: Easily create
            pretty popup messages (modals) in 'Shiny'. R package version 2.0.0.
            Available from https://CRAN.R-project.org/package=shinyalert"
          ),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities. 
            R package version 0.1.6.3. Available from 
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeiro, B. (2018). shinydashboard: 
            Create Dashboards with 'Shiny'. R package version 0.7.1. Available 
            from https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. 
            (2020). shiny: Web Application Framework for R. R package version 
            1.5.0. Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Lock, R., Frazer, P., Morgan, K., Lock, E., and Lock, D.  Statistics:
            Unlocking the Power of Data. Wiley, 2013. "
          ),
          p(
            class = "hangingindent",
            "Lumley, T. (2018). rmeta: Meta-Analysis. R package version 3.0.
            Available from https://CRAN.R-project.org/package=rmeta"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020). shinyWidgets: 
            Custom Inputs Widgets for Shiny. R package version 0.5.3. Available 
            from https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K. (2021). dplyr: A 
            Grammar of Data Manipulation. R package version 1.0.6. Available from
            https://CRAN.R-project.org/package=dplyr"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., Chang, W., Henry, L., Pedersen, T.L., Takahashi, K., 
            Wilke, C., Woo, K., Yutani, H., Dunnington, D.  (2020). ggplot2: 
            Create Elegant Data Visualisations Using the Grammar of Graphics. R package
            version 3.3.3. Available from https://CRAN.R-project.org/package=ggplot2"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., Seidel, D. (2020). scales: Scale Functions for 
            Visualization. R package version 1.1.1. Available from 
            https://CRAN.R-project.org/package=scales"
          ),
          p(
            class = "hangingindent",
            "Pennsylvania State University. (2022). Student enrollment. 
            Available from https://datadigest.psu.edu/student-enrollment"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = "This app explores confidence intervals for odds ratios and their
              use in the meta-analysis of real data.",
      type = "info"
    )
  })
  
  observeEvent(
    eventExpr = input$goPrereq,
    handlerExpr = {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "prerequisites")
  }
  )
  
  ##table in the explore page ----
  output$countTable1 <- renderDT(
    expr = {
      datatable(
        data = enrollmentData,
        caption = "Total Number of Students Enrolled, Fall 2021",
        colnames = c("Pennsylvania", "Non-Pennsylvania"),
        rownames = TRUE,
        style = "bootstrap4",
        options = list(
          responsive = TRUE,
          scrollX = FALSE,
          ordering = FALSE,
          paging = FALSE,
          lengthChange = FALSE,
          searching = FALSE,
          info = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = 1:2)
          )
        )
      ) %>%
        formatRound(
          columns = 1:2,
          digits = 0
        )
    }
  ) 
  
  output$countTable2 <- renderDT(
    expr = {
      datatable(
        data = enrollmentData1,
        caption = "Percentages of Students Enrolled, Fall 2021",
        colnames = c("Pennsylvania", "Non-Pennsylvania"),
        rownames = TRUE,
        style = "bootstrap4",
        options = list(
          responsive = TRUE,
          scrollX = FALSE,
          ordering = FALSE,
          paging = FALSE,
          lengthChange = FALSE,
          searching = FALSE,
          info = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = 1:2)
          )
        )
      ) %>%
        formatRound(
          columns = 1:2,
        )
    }
  ) 
  
  ## example page----
  observeEvent(
    eventExpr = input$sets,
    handlerExpr = {
      output$exampleGraphs <- renderUI(
        expr = {
          if (input$sets == "lungCancer") {
            list(
              h3("Data Context"),
              p("About 80% to 85% of lung cancers are non-small cell
                 lung cancer (NSCLC). The typical treatments include
                 chemotherapy, radiation therapy and targeted therapy.
                 Gefitinib and Erlotinib are two kinds of medicine used
                 in NSCLC targeted therapy. In the two comparisons,
                 Gefitinib represents treatment groups."),
              h3("First Graph : Gefitinib vs. Chemotherapy"),
              img(src = "gvc.PNG",
                  height = "100%",
                  width = "90%",
                  algin = "middle",
                  alt = "This image contains a table and a confidence interval plot
                  the table contains six columns (Study, Treatment Effective, 
                  Treatment Non-Effective, Control Effective, Control Non-Effective,
                  and Odds Ratio. Below the rows there is a summary row that
                  contains the total Odds Ratio. There are two studies."
              ),
              h3("Second Graph : Gefitinib vs. Erlotinib"),
              img(
                src = "gve.PNG",
                height = "100%",
                width = "90%",
                algin = "middle",
                alt = "This image contains a table and a confidence interval plot
                  the table contains six columns (Study, Treatment Effective, 
                  Treatment Non-Effective, Control Effective, Control Non-Effective,
                  and Odds Ratio. Below the rows there is a summary row that
                  contains the total Odds Ratio. In this case there are three
                  studies."
              ),
              h3("Commentary"), 
              p("The first analysis in this section generally compares the effect
              of targeted therapy and chemotherapy. ",strong("Summary OR = 1.4")," which is
                                     greater than 1. However, the CI of the `
                                     Summary OR` contains 1. So we fail to reject
                                     that the effectiveness of the two ways of
                                     treatments is about equal in this case. Then
                                     the second comparison as shown above is
                                     between two medicine within targeted therapy
                                     treatment. This time,", strong("Summary OR = 0.961"),
              ". However, the CI of the `Summary OR` contains
                                     1. So we fail to reject that the effectiveness
                                     of the two medicine is about equal in this
                                     case. In two analyses, we both fail to reject
                                     the null. However, targeted therapy in
                                     general is better than chemotherapy.")
            )
          }
          else if (input$sets == "marlaria") {
            list(
              h3("Data Context"),
              p("Artemisinin is a plant-derived compound, isolated from
                           the Artemisia annua, sweet wormwood a herb employed in
                           Chinese herbal medicine. This compound (along with its
                           derivative drugs), is the World Health Organization's
                           recommended treatment against malaria caused by Plasmodium
                           falciparum. Quinine, isolated from cinchona bark, is
                           the first meditation to treat malaria. In the two
                           comparisons, artesunate-based therapies represent
                           treatment groups."),
              h3("First Graph : Artesunate-based therapies vs. Quinine 
                            (for uncomplicated malaria)"),
              img(src = "avq.PNG",
                  height = "100%",
                  width = "90%",
                  algin = "middle",
                  alt = "fill me in later"
              ),
              h3("Second Graph : Artemether vs. Quinine (for cerebral malaria)"),
              img(
                src = "amvq.PNG",
                height = "100%",
                width = "90%",
                algin = "middle",
                alt = "fill me in later"
              ),
              h3("Commentary"), 
              p("The first analysis in this section compares the effect
                of artesunate-based therapies and quinine in treating
                uncomplicated malaria in pregnancy. Although we only have
                data from three studies, the advantage of using artesunate-based
                therapies is obvious:",strong("Summary OR = 7.59")," which is greater
                than 1. Then the second analysis as shown above is between
                artemether and quinine in treating cerebral malaria in
                African children less than 15 years of age. This time,
                ",strong("Summary OR = 0.933"),".However, the CI of the `Summary OR`
                contains 1. So the effectiveness of the two medicine is
                about equal in this case.")
            )
          }
          else if (input$sets == "vaccines") {
            list(
              h3("Data Context"),
              p("A combined measles-mumps-rubella-varicella (MMRV)
                           vaccine is expected to facilitate universal immunization
                           against these 4 diseases. Here randomized controlled
                           trials (RCTs) were conducted to compare single MMRV
                           dose with measles-mumps-rubella vaccine with varicella
                           vaccine (MMR + V). All included studies reported
                           seroconversion rate as serological response outcome.
                           Seroconversion rate was defined as percent of subjects
                           initially seronegative (with titers \u2264 assay cut-offs),
                           who developed postvaccination antibody titers above the
                           assay cut-off levels. In the three comparisons, MMRV
                           represents treatment groups."),
              h3("First Graph: MMRV vs. MMR+V Against Measles"),
              img(src = "mea.PNG",
                  height = "100%",
                  width = "90%",
                  algin = "middle",
                  alt = "fill me in later"
              ),
              h3("Second Graph : MMRV vs. MMR+V Against Mumps"),
              img(
                src = "mum.PNG",
                height = "100%",
                width = "90%",
                algin = "middle",
                alt = "fill me in later"
              ),
              h3("Third Graph : MMRV vs. MMR+V Against Rubella"),
              img(
                src = "rub.PNG",
                height = "100%",
                width = "90%",
                algin = "middle",
                alt = "fill me in later"
              ),
              h3("Commentary"),
              p("The three analyses in this section compare the MMRV
                vaccine and the MMR + V vaccine in preventing measles,
                mumps, and rubella. Intuitively, we would assume that the
                effectiveness of the two kinds of vaccine is equal.
                However, in the second comparison, ",strong("Summary OR = 0.483"),"
                and the CI does not contain 1. It suggests that the MMRV
                vaccine against mumps is less effective than the MMR + V
                vaccine. It is an interesting finding.")
            )
          }
        }
      )
    }
  )
  
  ##Calculating alpha by the confidence level input----
  dalpha <- reactive(
    x = {
    (1 - input$dLevel) / 2
  }
  )
  
  ##Updating Sample Size----
  dN1 <- reactive(
    x = {
    as.integer(input$nSamp1)
  }
  )
  
  dN2 <- reactive(
    x = {
    as.integer(input$nSamp2)
  })
  dN3 <- reactive({
    as.integer(input$nSamp3)
  }
  )
  
  #generate 50 new sample----
  UPS50P <- reactive(
    x = {
    input$newSample
    data.frame(
      x = do.call(paste0("rbinom"),
          c(list(n = dN1() * 50), list(1, 0.595)))
    ) %>%
      mutate(idx = rep(1:50, each = dN1()))
  }
  )
  
  ups50p <- reactive(
    x = {
    UPS50P() %>%
      group_by(idx) %>%
      summarise(
        Count1 = sum(x))
  }
  )
  
  ups50n <- reactive(
    x = {
    data.frame(idx = rep(1:50), 
               Count2 = input$nSamp1-ups50p()[,2])
  }
  )
  
  UWS50P <- reactive(
    x = {
    input$newSample
    data.frame(
      x = do.call(paste0("rbinom"),
          c(list(n = dN2() * 50), list(1, 0.844)))
    ) %>%
      mutate(idx = rep(1:50, each = dN2()))
  }
  )
  
  uws50p <- reactive(
    x = {
    UWS50P() %>%
      group_by(idx) %>%
      summarise(
        Count3 = sum(x))
  }
  )
  
  uws50n <- reactive(
    x = {
    data.frame(idx = rep(1:50), 
               input$nSamp2-uws50p()[,2])
  }
  )
  
  data50_1 <- reactive(
    x = {
    merge(ups50p(),uws50p())
  }
  )
  
  data50_2 <- reactive(
    x = {
    merge(ups50n(),uws50n())
  }
  )
  
  data50 <- reactive(
    x = {
    merge.data.frame(data50_1(), data50_2(), by = "idx")
  }
  )
  
  ##generate 50 new sample (combined sample size)----
  UPS50P_3 <- reactive(
    x = {
    input$newSample
    data.frame(
      x = do.call(
          paste0("rbinom"),
          c(list(n = dN3() * 50), list(1, 0.595)))
    ) %>%
      mutate(idx = rep(1:50, each = dN3()))
  })
  
  ups50p_3 <- reactive(
    x = {
    UPS50P_3() %>%
      group_by(idx) %>%
      summarise(
        Count1 = sum(x))
  }
  )
  
  ups50n_3 <- reactive(
    x = {
    data.frame(idx = rep(1:50), 
               Count2 = input$nSamp3-ups50p_3()[,2])
  }
  )
  
  UWS50P_3 <- reactive(
    x = {
    input$newSample
    data.frame(
      x = do.call(
          paste0("rbinom"),
          c(list(n = dN3() * 50), list(1, 0.844)))
    ) %>%
      mutate(idx = rep(1:50, each = dN3()))
  }
  )
  
  uws50p_3 <- reactive(
    x = {
    UWS50P_3() %>%
      group_by(idx) %>%
      summarise(
        Count3 = sum(x))
  }
  )
  
  uws50n_3 <- reactive(
    x = {
    data.frame(idx = rep(1:50), 
               input$nSamp3-uws50p_3()[,2])
  }
  )
  
  data50_1_3 <- reactive(
    x = {
    merge(ups50p_3(),uws50p_3())
  }
  )
  
  data50_2_3 <- reactive(
    x = {
    merge(ups50n_3(),uws50n_3())
  }
  )
  
  newdata50 <- reactive(
    x = {
    merge.data.frame(data50_1_3(),data50_2_3(), by = "idx")
  }
  )
  
  ##calculate the interval----
  Intervals <- reactive(
    x = {
    zvalue = qnorm(((1 - input$dLevel)/2), lower.tail = F)
    sampleRatio = (data50()[,2]*data50()[,5])/(data50()[,3]*data50()[,4])
    lowerbound = exp(log(sampleRatio) - zvalue*sqrt(1/data50()[,2] + 
                                                      1/data50()[,5] +
                                                      1/data50()[,3] + 
                                                      1/data50()[,4]))
    upperbound = exp(log(sampleRatio) + zvalue*sqrt(1/data50()[,2] + 
                                                      1/data50()[,5] +
                                                      1/data50()[,3] +
                                                      1/data50()[,4]))
    data.frame(idx = rep(1:50), 
               sampleRatio,
               lowerbound,
               upperbound,
               cover = (lowerbound < 0.35) & (0.35 < upperbound))
  }
  )
  
  newIntervals <- reactive(
    x = {
    zvalue = qnorm(((1 - input$dLevel1)/2), lower.tail = F)
    sampleRatio = (newdata50()[,2]*newdata50()[,5])/(newdata50()[,3]*newdata50()[,4])
    lowerbound = exp(log(sampleRatio) - zvalue*sqrt(1/newdata50()[,2] + 
                                                      1/newdata50()[,5] + 
                                                      1/newdata50()[,3] + 
                                                      1/newdata50()[,4]))
    upperbound = exp(log(sampleRatio) + zvalue*sqrt(1/newdata50()[,2] +
                                                      1/newdata50()[,5] + 
                                                      1/newdata50()[,3] + 
                                                      1/newdata50()[,4]))
    data.frame(idx = rep(1:50), 
               sampleRatio,
               lowerbound,
               upperbound,
               cover = (lowerbound < 0.35) & (0.35 < upperbound))
  }
  )
  
  ##default as all the samples are selected----
  selSampVal <- 50
  selectedSample <- reactive(
    x = {
    if (!is.null(input$plot_click)) {
      selSampVal <<- round(input$plot_click$y)
      if (selSampVal < 1) selSampVal <<- 1
      if (selSampVal > 50) selSampVal <<- 50
    }
    selSampVal
  }
  )
  
  # selected sample----
  OneSample <- reactive(
    x = {
    data50() %>%
      filter( idx == selectedSample() )
  }
  )
  
  OneSampleColor <- reactive(
    x = {
    colors <- c("TRUE" = "#ff7532", "FALSE" = "red")
    covers <- (Intervals() %>% filter(idx == selectedSample()) )$cover
    colors[ as.character(covers) ]
  }
  )
  
  # selected sample (combined version) 
  newOneSample <- reactive(
    x = {
    newdata50() %>%
      filter( idx == selectedSample() )
  }
  )
  
  newOneSampleColor <- reactive(
    x = {
    colors <- c("TRUE" = "#ff7532", "FALSE" = "red")
    covers <- (newIntervals() %>% filter(idx == selectedSample()) )$cover
    colors[ as.character(covers) ]
  }
  )
  
  ##print the CIplot----
  output$CIplot <- renderPlot(
    expr = {
      if (input$tabset == "Same Sample Size") {
        validate(
          need(is.numeric(input$nSamp3),
               message = "Please input sample size")
        )
        ggplot(data = newIntervals()) +
          geom_pointrange(
            aes(x = idx,
                ymin = lowerbound,
                ymax = upperbound,
                y = sampleRatio,
                colour = cover,
                alpha = idx == selectedSample(),
                size = idx == selectedSample()
            )) +
          theme_bw() +
          geom_hline(yintercept = 1, linewidth = 1.8, colour = "#000000", alpha = 1) +
          geom_hline(yintercept = .35, linewidth = 1.8, colour = boastPalette[3],
                     alpha = 1) +
          coord_flip() +
          scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = .8), guide = FALSE) +
          scale_color_manual(values = c("FALSE" = "#BC204B", "TRUE" = "#1E407C"),
                             guide = FALSE) +
          scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .5), guide = FALSE) +
          lims(y = c(-0.01, 4.55)) +
          labs(title = paste0(100 * input$dLevel1, "% Confidence Intervals"),
               x = "", y = "") +
          theme(legend.position = "none",
                axis.text.x = element_text(size = 14, face = "bold"),
                axis.text.y = element_text(size = 14, face = "bold"),
                axis.ticks.y = element_blank(),
                plot.title = element_text(size = 18),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14))
      } else {
        validate(
          need(is.numeric(input$nSamp1), is.numeric(input$nSamp2),
               message = "Please input sample size")
        )
        ggplot(data = Intervals()) +
          geom_pointrange(
            aes(x = idx,
                ymin = lowerbound,
                ymax = upperbound,
                y = sampleRatio,
                colour = cover,
                alpha = idx == selectedSample(),
                size = idx == selectedSample()
            )) +
          theme_bw() +
          geom_hline(yintercept = 1, linewidth = 1.8, colour = "#000000", alpha = 1) +
          geom_hline(yintercept = .35, linewidth = 1.8, colour = boastPalette[3], 
                     alpha = 1) +
          coord_flip() +
          scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = .8), guide = FALSE) +
          scale_color_manual(values = c("FALSE" = "#BC204B", "TRUE" = "#1E407C"),
                             guide = FALSE) +
          scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .5), guide = FALSE) +
          lims(y = c(-0.01, 4.55)) +
          labs(title = paste0(100 * input$dLevel, "% Confidence Intervals"),
               x = "", y = "") +
          theme(legend.position = "none",
                axis.text.x = element_text(size = 14, face = "bold"),
                axis.text.y = element_text(size = 14, face = "bold"),
                axis.ticks.y = element_blank(),
                plot.title = element_text(size = 18),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14))
      }
    },
    alt = "This is a plot with a confidence interval for every point, the points
        are listed vertically and you click in the same row as a data point to learn
        more about it",
  )
  
  
  
  ## sample display----
  output$sampleinfotable1 = renderTable({
  if (input$tabset == "Same Sample Size") {
    validate(
      need(is.numeric(input$nSamp3),
           message = "Please input sample size")
    )
    ctable <- matrix(c(
      percent(newOneSample()[, 2] / input$nSamp3),
      percent(newOneSample()[, 3] / input$nSamp3),
      percent(newOneSample()[, 4] / input$nSamp3),
      percent(newOneSample()[, 5] / input$nSamp3)
    ), ncol = 2,
    dimnames = list(Campus = c("University Park", "Other Campuses"),
    State = c("Penn", "Non-Penn")))
    rownames(ctable) <- c("University Park", "Other Campuses")
    ctable
  } else {
    validate(
      need(is.numeric(input$nSamp1), is.numeric(input$nSamp2),
           message = "Please input sample size")
    )
    ctable <- matrix(c(
      percent(OneSample()[, 2] / input$nSamp1),
      percent(OneSample()[, 3] / input$nSamp2),
      percent(OneSample()[, 4] / input$nSamp1),
      percent(OneSample()[, 5] / input$nSamp2)
    ), ncol = 2,
    dimnames = list(Campus = c("University Park", "Other Campuses"),
    State = c("Penn", "Non-Penn")))
    rownames(ctable) <- c("University Park", "Other Campuses")
    ctable
  }
}, align = "c")

output$sampleinfotable2 = renderTable({
  if (input$tabset == "Same Sample Size") {
    validate(
      need(is.numeric(input$nSamp3),
           message = "Please input sample size")
    )
    ctable <- matrix(c(
      newOneSample()[, 2],
      newOneSample()[, 3],
      newOneSample()[, 4],
      newOneSample()[, 5]
    ), ncol = 2,
    dimnames = list(Campus = c("University Park", "Other Campuses"),
    State = c("Penn", "Non-Penn")))
    rownames(ctable) <- c("University Park", "Other Campuses")
    ctable
  } else {
    validate(
      need(is.numeric(input$nSamp1), is.numeric(input$nSamp2),
           message = "Please input sample size")
    )
    ctable <- matrix(c(
      OneSample()[, 2],
      OneSample()[, 3],
      OneSample()[, 4],
      OneSample()[, 5]
    ), ncol = 2,
    dimnames = list(Campus = c("University Park", "Other Campuses"),
    State = c("Penn", "Non-Penn")))
    rownames(ctable) <- c("University Park", "Other Campuses")
    ctable
  }
}, align = "c")

  
  
  
output$sampleinforatioUi <- renderUI(
  expr = {
  if (input$tabset == "Combined Sample Size") {
    validate(
      need(is.numeric(input$nSamp3),
           message = "Please input sample size")
    )
    cratio <- round(((newOneSample()[, 2]) * (newOneSample()[, 5]) / 
                       (newOneSample()[, 3] * newOneSample()[, 4])), 2)
    p(paste0("Sample Odds Ratio, =", cratio))
  } else {
    validate(
      need(is.numeric(input$nSamp1), is.numeric(input$nSamp2),
           message = "Please input sample size")
    )
    cratio <- round(((OneSample()[, 2]) * (OneSample()[, 5]) /
                       (OneSample()[, 3] * OneSample()[, 4])), 2)
    p(paste0("Sample Odds Ratio, =", cratio))
  }
})

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
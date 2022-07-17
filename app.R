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
# Load additional dependencies and setup functions
# source("global.R")

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
      title = "Odds Ratio", # You may use a shortened form of the title here
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Odds_Ratio")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ## Create the sidebar/left navigation menu ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Example", tabName = "example", icon = icon("book-reader")), 
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
          h1("Odds Ratio"), # This should be the full name.
          p(" This app explores confidence intervals for odds ratios and 
            their use in the meta-analysis of real data."),
          h2("Instructions"),
          tags$ol(
            tags$li("Click on the prerequisites button to review/learn how 
                    to calculate odds ratio"), 
            tags$li("Click the go button to enter the explore page."),
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
              inputId = "go1",
              label = "Go!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p("This app was developed and coded by Jingjun Wang and updated by 
            Shravani Samala and Junjie He. Special thanks to Neil Hatfield.",
            br(),
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 6/14/2022 by JJH.")
          )
        ),
        ## Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("What is odds ratio?"),
          p("An odds ratio relates the odds of an event under two different 
            conditions. 
            For example if two-thirds of women ate vegetables at lunch today 
            (odds of 2 to 1), while only one-third of men ate vegetables 
            (odds of 1 to 2)
            - then the odds for women are four times as great as the odds for
            men
            (so 4 is the odds ratio)."),
          h2("How to calculate the confidence interval of an odds ratio?"),
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
              ),
              alt = "fill me in later",
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
          p("As with the relative risk, the log-odds ratio \\(\\log\\hat
            {\\theta}\\)
            has a better normal approximation than \\(\\hat{\\theta}\\) does. 
            Therefore, we usually obtain a confidence interval on the log scale
            (log here means natural log). The estimated variance of \\(\\log\\hat
            {\\theta}\\)
            is easy to remember,"),
          p("\\[\\widehat{V}(\\log\\widehat{\\theta})=\\frac{1}{a}+\\frac{1}{b}+
             \\frac{1}{c}+\\frac{1}{d}\\]"),
          p("and we get a 95% confidence interval for \\(\\theta\\) by 
            exponentiating the endpoints of"),
          p("\\[\\log\\widehat{\\theta}\\pm1.96\\sqrt{\\frac{1}{a}+\\frac{1}{b}+
             \\frac{1}{c}+\\frac{1}{d}}\\]"),
          br(),
          h2("Example"),
          p("Here is the contingency table from a case-control study of smoking 
            and lung cancer:"),
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
                    tags$th("Cancer", style = "text-align: center;"),
                    tags$th("Non-Cancer", style = "text-align: center;"), 
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$th("Smoker", style = "text-align: center;"),
                    tags$td("647"),
                    tags$td("622"), 
                    align = "center"
                  ),
                  tags$tr(
                    tags$th("Non-Smoker",style = "text-align: center;"),
                    tags$td("2"),
                    tags$td("27"), 
                    align = "center"
                  ), 
                )
              ),
              alt = "fill me in later",
            )
          ),
          p("The odds of lung cancer for smokers is calculated as \\(\\frac{647}
            {622}= 1.04\\)."),
          p("The odds of lung cancer for non-smokers is \\(\\frac{2}{27}= 0.07\\).
            "),
          p("The ratio of the odds of lung cancer in smokers divided by the 
            odds of lung cancer in non-smokers: \\(\\frac{647}{622}\\big/\\frac{2}
            {27}=14.04\\)."), 
          p("Here, the odds ratio is greater than 1."),
          p("Being a smoker is considered to be associated with having lung 
            cancer since smoking raises the odds of having lung cancer."),
        ),
        
        ## Set up an Explore Page----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Residency Status Differences Between Campuses"),
          p("Below are the tables for the counts and percentages of enrollment by
            residency between University Park and the Commonwealth Campuses of 
            Penn State University. Use the slide controls to change the confidence
            level interval as well as the sample sizes taken from University Park
            and the Commonwealth campuses. Observe the difference between confidence
            intervals for each sample. Check below the plot to see the sample counts,
            percentages, and odds ratio."),
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
                    inputId = "dlevel",
                    "Confidence Level",
                    min = .10,
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
                    inputId = "dlevel1",
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
             for true odds ratio. Click on an interval (dot) to show the underlying
             data", style="text-align: center;"), 
             fluidRow(
               column(
                 width = 4, 
                 offset = 0,
                 h3(strong("Sample Counts:")),
                 span(tableOutput("sampleinfotable2"))
               ), 
               column(
                 width = 4,
                offset = 0,
                h3(strong("Sample Percentages:")),
                span(tableOutput("sampleinfotable1"))
               ), 
               column(
                 width = 4,
                 offset = 0,
                 h3(strong("Sample Odds Ratio:")),
                 span(textOutput("sampleinforatio"))
               )), 
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
                   tags$ul(tags$li("all datasets below are about the comparsion of  medical
                             therapy or vaccine effectiveness."),
                           tags$li("choose the dataset to see the odds ratio and related
                             95% confidence interval to check which thearpy or vaccine
                             is better.")),
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
                             "Vaccines Immunogenicity = vaccines"
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
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = "This app explores confidence intervals for odds ratios and their
              use in the meta-analysis of real data.",
      type = "info"
    )
  })
  
  observeEvent(input$go1,{
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "prerequisites")
  })
  
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
              h3("data context"),
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
                  alt = "fill me in later"
              ),
              h3("Second Graph : Gefitinib vs. Erlotinib"),
              img(
                src = "gve.PNG",
                height = "100%",
                width = "90%",
                algin = "middle",
                alt = "fill me in later"
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
              h3("data context"),
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
              h3("data context"),
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
              h3("third Graph : MMRV vs. MMR+V Against Rubella"),
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
  dalpha <- reactive({
    (1 - input$dlevel) / 2
  })
  
  ##Updating Sample Size----
  dN1 <- reactive({
    as.integer(input$nSamp1)
  })
  
  dN2 <- reactive({
    as.integer(input$nSamp2)
  })
  dN3 <- reactive({
    as.integer(input$nSamp3)
  })
  
  #generate 50 new sample----
  UPS50P <- reactive({
    input$newSample
    data.frame(
      x = do.call(paste0("rbinom"),
          c(list(n = dN1() * 50), list(1, 0.595)))
    ) %>%
      mutate(idx = rep(1:50, each = dN1()))
  })
  
  ups50p <- reactive({
    UPS50P() %>%
      group_by(idx) %>%
      summarise(
        Count1 = sum(x))
  })
  
  ups50n <- reactive({
    data.frame(idx = rep(1:50), 
               Count2 = input$nSamp1-ups50p()[,2])
  })
  
  UWS50P <- reactive({
    input$newSample
    data.frame(
      x = do.call(paste0("rbinom"),
          c(list(n = dN2() * 50), list(1, 0.844)))
    ) %>%
      mutate(idx = rep(1:50, each = dN2()))
  })
  
  uws50p <- reactive({
    UWS50P() %>%
      group_by(idx) %>%
      summarise(
        Count3 = sum(x))
  })
  
  uws50n <- reactive({
    data.frame(idx = rep(1:50), 
               input$nSamp2-uws50p()[,2])
  })
  
  data50_1 <- reactive({
    merge(ups50p(),uws50p())
  })
  
  data50_2 <- reactive({
    merge(ups50n(),uws50n())
  })
  
  data50 <- reactive({
    merge.data.frame(data50_1(), data50_2(), by = "idx")
  })
  
  ##generate 50 new sample (combined sample size)----
  UPS50P_3 <- reactive({
    input$newSample
    data.frame(
      x = do.call(
          paste0("rbinom"),
          c(list(n = dN3() * 50), list(1, 0.595)))
    ) %>%
      mutate(idx = rep(1:50, each = dN3()))
  })
  
  ups50p_3 <- reactive({
    UPS50P_3() %>%
      group_by(idx) %>%
      summarise(
        Count1 = sum(x))
  })
  
  ups50n_3 <- reactive({
    data.frame(idx = rep(1:50), 
               Count2 = input$nSamp3-ups50p_3()[,2])
  })
  
  UWS50P_3 <- reactive({
    input$newSample
    data.frame(
      x = do.call(
          paste0("rbinom"),
          c(list(n = dN3() * 50), list(1, 0.844)))
    ) %>%
      mutate(idx = rep(1:50, each = dN3()))
  })
  
  uws50p_3 <- reactive({
    UWS50P_3() %>%
      group_by(idx) %>%
      summarise(
        Count3 = sum(x))
  })
  
  uws50n_3 <- reactive({
    data.frame(idx = rep(1:50), 
               input$nSamp3-uws50p_3()[,2])
  })
  
  data50_1_3 <- reactive({
    merge(ups50p_3(),uws50p_3())
  })
  
  data50_2_3 <- reactive({
    merge(ups50n_3(),uws50n_3())
  })
  
  newdata50 <- reactive({
    merge.data.frame(data50_1_3(),data50_2_3(), by = "idx")
  })
  
  ##calculate the interval----
  Intervals <- reactive({
    zvalue = qnorm(((1 - input$dlevel)/2), lower.tail = F)
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
  })
  
  newIntervals <- reactive({
    zvalue = qnorm(((1 - input$dlevel1)/2), lower.tail = F)
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
  })
  
  ##default as all the samples are selected----
  selected_sample <- 50
  selectedSample <- reactive({
    if (!is.null(input$plot_click)) {
      selected_sample <<- round(input$plot_click$y)
      if (selected_sample < 1) selected_sample <<- 1
      if (selected_sample > 50) selected_sample <<- 50
    }
    selected_sample
  })
  
  # selected sample----
  OneSample <- reactive({
    data50() %>%
      filter( idx == selectedSample() )
  })
  
  OneSampleColor <- reactive({
    colors <- c("TRUE" = "#ff7532", "FALSE" = "red")
    covers <- (Intervals() %>% filter(idx == selectedSample()) )$cover
    colors[ as.character(covers) ]
  })
  
  # selected sample (combined version) 
  newOneSample <- reactive({
    newdata50() %>%
      filter( idx == selectedSample() )
  })
  
  newOneSampleColor <- reactive({
    colors <- c("TRUE" = "#ff7532", "FALSE" = "red")
    covers <- (newIntervals() %>% filter(idx == selectedSample()) )$cover
    colors[ as.character(covers) ]
  })
  
  ##print the CIplot----
  output$CIplot <- renderPlot({
    if (input$tabset == "Same Sample Size"){
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
        theme_bw()+
        geom_hline(yintercept = 1, size = 1.8, colour = "#000000", alpha = 1) +
        geom_hline(yintercept = .35, size = 1.8, colour = "#0B6623", alpha = 1) +
        coord_flip() +
        scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = .8), guide = FALSE) +
        scale_color_manual(values = c("FALSE" = "#BC204B", "TRUE" = "#1E407C"), guide = FALSE) +
        scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .5), guide = FALSE) +
        lims(y = c(-0.01,4.55)) +
        labs(title = paste0(100 * input$dlevel1, "% Confidence Intervals"),
             x="",y="") +
        theme(legend.position = "none",
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(size = 18),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14))
    }
    else{
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
        theme_bw()+
        geom_hline(yintercept = 1, size = 1.8, colour = "#000000", alpha = 1) +
        geom_hline(yintercept = .35, size = 1.8, colour = "#0B6623", alpha = 1) +
        coord_flip() +
        scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = .8), guide = FALSE) +
        scale_color_manual(values = c("FALSE" = "#BC204B", "TRUE" = "#1E407C"), guide = FALSE) +
        scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .5), guide = FALSE) +
        lims(y = c(-0.01,4.55)) +
        labs(title = paste0(100 * input$dlevel, "% Confidence Intervals"),
             x = "",y = "") +
        theme(legend.position = "none",
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(size = 18),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14))
    }
  },    
  alt = "fill me in later",
  )
  
  
  ## sample display----
  output$sampleinfotable1 = renderTable({
    if (input$tabset == "Same Sample Size"){
      validate(
        need(is.numeric(input$nSamp3),
             message = "Please input sample size")
      )
      ctable <- matrix(c(percent(newOneSample()[,2]/input$nSamp3), 
                         percent(newOneSample()[,3]/input$nSamp3), 
                         percent(newOneSample()[,4]/input$nSamp3), 
                         percent(newOneSample()[,5]/input$nSamp3)), ncol = 2, 
                       dimnames = list(Campus = c("University Park","Other Campuses"), 
                                       State = c("Penn", "Non-Penn")))
      rownames(ctable) = c("University Park","Other Campuses")
      ctable
    }
    else{
      validate(
        need(is.numeric(input$nSamp1),is.numeric(input$nSamp2),
             message = "Please input sample size")
      )
      ctable <- matrix(c(percent(OneSample()[,2]/input$nSamp1), 
                         percent(OneSample()[,3]/input$nSamp2), 
                         percent(OneSample()[,4]/input$nSamp1), 
                         percent(OneSample()[,5]/input$nSamp2)), ncol = 2, 
                       dimnames = list(Campus = c("University Park","Other Campuses"), 
                                       State = c("Penn", "Non-Penn")))
      rownames(ctable) = c("University Park","Other Campuses")
      ctable
    }
  })
  output$sampleinfotable2 = renderTable({
    if (input$tabset == "Same Sample Size"){
      validate(
        need(is.numeric(input$nSamp3),
             message = "Please input sample size")
      )
      ctable <- matrix(c(newOneSample()[,2], 
                         newOneSample()[,3],
                         newOneSample()[,4],
                         newOneSample()[,5]), ncol = 2, 
                       dimnames = list(Campus = c("University Park","Other Campuses"), 
                                       State = c("Penn", "Non-Penn")))
      rownames(ctable) = c("University Park","Other Campuses")
      ctable
    }
    else{
      validate(
        need(is.numeric(input$nSamp1),is.numeric(input$nSamp2),
             message = "Please input sample size")
      )
      ctable <- matrix(c(OneSample()[,2],
                         OneSample()[,3],
                         OneSample()[,4],
                         OneSample()[,5]), ncol = 2, 
                       dimnames = list(Campus = c("University Park","Other Campuses"),
                                       State = c("Penn", "Non-Penn")))
      rownames(ctable) = c("University Park","Other Campuses")
      ctable
    }
  })
  
  output$sampleinforatio = renderText({
    if (input$tabset == "Combined Sample Size"){
      validate(
        need(is.numeric(input$nSamp3),
             message = "Please input sample size")
      )
      cratio <- round(((newOneSample()[,2])*(newOneSample()[,5]) / 
                         (newOneSample()[,3]*newOneSample()[,4])), 2)
      cratio
    }
    else{
      validate(
        need(is.numeric(input$nSamp1),is.numeric(input$nSamp2),
             message = "Please input sample size")
      )
      cratio <- round(((OneSample()[,2])*(OneSample()[,5]) /
                         (OneSample()[,3]*OneSample()[,4])), 2)
      cratio
    }
  })
  
  # ##forestplot----
  # makeDatatabletoList <- function(mytable){
  #   mylist = c()
  #   rnum <- nrow(mytable)
  #   for (i in 1:rnum){
  #     mylist[[i]] = matrix(as.numeric(mytable[i,]),nrow = 2,byrow = TRUE)
  #   }
  #   mylist
  # }
  # 
  # makeTable <- function(mylist, referencerow=2)
  # {
  #   require("rmeta")
  #   numstrata <- length(mylist)
  #   ntrt <- vector()
  #   nctrl <- vector()
  #   ptrt <- vector()
  #   pctrl <- vector()
  #   if (referencerow == 1) {nonreferencerow <- 2}
  #   else {nonreferencerow <- 1}
  #   for (i in 1:numstrata)
  #   {
  #     mymatrix <- mylist[[i]]
  #     DiseaseUnexposed <- mymatrix[referencerow,1]
  #     ControlUnexposed <- mymatrix[referencerow,2]
  #     totUnexposed <- DiseaseUnexposed + ControlUnexposed
  #     nctrl[i] <- totUnexposed
  #     pctrl[i] <- DiseaseUnexposed
  #     DiseaseExposed <- mymatrix[nonreferencerow,1]
  #     ControlExposed <- mymatrix[nonreferencerow,2]
  #     totExposed <- DiseaseExposed + ControlExposed
  #     ntrt[i] <- totExposed
  #     ptrt[i] <- DiseaseExposed
  #   }
  #   names <- as.character(seq(1,numstrata))
  #   myMH <- meta.MH(ntrt, nctrl, ptrt, pctrl, conf.level = 0.95, 
  #                   names = names,
  #                   statistic = "OR")
  #   
  #   
  #   tabletext <- cbind(c("","Study",myMH$names,NA,"Summary"),
  #                      c("Treatment","(effective)",ptrt,NA,NA),
  #                      c("Treatment","(non-effective)",pctrl, NA,NA),
  #                      c("Control","(effective)",(ntrt-ptrt),NA,NA),
  #                      c("Control","(non-effective)",(nctrl-pctrl), NA,NA),
  #                      c("","OR",format((exp(myMH$logOR)),digits = 3),NA,
  #                        format((exp(myMH$logMH)),digits=3)))
  # }
  # 
  # 
  # makeForestPlot <- function(mylist, referencerow=2)
  # {
  #   require("rmeta")
  #   numstrata <- length(mylist)
  #   ntrt <- vector()
  #   nctrl <- vector()
  #   ptrt <- vector()
  #   pctrl <- vector()
  #   if (referencerow == 1) {nonreferencerow <- 2}
  #   else {nonreferencerow <- 1}
  #   for (i in 1:numstrata)
  #   {
  #     mymatrix <- mylist[[i]]
  #     DiseaseUnexposed <- mymatrix[referencerow,1]
  #     ControlUnexposed <- mymatrix[referencerow,2]
  #     totUnexposed <- DiseaseUnexposed + ControlUnexposed
  #     nctrl[i] <- totUnexposed
  #     pctrl[i] <- DiseaseUnexposed
  #     DiseaseExposed <- mymatrix[nonreferencerow,1]
  #     ControlExposed <- mymatrix[nonreferencerow,2]
  #     totExposed <- DiseaseExposed + ControlExposed
  #     ntrt[i] <- totExposed
  #     ptrt[i] <- DiseaseExposed
  #   }
  #   names <- as.character(seq(1,numstrata))
  #   myMH <- meta.MH(ntrt, nctrl, ptrt, pctrl, conf.level = 0.95, 
  #                   names = names,
  #                   statistic = "OR")
  # }
  
  ## Non-Small Cell Lung Cancer Treatment introduction
  # output$nsclc = renderText("About 80% to 85% of lung cancers are non-small cell
  #                           lung cancer (NSCLC). The typical treatments include
  #                           chemotherapy, radiation therapy and targeted therapy.
  #                           Gefitinib and Erlotinib are two kinds of medicine used
  #                           in NSCLC targeted therapy. In the two comparisons,
  #                           Gefitinib represents treatment groups.")
  # 
  # # drug 1: Gefitinib vs chemotherapy
  # gvc1 <- matrix(c(42,37,48,53),nrow = 2,byrow = TRUE)
  # gvc2 <- matrix(c(18,12,26,32),nrow = 2,byrow = TRUE)
  # gvc_list = list(gvc1, gvc2)
  # 
  # # makeForestPlotForRCTs(gvc_list)
  # output$plot1 = renderUI({
  #   img(src = "gvc.PNG", width = "80%", algin = "middle")
  # })
  # 
  # # drug 2: Gefitinib vs Erlotinib 
  # gve1 <- matrix(c(28,6,22,12),nrow = 2,byrow = TRUE)
  # gve2 <- matrix(c(36,14,38,12),nrow = 2,byrow = TRUE)
  # gve3 <- matrix(c(16,19,21,14),nrow = 2,byrow = TRUE)
  # gve_list = list(gve1, gve2, gve3)
  # 
  # #  makeForestPlot(gve_list)
  # output$plot2 = renderUI({
  #   img(src = "gve.PNG", width = "80%", algin = "middle",
  # alt = "fill me in later")
  # })
  # 
  ## Malaria Treatment
  # output$mala = renderText("Artemisinin is a plant-derived compound, isolated from
  #                          the Artemisia annua, sweet wormwood a herb employed in
  #                          Chinese herbal medicine. This compound (along with its
  #                          derivative drugs), is the World Health Organization's
  #                          recommended treatment against malaria caused by Plasmodium
  #                          falciparum. Quinine, isolated from cinchona bark, is
  #                          the first meditation to treat malaria. In the two
  #                          comparisons, artesunate-based therapies represent
  #                          treatment groups.")
  # 
  # 
  # #drug 1: Artemisinin-based combination therapies vs. Quinine
  # avq1 <- matrix(c(37,26,2,15),nrow = 2,byrow = TRUE)
  # avq2 <- matrix(c(64,34,2,8),nrow = 2,byrow = TRUE)
  # avq3 <- matrix(c(137,122,1,3),nrow = 2,byrow = TRUE)
  # avq_list = list(avq1, avq2, avq3)
  # output$plot3 = renderUI({
  #   img(src = "avq.PNG", width = "85%", algin = "middle",
  # alt = "fill me in later")
  # })
  # 
  # 
  # #drug 2: artemether vs. Quinine
  # amvq1 <- matrix(c(6,10,45,42),nrow = 2,byrow = TRUE)
  # amvq2 <- matrix(c(18,8,71,63),nrow = 2,byrow = TRUE)
  # amvq3 <- matrix(c(11,14,43,35),nrow = 2,byrow = TRUE)
  # amvq4 <- matrix(c(1,2,17,17),nrow = 2,byrow = TRUE)
  # amvq5 <- matrix(c(10,12,73,69),nrow = 2,byrow = TRUE)
  # amvq6 <- matrix(c(59,62,229,226),nrow = 2,byrow = TRUE)
  # amvq7 <- matrix(c(3,2,35,37),nrow = 2,byrow = TRUE)
  # amvq_list = list(amvq1, amvq2, amvq3, amvq4, amvq5, amvq6, amvq7)
  # output$plot4 = renderUI({
  #   img(src = "amvq.PNG", width = "85%", algin = "middle",
  # alt = "fill me in later")
  # })
  # 
  # 
  ## Vaccines Immunogenicity
  # output$vacc = renderText("A combined measles-mumps-rubella-varicella (MMRV)
  #                          vaccine is expected to facilitate universal immunization
  #                          against these 4 diseases. Here randomized controlled
  #                          trials (RCTs) were conducted to compare single MMRV
  #                          dose with measles-mumps-rubella vaccine with varicella
  #                          vaccine (MMR + V). All included studies reported
  #                          seroconversion rate as serological response outcome.
  #                          Seroconversion rate was defined as percent of subjects
  #                          initially seronegative (with titers \u2264 assay cut-offs),
  #                          who developed postvaccination antibody titers above the
  #                          assay cut-off levels. In the three comparisons, MMRV
  #                          represents treatment groups.")
  # #1: measles
  # mea1 <- matrix(c(289,141,4,1),nrow = 2,byrow = TRUE)
  # mea2 <- matrix(c(1107,540,9,15),nrow = 2,byrow = TRUE)
  # mea3 <- matrix(c(73,68,1,6),nrow = 2,byrow = TRUE)
  # mea4 <- matrix(c(1114,181,29,9),nrow = 2,byrow = TRUE)
  # mea12 <- matrix(c(290,145,12,0),nrow = 2,byrow = TRUE)#contain 0
  # mea5 <- matrix(c(980,349,9,1),nrow = 2,byrow = TRUE)
  # mea6 <- matrix(c(299,106,7,0),nrow = 2,byrow = TRUE)#contain 0
  # mea7 <- matrix(c(2437,841,72,20),nrow = 2,byrow = TRUE)
  # mea8 <- matrix(c(125,113,9,9),nrow = 2,byrow = TRUE)
  # mea9 <- matrix(c(10,7,0,1),nrow = 2,byrow = TRUE)#contain 0
  # mea10 <- matrix(c(633,199,37,14),nrow = 2,byrow = TRUE)
  # mea11 <- matrix(c(294,155,6,1),nrow = 2,byrow = TRUE)
  # mea_list = list(mea1, mea2, mea3, mea4, mea5, mea7, mea8, mea10, mea11)
  # output$plot5 = renderUI({
  #   img(src = "mea.PNG", width = "90%", algin = "middle",
  # alt = "fill me in later")
  # })
  # 
  # #2: mumps
  # mum1 <- matrix(c(287,137,12,4),nrow = 2,byrow = TRUE)
  # mum2 <- matrix(c(927,516,167,28),nrow = 2,byrow = TRUE)
  # mum3 <- matrix(c(70,69,2,4),nrow = 2,byrow = TRUE)
  # mum4 <- matrix(c(992,173,117,9),nrow = 2,byrow = TRUE)
  # mum5 <- matrix(c(292,148,3,2),nrow = 2,byrow = TRUE)
  # mum6 <- matrix(c(1002,350,10,1),nrow = 2,byrow = TRUE)
  # mum7 <- matrix(c(272,101,30,4),nrow = 2,byrow = TRUE)
  # mum8 <- matrix(c(2409,854,100,18),nrow = 2,byrow = TRUE)
  # mum9 <- matrix(c(113,108,20,10),nrow = 2,byrow = TRUE)
  # mum10 <- matrix(c(10,8,0,0),nrow = 2,byrow = TRUE)#contain 0
  # mum11 <- matrix(c(613,191,37,16),nrow = 2,byrow = TRUE)
  # mum12 <- matrix(c(262,145,33,9),nrow = 2,byrow = TRUE)
  # mum_list = list(mum1, mum2, mum3, mum4, mum5, mum6, mum7, mum8, mum9, mum11, 
  #                 mum12)
  # 
  # output$plot6 = renderUI({
  #   img(src = "mum.PNG", width = "90%", algin = "middle", 
  # alt = "fill me in later")
  # })
  # 
  # #3: rubella
  # rub1 <- matrix(c(288,141,10,0),nrow = 2,byrow = TRUE)#contain 0
  # rub2 <- matrix(c(1114,552,3,3),nrow = 2,byrow = TRUE)
  # rub3 <- matrix(c(73,74,1,0),nrow = 2,byrow = TRUE)#contain 0
  # rub4 <- matrix(c(1148,189,1,0),nrow = 2,byrow = TRUE)#contain 0
  # rub5 <- matrix(c(289,142,15,11),nrow = 2,byrow = TRUE)
  # rub6 <- matrix(c(1004,352,11,4),nrow = 2,byrow = TRUE)
  # rub7 <- matrix(c(303,106,3,0),nrow = 2,byrow = TRUE)#contain 0
  # rub8 <- matrix(c(2501,859,31,7),nrow = 2,byrow = TRUE)
  # rub9 <- matrix(c(113,108,20,10),nrow = 2,byrow = TRUE)#contain 0
  # rub10 <- matrix(c(10,8,0,0),nrow = 2,byrow = TRUE)#contain 0
  # rub11 <- matrix(c(665,208,2,4),nrow = 2,byrow = TRUE)
  # rub12 <- matrix(c(297,157,1,0),nrow = 2,byrow = TRUE)#contain 0
  # rub_list = list(rub2,rub5, rub6, rub8, rub11)
  # output$plot7 = renderUI({
  #   img(src = "rub.PNG", width = "85%", algin = "middle",
  # alt = "fill me in later")
  # })
 
  ##the code upper I don't know what they are aiming, but might be related to the
  ##plots that should be made in the example page. I comment out them. 
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)

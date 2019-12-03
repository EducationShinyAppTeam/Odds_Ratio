library(shiny)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(shinyDND)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(leaps)
library(ggplot2)
library(markdown)
# insert "convertMentItem" function

convertMenuItem <- function(mi, tabName) {
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if (length(mi$attribs$class) > 0 && mi$attribs$class == "treeview") {
    mi$attribs$class = NULL
  }
  mi
}


# Creat the UI
shinyUI(fluidPage(
  dashboardPage(
    skin = "black",
    
    #header
    dashboardHeader(
      title = "Odds Ratio",
      titleWidth = 180,
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/', icon("home"))
      ),
      tags$li(class = "dropdown", actionLink("info", icon("info", class =
                                                            "myClass")))
    ),
    
    #menu bar
    dashboardSidebar(
      width = 180,
      sidebarMenu(
        id = 'tabs',
        style = 'font-size:13px;',
        convertMenuItem(
          menuItem(
            "Overview",
            tabName = "instruction",
            icon = icon("dashboard"),
            menuSubItem("Prerequisites", tabName = "prereq", icon =
                          icon("book"))
          ),
          'instruction'
        ),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem(
          "Real Data Analysis",
          tabName = "analysis",
          icon = icon("cogs")
        )
      )
    ),
    
    #change the color,bacground color & word styles of buttons, icons & words
    dashboardBody(
      #change header font
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
      ),
      
      #change icon color
      tags$head(
        tags$style(".fa-home {color:#ffffff}"),
        tags$style(".fa-info {color:#ffffff}")
      ),
      
      #change button color
      tags$head(
        tags$style(HTML(
          '#start1{color:white;background-color: #ff5300}'
        )),
        tags$style(HTML(
          '#go{color:white;background-color: #ff5300}'
        )),
        tags$style(HTML(
          '#pre{color:white;background-color: #ff5300}'
        )),
        tags$style(HTML(
          '#analysis{color:white;background-color: #ff5300}'
        ))
      ),
      
      
      #change header color
      tags$style(
        HTML(
          '
          .skin-black .main-header>.navbar {
          background-color: #ff5300 ;
          }
          
          .content-wrapper,.right-side {
          background-color: white;
          }
          
          .skin-black .main-header .logo {
          background-color: #ff5300  ;
          color: white;
          }
          .skin-black .main-header .logo:hover {
          background-color: #ff5300;
          }
          .skin-black .main-header .navbar .sidebar-toggle:hover{
          background-color: #ff5300;
          }'
)
        ),


tabItems(
  # instruction page
  tabItem(
    tabName = "instruction",
    tags$a(href = 'http://stat.psu.edu/', tags$img(
      src = 'logo.png', align = "left", width = 180
    )),
    br(),
    br(),
    br(),
    h3(strong("About:")),
    p(
      'This app explores confidence intervals for odds ratios and their use in the meta-analysis of real data.'
    ),
    br(),
    div(
      style = "text-align: center",
      actionButton(
        "pre",
        "Prerequisite",
        icon("bolt"),
        style = 'padding:10px; font-size:100%',
        class = "circle grow"
      )
    ),
    br(),
    h3(strong('Instructions:')),
    tags$ul(
      tags$li('Click the explore button to enter the explore page. '),
      tags$li(
        'Explore either the equal sample size or different sample size situation. Then use the slider bars to change the confidence level or sample size(s).'
      ),
      tags$li(
        'After working with the explore section, have a look at the summaries of real data. Note that each line of data represents a different individual experiment.'
      )
    ),
    
    
    div(
      style = "text-align: center",
      actionButton("go", "Explore", icon("bolt"), class =
                     "circle grow")
    ),
    br(),
    h3(strong('Acknowledgements:')),
    p("This app was developed and coded by Jingjun Wang.")
  ),
  
  # pre-requisite page
  tabItem(
    tabName = "prereq",
    withMathJax(),
    h3(strong('Background')),
    h3('What is odds ratio?'),
    p(
      'An odds ratio relates the odds of an event under two different conditions. For example if two-thirds of women ate vegetables at lunch today (odds of 2 to 1), while only one-third of men ate vegetables (odds of 1 to 2) - then the odds for women are four times as great as the odds for men (so 4 is the odds ratio).'
    ),
    h3('How to calculate the confidence interval of an odds ratio?'),
    tags$img(src = 'newtable.PNG', width =
               "30%"),
    p(
      "The natural estimator of \\(\\theta\\) is the sample cross-product ratio, \\(\\widehat{\\theta}=\\frac{ad}{bc}\\)"
    ),
    p(
      "The properties of \\(\\hat{\\theta}\\) are easily established under multinomial sampling, but the same properties will hold under Poisson or product-multinomial sampling with either the row totals or column totals (but not both) regarded as fixed."
    ),
    p(
      "As with the relative risk, the log-odds ratio \\(\\log\\hat{\\theta}\\) has a better normal approximation than \\(\\hat{\\theta}\\) does. Therefore, we usually obtain a confidence interval on the log scale (log here means natural log). The estimated variance of \\(\\log\\hat{\\theta}\\) is easy to remember,"
    ),
    p((
      "\\(\\widehat{V}(\\log\\widehat{\\theta})=\\frac{1}{a}+\\frac{1}{b}+\\frac{1}{c}+\\frac{1}{d}\\)"
    ),
    style = "text-align: center"
    ),
    p(
      "and we get a 95% confidence interval for \\(\\theta\\) by exponentiating the endpoints of"
    ),
    p((
      "\\(\\log\\widehat{\\theta}\\pm1.96\\sqrt{\\frac{1}{a}+\\frac{1}{b}+\\frac{1}{c}+\\frac{1}{d}}\\)"
    ),
    style = "text-align: center"
    ),
    br(),
    h3('Example'),
    p(
      'Here is the contingency table from a case-control study of smoking and lung cancer:'
    ),
    br(),
    tags$img(src = 'sample_question.PNG', width =
               "30%"),
    br(),
    br(),
    p(
      "The odds of lung cancer for smokers is calculated as \\(\\frac{647}{662}= 1.04\\) "
    ),
    br(),
    p(
      'The odds of lung cancer for non-smokers is \\(\\frac{2}{27}= 0.07\\) .'
    ),
    br(),
    p(
      'It is the ratio of the odds of lung cancer in smokers divided by the odds of lung cancer in non-smokers: \\(\\frac{\\frac{647}{662}}{\\frac{2}{27}}= 14.04\\). '
    ),
    br(),
    p('Here, the odds ratio is greater than 1.'),
    br(),
    p(
      'Being a smoker is considered to be associated with having lung cancer since smoking raises the odds of having lung cancer'
    ),
    br(),
    div(
      style = "text-align: center",
      actionButton(
        "start1",
        "Go to the overview",
        icon("bolt"),
        style = 'padding:10px; font-size:100%',
        class = "circle grow"
      )
    )
  ),
  
  # explore page
  
  tabItem(
    tabName = "explore",
    titlePanel(
      strong(
        "Odds Ratio for Enrollment by Residency between University Park and Commonwealth Campuses"
      )
    ),
    sidebarLayout(
      sidebarPanel(
        h3(strong("True Population:")),
        h4("Count: "),
        img(
          src = "2016Count.PNG",
          height = "100%",
          width = "90%",
          algin = "middle"
        ),
        h4("Percentage: "),
        img(
          src = "2016Diff.PNG",
          height = "100%",
          width = "90%",
          algin = "middle"
        ),
        br(),
        br(),
        (p(("Odds of Pennsylvania residents for University Park:       1.47 "),
           style = "white-space: pre-wrap"
        )),
        (p(("Odds of Pennsylvania residents for other campuses:     5.42 "),
           style = "white-space: pre-wrap"
        )),
        (p(("Odds ratio (θ) :    \\(\\frac{1.47}{5.42}= 0.27\\)"),
           style = "white-space: pre-wrap"
        )),
        
        br(),
        
        tags$style(
          HTML(
            " .tabbable > .nav > li > a                  {background-color: white; color:#ff7532}
            .tabbable > .nav > li[class=active] > a {background-color: #ff7532; color: white;}
            "
          )
          ),
        h3(strong("Sliders: ")),
        tabsetPanel(
          id = "tabset",
          tabPanel(
            "Seperate Sample Sizes",
            fluid = TRUE,
            
            br(),
            tags$style(
              HTML(
                ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #ff864c}"
              )
            ),
            tags$style(
              HTML(
                ".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #ff864c}"
              )
            ),
            tags$style(
              HTML(
                ".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #ff864c}"
              )
            ),
            sliderInput(
              "dlevel",
              "Confidence Level",
              min =
                .10,
              max = 0.99,
              value = 0.95,
              step = 0.01
            ),
            
            sliderInput(
              "nSamp1",
              "Sample Size for University Park",
              min =
                30,
              max = 200,
              value = 50,
              step = 5
            ),
            sliderInput(
              "nSamp2",
              "Sample Size for Other Campuses",
              min =
                30,
              max = 200,
              value = 50,
              step = 5
            )
            
            
          ),
          tabPanel(
            "Same Sample Size",
            fluid = TRUE,
            
            br(),
            tags$style(
              HTML(
                ".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #ff864c}"
              )
            ),
            tags$style(
              HTML(
                ".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #ff864c}"
              )
            ),
            
            sliderInput(
              "dlevel1",
              "Confidence Level",
              min =
                .10,
              max = 0.99,
              value = 0.95,
              step = 0.01
            ),
            
            sliderInput(
              "nSamp3",
              "Sample Sizes for both University Park and Other Campuses",
              min =
                30,
              max = 200,
              value = 50,
              step = 5
            )
            
          )
        )
        
        
          ),
      mainPanel(
        sidebarLayout(
          mainPanel(
            plotOutput("CIplot", height = "600px", click = "plot_click"),
            bsPopover(
              "CIplot",
              "Confidence Interval Plot",
              "The orange lines indicate a confidence interval that smaller or greater than 1 and the purple lines indicate confidence intervas containing 1. Click on an interval to see detailed information on the right-hand side for the chosen sample.",
              trigger =
                "hover",
              placement = "bottom"
            )
          ),
          sidebarPanel(
            h3(strong("Sample Counts:")),
            span(tableOutput("sampleinfotable2"), style =
                   "font-size: 18px"),
            h3(strong("Sample Percentages:")),
            span(tableOutput("sampleinfotable1"), style =
                   "font-size: 18px"),
            h3(strong("Sample Odds Ratio:")),
            span(textOutput("sampleinforatio"), style =
                   "font-size: 18px")
          )
        ),
        br(),
        actionButton("newSample", "Generate 50 New Samples", icon("retweet"),
                     style = "color: white; background-color: #ff7532"),
        bsPopover(
          "newSample",
          "Note",
          "By clicking on this button, new sample with the size you input will be generated.",
          trigger =
            "hover",
          placement = "bottom"
        )
        
      )
      )
    
    
),

#analysis page
tabItem(tabName = "analysis",
        sidebarLayout(
          sidebarPanel(
            tags$style(
              type = 'text/css',
              ".selectize-input { font-size: 18px; line-height: 18px;} .selectize-dropdown { font-size: 19px; line-height: 19px; }"
            ),
            h3(strong("Choose a Dataset Below")),
            selectInput(
              "sets",
              NULL,
              list(
                "Non-Small Cell Lung Cancer Treatment" =
                  c(
                    "Gefitinib vs. Chemotherapy" = "gvc",
                    "Gefitinib vs. Erlotinib" = "gve"
                  ),
                "Malaria Treatment" =
                  c(
                    "Artesunate-based therapies vs. Quinine (for uncomplicated malaria)" = "avq",
                    'Artemether vs. Quinine (for cerebral malaria)' = 'amvq'
                  ),
                "Vaccines Immunogenicity" =
                  c(
                    "MMRV vs. MMR+V Against Measles" = "mea",
                    "MMRV vs. MMR+V Against Mumps" =
                      "mum",
                    "MMRV vs. MMR+V Against Rubella" =
                      "rub"
                  )
                
                
              ),
              width = validateCssUnit("70%")
            ),
            useShinyjs(),
            conditionalPanel(
              condition = "input.sets== 'gve'",
              div(style="display: inline-block;vertical-align:top; width: 200px;", 
                  actionButton("comments1", "Comments", style="color: #fff; background-color: #9874e3"))
            ),
            conditionalPanel(
              condition = "input.sets== 'amvq'",
              div(style="display: inline-block;vertical-align:top; width: 200px;", 
                  actionButton("comments2", "Comments", style="color: #fff; background-color: #9874e3"))
            ),
            conditionalPanel(
              condition = "input.sets== 'rub'",
              div(style="display: inline-block;vertical-align:top; width: 200px;", 
                  actionButton("comments3", "Comments", style="color: #fff; background-color: #9874e3"))
              
            ),
            
            h3(strong("Background Knowledge")),
            conditionalPanel(
              condition = "input.sets== 'gvc'|input.sets== 'gve'",
              tags$style("#nsclc{ font-size: 20px; }"),
              textOutput("nsclc")
            ),
            conditionalPanel(
              condition = "input.sets == 'avq'|input.sets== 'amvq'",
              tags$style("#mala{ font-size: 20px; }"),
              textOutput("mala")
            ),
            conditionalPanel(
              condition = "input.sets == 'mea'|input.sets== 'mum'|input.sets== 'rub'",
              tags$style("#vacc{ font-size: 20px; }"),
              textOutput("vacc")
              
            )
            
          ),
          
          mainPanel( 
            conditionalPanel(
              condition = "input.sets == 'gvc'",
              h3(p(
                strong("Gefitinib vs. Chemotherapy"),
                style = "text-align: center"
              )),
              h4(p(("Those two studies come from two individual studies."),
                   style = "text-align: center"
              )),
              img(src="gvc.PNG",
                  height = "100%", 
                  width = "90%",
                  algin = "middle"
              )
              
            ),
            conditionalPanel(
              condition = "input.sets == 'gve'",
              h3(p(strong(
                "Gefitinib vs. Erlotinib"
              ),
              style = "text-align: center")),
              h4(p(("Those three studies come from three individual studies."),
                   style = "text-align: center"
              )),
              img(
                src = "gve.PNG",
                height = "100%",
                width = "90%",
                algin = "middle"
              ),
              shinyjs::hidden(
                wellPanel(id = "nsclc_comments",
                          HTML(markdownToHTML(fragment.only=TRUE, 
                                              text=c(
                                                "The first analysis in this section generally compares the effect of targeted therapy and chemotherapy. `Summary OR = 1.4` which is greater than 1. However, the CI of the `Summary OR` contains 1. So we fail to reject that the effectiveness of the two ways of treatments is about equal in this case.
                                                
Then the second comparison as shown above is between two medicine within targeted therapy treatment. This time, `Summary OR = 0.961`. However, the CI of the `Summary OR` contains 1. So we fail to reject that the effectiveness of the two medicine is about equal in this case.
                                                
In two analyses, we both fail to reject the null. However, targeted therapy in general is better than chemotherapy."
                                              )))
                                              )
                                              )
                                              ),
            conditionalPanel(
              condition = "input.sets == 'avq'",
              h3(p(
                strong("Artesunate-based therapies vs. Quinine"),
                style = "text-align: center"
              )),
              h4(p(
                strong("(uncomplicated malaria in pregnancy)"),
                style = "text-align: center"
              )),
              h4(p(("Those three studies come from three individual studies."),
                   style = "text-align: center"
              )),
              img(
                src = "avq.PNG",
                height = "100%",
                width = "90%",
                algin = "middle"
              )
            ),
            conditionalPanel(
              condition = "input.sets == 'amvq'",
              h3(p(strong(
                "Artemether vs. Quinine"
              ),
              style = "text-align: center")),
              h4(p(
                strong("(cerebral malaria in African children \u2264 15 years of age)"),
                style = "text-align: center"
              )),
              h4(p(("Those seven studies come from seven individual studies."),
                   style = "text-align: center"
              )),
              img(
                src = "amvq.PNG",
                height = "100%",
                width = "90%",
                algin = "middle"
              ),
              shinyjs::hidden(
                wellPanel(id = "mala_comments",
                          HTML(markdownToHTML(fragment.only=TRUE, 
                                              text=c(
                                                "The first analysis in this section compares the effect of artesunate-based therapies and quinine in treating uncomplicated malaria in pregnancy. Although we only have data from three studies, the advantage of using artesunate-based therapies is obvious: `Summary OR = 7.59` which is greater than 1. Then the second analysis as shown above is between artemether and quinine in treating cerebral malaria in African children less than 15 years of age. This time, `Summary OR = 0.933`. However, the CI of the `Summary OR` contains 1. So the effectiveness of the two medicine is about equal in this case."
                                              )))
                                              )
                                              )
                ),
            conditionalPanel(
              condition = "input.sets == 'mea'",
              h3(p(
                strong("MMRV vs. MMR+V Against Measles"),
                style = "text-align: center"
              )),
              h4(p(("Those nine studies come from nine individual studies."),
                   style = "text-align: center"
              )),
              img(
                src = "mea.PNG",
                height = "100%",
                width = "90%",
                algin = "middle"
              )
            ),
            conditionalPanel(
              condition = "input.sets == 'mum'",
              h3(p(
                strong("MMRV vs. MMR+V Against Mumps"),
                style = "text-align: center"
              )),
              h4(p(("Those eleven studies come from eleven individual studies."),
                   style = "text-align: center"
              )),
              img(
                src = "mum.PNG",
                height = "100%",
                width = "90%",
                algin = "middle"
              )
            ),
            conditionalPanel(
              condition = "input.sets == 'rub'",
              h3(p(strong(
                "MMRV vs. MMR+V Against Rubella"
              ),
              style = "text-align: center")),
              h4(p(("Those five studies come from five individual studies."),
                   style = "text-align: center"
              )),
              img(
                src = "rub.PNG",
                height = "100%",
                width = "90%",
                algin = "middle"
              ),
              shinyjs::hidden(
                wellPanel(id = "vacc_comments",
                          HTML(markdownToHTML(fragment.only=TRUE, 
                                              text=c(
                                                "The three analyses in this section compare the MMRV vaccine and the MMR + V vaccine in preventing measles, mumps, and rubella. Intuitively, we would assume that the effectiveness of the two kinds of vaccine is equal. However, in the second comparison, `Summary OR = 0.483` and the CI does not contain 1. It suggests that the MMRV vaccine against mumps is less effective than the MMR + V vaccine. It is an interesting finding."
                                              )))
                                              )
                                              )
                )
              )
          
            ))


                                              )#close tabItems
          )#close dashboardbody

###closing for SERVER DON'T DELET####
        )
))

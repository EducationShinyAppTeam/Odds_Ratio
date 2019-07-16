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
library(metafor)

# insert "convertMentItem" function

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}


# Creat the UI
shinyUI(fluidPage(
       dashboardPage(skin = "black",
                    
                    #header 
                    dashboardHeader(title = "Odds Ratio",
                                    titleWidth = 180,
                                    tags$li(class = "dropdown", tags$a(href='https://shinyapps.science.psu.edu/',icon("home"))),
                                    tags$li(class = "dropdown", actionLink("info",icon("info",class="myClass")))
                    ),
          
                    #menu bar
                    dashboardSidebar(width = 180,
                           sidebarMenu(id='tabs',style='font-size:13px;',
                                       convertMenuItem(menuItem("Overview",tabName = "instruction", icon = icon("dashboard"),
                                                               menuSubItem("Prerequisites", tabName= "prereq", icon=icon("book"))),
                                                       'instruction'),
                                       menuItem("Explore",tabName = "explore", icon = icon("wpexplorer")),
                                       menuItem("Real Data Analysis",tabName = "analysis", icon = icon("cogs"))
                                      )),
                    
                    #change the color,bacground color & word styles of buttons, icons & words
                    dashboardBody(
                                  #change header font
                                  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")),
                                
                                  #change icon color
                                  tags$head(tags$style(".fa-home {color:#ffffff}"),
                                            tags$style(".fa-info {color:#ffffff}")),
                                        
                                  #change button color
                                  tags$head(tags$style(HTML('#start1{color:white;background-color: #ff5300}')),
                                            tags$style(HTML('#go{color:white;background-color: #ff5300}')),
                                            tags$style(HTML('#pre{color:white;background-color: #ff5300}')),
                                            tags$style(HTML('#analysis{color:white;background-color: #ff5300}'))
                                  ),
                                  
                               
                                  #change header color
                                  tags$style(HTML('
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
                                                  }')),
  

                                  tabItems(      
                                           # instruction page
                                           tabItem(tabName = "instruction",
                                                   tags$a(href='http://stat.psu.edu/',tags$img(src='logo.png', align = "left", width = 180)),
                                                   br(),br(),br(),
                                                   h3(strong("About:")),
                                                   h4('This app displays the confident interval of odds ratio.'),
                                                   br(),
                                                   div(style = "text-align: center",
                                                       actionButton("pre","Prerequisite",icon("bolt"),style='padding:10px; font-size:100%',class="circle grow")),
                                                   br(),
                                                   h3(strong('Instructions:')),
                                                   h4(tags$li('Click Go button to enter the explore page. ')),
                                                   h4(tags$li('Use the radio buttons to select different variables and see the changes in the interaction plot. Or use slider bars to change the parameters. ')),
                                                   h4(tags$li('After working with the explore section, you can start the matching analysis to test your understanding of the concepts. You can use "i" button for instruction and "?" for hints.')),
                               
                                                   div(style = "text-align: center",
                                                       actionButton("go","Explore",icon("bolt"),class="circle grow")),
                                                   br(),
                                                   h3(strong('Acknowledgements:')),
                                                   h4("This app was developed and coded by Jingjun Wang.")
                                                  ),
                         
                                           # pre-requisite page
                                           tabItem(tabName="prereq", withMathJax(),
                                                   h3(strong('Background')),
                                                   h3('What is odds ratio?'),
                                                   h4('An odds ratio is a measure of association between the presence or absence of two properties.'),
                                                   h3('How to calculate the confident interval of an odds ratio?'),
                                                   tags$img(src = 'table.png'),
                                                   tags$img(src = 'knowledge.png',style = "width: 85%;"),
                                                   br(),
                                                   h3('Example'),
                                                   h4('Here is the contingency table from a case-control study of smoking and lung cancer:'),
                                                   br(),
                                                   tags$img(src = 'sample_question.png'),   
                                                   br(),
                                                   br(),
                                                   tags$li(h4("The odds of lung cancer for smokers is calculated as 647/622 = 1.04")),

                                                   br(),
                                                   tags$li(h4('The odds of lung cancer for non-smokers is 2/27 = 0.07.')),
                                                   br(),
                                                   tags$li(h4('It is the ratio of the odds of lung cancer in smokers divided by the odds of lung cancer in non-smokers: (647/622)/(2/27) = 14.04. ')),
                                                   br(),
                                                   tags$li(h4('Here, the odds ratio is greater than 1.')),
                                                   br(),
                                                   h4('Being a smoker is considered to be associated with having lung cancer since smoking raises the odds of having lung cancer'),
                                                   br(),
                                                   div(style = "text-align: center",
                                                       actionButton("start1","Go to the overview",icon("bolt"),style='padding:10px; font-size:100%',class="circle grow"))
                                                  ),
                                           
                                           # explore page
                                           
                                           tabItem(tabName = "explore",
                                                   titlePanel(strong("Odds Ratio for Enrollment by Residency between University Park and Commonwealth Campuses")),
                                                   sidebarLayout(
                                                     sidebarPanel(
                                                       h3(strong("True Population:")),
                                                       h4("Percentage: "),
                                                       img(src="2016Diff.png",height = "100%", width = "90%",algin = "middle"),
                                                       h4("Count: "),
                                                       img(src="2016Count.png",height = "100%", width = "90%",algin = "middle"),
                                                       br(), br(),
                                                       h4(p(("Odds of Pennsylvania residents for University Park:       1.47 "), 
                                                            style="white-space: pre-wrap")),
                                                       h4(p(("Odds of Pennsylvania residents for other campuses:     5.42 "),
                                                            style="white-space: pre-wrap")),
                                                       h4(p(("Odds ratio (theta) :                                                                    0.27"),
                                                            style="white-space: pre-wrap")),
                                                       
                                                       br(),
                                                       
                                                       tags$style(HTML(" .tabbable > .nav > li > a                  {background-color: white; color:#ff7532}
                                                                        .tabbable > .nav > li[class=active] > a {background-color: #ff7532; color: white;}
                                                                         ")),
                                                       h3(strong("Sliders: ")),
                                                       tabsetPanel( id = "tabset",
                                                         tabPanel("Seperate Sample Sizes", fluid = TRUE,
                                                                  
                                                                    br(),
                                                                    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #ff864c}")),
                                                                    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #ff864c}")),
                                                                    tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #ff864c}")),
                                                                    sliderInput("dlevel", "Confidence Level",
                                                                                min=.10, max = 0.99, value = 0.95, step = 0.01),
                                                                    
                                                                    sliderInput("nSamp1", "Sample Size for University Park",
                                                                                min=30, max = 200, value = 50, step = 5),
                                                                    sliderInput("nSamp2", "Sample Size for Other Campuses",
                                                                                min=30, max = 200, value = 50, step = 5)
                                                                  
                                                                  
                                                         ),
                                                         tabPanel("Same Sample Size", fluid = TRUE,
                                                                  
                                                                    br(),
                                                                    tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #ff864c}")),
                                                                    tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #ff864c}")),
                                                                    
                                                                    sliderInput("dlevel1", "Confidence Level",
                                                                                min=.10, max = 0.99, value = 0.95, step = 0.01),
                                                                    
                                                                    sliderInput("nSamp3", "Sample Sizes for both University Park and Other Campuses",
                                                                                min=30, max = 200, value = 50, step = 5)
                                                                  
                                                         )
                                                       )
                                                      
                                                       
                                                     ),
                                                     mainPanel(
                                         
                                                       sidebarLayout(
                                                         mainPanel(plotOutput("CIplot",height = "600px", click = "plot_click"),
                                                                   bsPopover("CIplot","Confidence Interval Plot",
                                                                             "The orange lines indicate a confidence interval that smaller or greater than 1 and the purple lines indicate confidence intervas containing 1. Click on an interval to see detailed information on the right-hand side for the chosen sample.",
                                                                             trigger="hover",placement="bottom")),
                                                         sidebarPanel(
                                                           h3(strong("Sample Percentages:")),
                                                           span(tableOutput("sampleinfotable1"), style="font-size: 18px"),
                                                           h3(strong("Sample Counts:")),
                                                           span(tableOutput("sampleinfotable2"), style="font-size: 18px"),
                                                           h3(strong("Sample Odds Ratio:")),
                                                           span(textOutput("sampleinforatio"), style="font-size: 18px"))
                                                         ),
                                                       br(),
                                                       actionButton("newSample", "Generate 50 New Samples",icon("retweet"),
                                                                    style = "color: white; background-color: #ff7532"),
                                                       bsPopover("newSample","Note","By clicking on this button, new sample with the size you input will be generated.",
                                                                 trigger="hover",placement="bottom")
                                                     )
                                                   )
                                                   
                                                   
                                           ),
                                           
                                           #analysis page
                                           tabItem(tabName="analysis",
                                                   sidebarLayout(
                                                     sidebarPanel(
                                                       
                                                       tags$style(type='text/css', ".selectize-input { font-size: 18px; line-height: 18px;} .selectize-dropdown { font-size: 19px; line-height: 19px; }"),
                                                       h3(strong("Choose a Dataset Below")),
                                                       selectInput("sets",NA,
                                                                   list("Non-Small Cell Lung Cancer Treatment" = 
                                                                          c(
                                                                     "Gefitinib vs. Chemotherapy"="gvc",
                                                                     "Gefitinib vs. Erlotinib"= "gve"
                                                                             ),
                                                                     "Malaria Treatment" =
                                                                       c("Artesunate-based therapies vs. Quinine (for uncomplicated malaria)" = "avq",
                                                                         'Artemether vs. Quinine (for cerebral malaria)' = 'amvq'),
                                                                     "Vaccines Immunogenicity" = 
                                                                       c("MMRV vs. MMR+V Against Measles"="mea",
                                                                         "MMRV vs. MMR+V Against Mumps"="mum",
                                                                         "MMRV vs. MMR+V Against Rubella"="rub"
                                                                        )

                                                                        
                                                                        ),
                                                                   width = validateCssUnit("70%")
                                                                   ),
                                                       br(),
                                                       h3(strong("Background Knowledge")),
                                                       conditionalPanel(
                                                         condition = "input.sets== 'gvc'|input.sets== 'gve'",
                                                         tags$style("#nsclc{ font-size: 20px; }" ),
                                                         textOutput("nsclc")
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.sets == 'avq'|input.sets== 'amve'",
                                                         tags$style("#mala{ font-size: 20px; }" ),
                                                         textOutput("mala")
                                                         ),
                                                        conditionalPanel(
                                                           condition = "input.sets == 'mea'|input.sets== 'mum'|input.sets== 'rub'",
                                                           tags$style("#vacc{ font-size: 20px; }" ),
                                                           textOutput("vacc")

                                                       )
                                                      
                                                   ),
                                                   mainPanel(
                                                    conditionalPanel(
                                                         condition = "input.sets == 'gvc'",
                                                         h3(p(strong("Gefitinib vs. Chemotherapy"), 
                                                              style="text-align: center")),
                                                          uiOutput("plot1")
                                                         #a=img(src="gvc.png",height = "100%", width = "80%",algin = "middle")
                                                         
                                                         ),
                                                    conditionalPanel(
                                                        condition = "input.sets == 'gve'",
                                                        h3(p(strong("Gefitinib vs. Erlotinib"), 
                                                             style="text-align: center")),
                                                        img(src="gve.png",height = "100%", width = "80%",algin = "middle")
                                                        ),
                                                    conditionalPanel(
                                                      condition = "input.sets == 'avq'",
                                                      h3(p(strong("Artesunate-based therapies vs. Quinine"), 
                                                           style="text-align: center")),
                                                      h4(p(strong("(uncomplicated malaria in pregnancy)"), 
                                                           style="text-align: center")), 
                                                      img(src="avq.png",height = "100%", width = "80%",algin = "middle")
                                                      ),
                                                    conditionalPanel(
                                                      condition = "input.sets == 'amvq'",
                                                      h3(p(strong("Artemether vs. Quinine"), 
                                                           style="text-align: center")),
                                                      h4(p(strong("(cerebral malaria in African children \u2264 15 years of age)"), 
                                                           style="text-align: center")), 
                                                      img(src="amvq.png",height = "100%", width = "80%",algin = "middle")
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.sets == 'mea'",
                                                      h3(p(strong("MMRV vs. MMR+V Against Measles"), 
                                                           style="text-align: center")),
                                                      img(src="mea.png",height = "100%", width = "80%",algin = "middle")
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.sets == 'mum'",
                                                      h3(p(strong("MMRV vs. MMR+V Against Mumps"), 
                                                           style="text-align: center")),
                                                      img(src="mum.png",height = "100%", width = "80%",algin = "middle")
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.sets == 'rub'",
                                                      h3(p(strong("Artemether vs. Quinine"), 
                                                           style="text-align: center")),
                                                      img(src="rub.png",height = "100%", width = "80%",algin = "middle")
                                                    )
                                                              )
                                                   
                                                   ))
   
                    
                    )#close tabItems            
      )#close dashboardbody                                   
                      
###closing for SERVER DON'T DELET####          
)))


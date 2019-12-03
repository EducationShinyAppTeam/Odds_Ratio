library(shiny)
library(png)
library(shinyBS)
library(shinyDND)
library(shinyjs)
library(shinydashboard)
library(simstudy)
library(shinyalert)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(data.table)
library(scales)
library(Hmisc)
library(colorspace)
library(tidyverse)
library(broom)
library(rmeta)

#read in dataset



# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  ##########################action buttons##################################### 
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "This app explores confidence intervals for odds ratios and their use in the meta-analysis of real data.",
      type = "info"
    )
  })
  
  observeEvent(input$pre,{
    updateTabItems(session,"tabs","prereq")
  })
  
  observeEvent(input$start1,{
    updateTabItems(session,"tabs","instruction")
  })
  
  observeEvent(input$go,{
    updateTabItems(session,"tabs","explore")
  })
  
  
  observeEvent(input$analysis,{
    updateTabItems(session,"tabs","analysis")
  })
  
  ############################Gray out buttons###############################
  
  
  observeEvent(input$start2, {
    updateButton(session, "answer", disabled = TRUE)
  })
  
  observeEvent(input$challenge, {
    updateButton(session, "answer", disabled = FALSE)
  })
  
  observeEvent(input$answer, {
    updateButton(session, "answer", disabled=TRUE)
  })
  
  observeEvent(input$begin, {
    updateButton(session, "submit", disabled = TRUE)
  })
  
  
  
  
  
  
  #exploring
  ####################################################################
  #####################################################################
  
  
  # print greek letter
  
  output$hypo <- renderPrint({
    print(paste0("H0: $$\\hat{A}_{\\small{\\textrm{M???}}} =", 1,"$$"))
  })
  
  output$testdesign = renderUI({
    if(input$testdesigncheckbox)
    {
      h4("A researcher wants to sample a group of n University Park students and n students from other Penn State campuses to ask them about their experiences in college.  Although the percentage of Pennsylvania residents is 24.9% lower at University Park, a critic believes her sampling technique would provide a sample of students with a proportion (p) that does not depend on the campus (the null hypothesis). The researcher uses her samples to conduct a test of that null hypothesis and this app shows how that test would behave when the sampling is really unbiased and the University Park campus has a proportion that is 0.249 lower lower. ")
    }
  })
  
  
  #Calculating alpha by the confidence level input
  dalpha <- reactive({
    (1 - input$dlevel) / 2
  })
  
  #Updating Sample Size
  dN1 <- reactive({
    as.integer(input$nSamp1)
  })
  
  dN2 <- reactive({
    as.integer(input$nSamp2)
  })
  dN3 <- reactive({
    as.integer(input$nSamp3)
  })
  
  # standardError <- reactive({
  #   sqrt(0.595*0.405/dN() + 0.844*0.156/dN())
  # })
  
  #population mean plot with true diffmean
  output$dpopMean  = renderPlot({
    # dat <- read.table(text = "University_Park Other_Campuses
    #                           Pennsylvania_Students 0.595 0.844
    #                           Out-of-State_Students 0.405	 0.156",
    #                   sep = "",header = TRUE)
    
    dfPop <- data.frame(types = rep(c("Pennsylvania_Students", "Out-of-State_Students"), each=2),
                        location=rep(c("University Park", "Other Campuses"),2),
                        samplepercent=c(0.595,0.844,0.405,0.156))
    
    ggplot(dfPop,aes(x = location,y = samplepercent, fill = types)) +
      geom_bar(position = position_fill(),stat="identity", width=0.3) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_brewer(palette="Paired")+
      labs(
        title = paste0("population proportion(diff) = -24.9%, σ(p(UP)-p(Others)) = ",round(sqrt(0.595*0.405 + 0.844*0.156),3)),
        y = "Enrollment by Percentage")
    
  })
  #generate 50 new sample
  
  UPS50P <- reactive({
    input$newSample
    # rbinom(n=dN1(), 1, 0.595)
    data.frame(
      x =
        do.call(
          paste0("rbinom"),
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
      x =
        do.call(
          paste0("rbinom"),
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
    merge(data50_1(),data50_2(), by="idx")
  })
  
  
  #generate 50 new sample (combined sample size)
  
  UPS50P_3 <- reactive({
    input$newSample
    
    data.frame(
      x =
        do.call(
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
      x =
        do.call(
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
    merge(data50_1_3(),data50_2_3(), by="idx")
  })
  
  
  
  
  #calculate the interval
  Intervals <- reactive({
    zvalue = qnorm(((1-input$dlevel)/2), lower.tail = F)
    sampleRatio = (data50()[,2]*data50()[,5])/(data50()[,3]*data50()[,4])
    lowerbound = exp(log(sampleRatio)-zvalue*sqrt(1/data50()[,2]+1/data50()[,5]+1/data50()[,3]+1/data50()[,4]))
    upperbound = exp(log(sampleRatio)+zvalue*sqrt(1/data50()[,2]+1/data50()[,5]+1/data50()[,3]+1/data50()[,4]))
    data.frame(idx = rep(1:50), 
               sampleRatio,
               lowerbound,
               upperbound,
               cover = (lowerbound < 1) & (1 < upperbound))
    
  })
  
  newIntervals <- reactive({
    zvalue = qnorm(((1-input$dlevel1)/2), lower.tail = F)
    sampleRatio = (newdata50()[,2]*newdata50()[,5])/(newdata50()[,3]*newdata50()[,4])
    lowerbound = exp(log(sampleRatio)-zvalue*sqrt(1/newdata50()[,2]+1/newdata50()[,5]+1/newdata50()[,3]+1/newdata50()[,4]))
    upperbound = exp(log(sampleRatio)+zvalue*sqrt(1/newdata50()[,2]+1/newdata50()[,5]+1/newdata50()[,3]+1/newdata50()[,4]))
    data.frame(idx = rep(1:50), 
               sampleRatio,
               lowerbound,
               upperbound,
               cover = (lowerbound < 1) & (1 < upperbound))
    
  })
  
  output$show1 <- renderTable({
    newIntervals()
    
  })
  output$show2 <- renderTable({
    
    Intervals()
  })
  
  #default as all the samples are selected
  selected_sample <- 50
  selectedSample <- reactive({
    if (! is.null(input$plot_click)) {
      selected_sample <<- round(input$plot_click$y)
      if (selected_sample < 1) selected_sample <<- 1
      if (selected_sample > 50) selected_sample <<- 50
    }
    selected_sample
  })
  # selected sample 
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
  
  #print the CIplot
  output$CIplot <- renderPlot({
    if (input$tabset == "Same Sample Size"){
      validate(
        need(is.numeric(input$nSamp3),
             message = "Please input sample size")
      )
      
      ggplot(data = newIntervals()) +
        geom_pointrange(
          aes(x=idx, ymin = lowerbound, ymax = upperbound, y = sampleRatio, colour = cover,
              alpha = idx == selectedSample(),
              size = idx == selectedSample()
          )) +
        geom_hline(yintercept = 1, size = 1.8, colour = "#000000", alpha = 0.5) +
        geom_hline(yintercept = .27, size = 1.8, colour = "#0B6623", alpha = 0.5) +
        coord_flip() +
        scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = .8), guide = FALSE) +
        scale_color_manual(values = c("FALSE" = "#ff864c", "TRUE" = "#916cdf"), guide = FALSE) +
        scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .5), guide = FALSE) +
        lims(y = c(-0.01,4.55)) +
        labs(title = paste0(100 * input$dlevel1, "% Confidence Intervals"),
             x = "",y="black vertical line for null theta & green vertical line for true odds ratio",hjust = 5, vjust = 1) +
        theme(legend.position = "none",
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(size=18),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
    }
    
    else{
      validate(
        need(is.numeric(input$nSamp1), is.numeric(input$nSamp2),
             message = "Please input sample size")
      )
      
      ggplot(data = Intervals()) +
        geom_pointrange(
          aes(x=idx, ymin = lowerbound, ymax = upperbound, y = sampleRatio, colour = cover,
              alpha = idx == selectedSample(),
              size = idx == selectedSample()
          )) +
        geom_hline(yintercept = 1, size = 1.8, colour = "#000000", alpha = 0.5) +
        geom_hline(yintercept = .27, size = 1.8, colour = "#0B6623", alpha = 0.5) +
        coord_flip() +
        scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = .8), guide = FALSE) +
        scale_color_manual(values = c("FALSE" = "#ff864c", "TRUE" = "#916cdf"), guide = FALSE) +
        scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .5), guide = FALSE) +
        lims(y = c(-0.01,4.55)) +
        labs(title = paste0(100 * input$dlevel, "% Confidence Intervals"),
             x = "",y="black vertical line for null theta & green vertical line for true odds ratio",hjust = 5, vjust = 1) +
        theme(legend.position = "none",
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(size=18),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
    }
    
  })
  
  
  # sample display
  output$sampleinfotable1 = renderTable({
    if (input$tabset == "Same Sample Size"){
      validate(
        need(is.numeric(input$nSamp3),
             message = "Please input sample size")
      )
      ctable <- matrix(c(percent(newOneSample()[,2]/input$nSamp3), percent(newOneSample()[,3]/input$nSamp3), 
                         percent(newOneSample()[,4]/input$nSamp3), percent(newOneSample()[,5]/input$nSamp3)), ncol=2, 
                       dimnames = list(Campus = c("University Park","Other Campuses"), State = c("Penn", "Non-Penn")))
      rownames(ctable) = c("University Park","Other Campuses")
      ctable
    }
    
    else{
      validate(
        need(is.numeric(input$nSamp1),is.numeric(input$nSamp2),
             message = "Please input sample size")
      )
      ctable <- matrix(c(percent(OneSample()[,2]/input$nSamp1), percent(OneSample()[,3]/input$nSamp2), 
                         percent(OneSample()[,4]/input$nSamp1), percent(OneSample()[,5]/input$nSamp2)), ncol=2, 
                       dimnames = list(Campus = c("University Park","Other Campuses"), State = c("Penn", "Non-Penn")))
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
      ctable <- matrix(c(newOneSample()[,2], newOneSample()[,3], newOneSample()[,4], newOneSample()[,5]), ncol=2, 
                       dimnames = list(Campus = c("University Park","Other Campuses"), State = c("Penn", "Non-Penn")))
      rownames(ctable) = c("University Park","Other Campuses")
      ctable
    }
    else{
      validate(
        need(is.numeric(input$nSamp1),is.numeric(input$nSamp2),
             message = "Please input sample size")
      )
      ctable <- matrix(c(OneSample()[,2], OneSample()[,3], OneSample()[,4], OneSample()[,5]), ncol=2, 
                       dimnames = list(Campus = c("University Park","Other Campuses"), State = c("Penn", "Non-Penn")))
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
      cratio<-round(((newOneSample()[,2])*(newOneSample()[,5])/(newOneSample()[,3]*newOneSample()[,4])), 2)
      cratio
    }
    else{
      validate(
        need(is.numeric(input$nSamp1),is.numeric(input$nSamp2),
             message = "Please input sample size")
      )
      cratio<-round(((OneSample()[,2])*(OneSample()[,5])/(OneSample()[,3]*OneSample()[,4])), 2)
      cratio
    }
  })
  
  
  ########forestplot########
  
  makeDatatabletoList <- function(mytable){
    mylist = c()
    rnum <- nrow(mytable)
    for (i in 1:rnum){
      mylist[[i]] = matrix(as.numeric(mytable[i,]),nrow=2,byrow=TRUE)
    }
    mylist
  }
  
  makeTable <- function(mylist, referencerow=2)
  {
    require("rmeta")
    numstrata <- length(mylist)
    # make an array "ntrt" of the number of people in the exposed group, in each stratum
    # make an array "nctrl" of the number of people in the unexposed group, in each stratum
    # make an array "ptrt" of the number of people in the exposed group that have the disease,
    # in each stratum
    # make an array "pctrl" of the number of people in the unexposed group that have the disease,
    # in each stratum
    ntrt <- vector()
    nctrl <- vector()
    ptrt <- vector()
    pctrl <- vector()
    if (referencerow == 1) { nonreferencerow <- 2 }
    else                   { nonreferencerow <- 1 }
    for (i in 1:numstrata)
    {
      mymatrix <- mylist[[i]]
      DiseaseUnexposed <- mymatrix[referencerow,1]
      ControlUnexposed <- mymatrix[referencerow,2]
      totUnexposed <- DiseaseUnexposed + ControlUnexposed
      nctrl[i] <- totUnexposed
      pctrl[i] <- DiseaseUnexposed
      DiseaseExposed <- mymatrix[nonreferencerow,1]
      ControlExposed <- mymatrix[nonreferencerow,2]
      totExposed <- DiseaseExposed + ControlExposed
      ntrt[i] <- totExposed
      ptrt[i] <- DiseaseExposed
    }
    names <- as.character(seq(1,numstrata))
    myMH <- meta.MH(ntrt, nctrl, ptrt, pctrl, conf.level=0.95, names=names,statistic="OR")
    
    
    tabletext<-cbind(c("","Study",myMH$names,NA,"Summary"),
                     c("Treatment","(effective)",ptrt,NA,NA),
                     c("Treatment","(non-effective)",pctrl, NA,NA),
                     c("Control","(effective)",(ntrt-ptrt),NA,NA),
                     c("Control","(non-effective)",(nctrl-pctrl), NA,NA),
                     c("","OR",format((exp(myMH$logOR)),digits=3),NA,format((exp(myMH$logMH)),digits=3)))
  }
  
  
  makeForestPlot <- function(mylist, referencerow=2)
  {
    require("rmeta")
    numstrata <- length(mylist)
    # make an array "ntrt" of the number of people in the exposed group, in each stratum
    # make an array "nctrl" of the number of people in the unexposed group, in each stratum
    # make an array "ptrt" of the number of people in the exposed group that have the disease,
    # in each stratum
    # make an array "pctrl" of the number of people in the unexposed group that have the disease,
    # in each stratum
    ntrt <- vector()
    nctrl <- vector()
    ptrt <- vector()
    pctrl <- vector()
    if (referencerow == 1) { nonreferencerow <- 2 }
    else                   { nonreferencerow <- 1 }
    for (i in 1:numstrata)
    {
      mymatrix <- mylist[[i]]
      DiseaseUnexposed <- mymatrix[referencerow,1]
      ControlUnexposed <- mymatrix[referencerow,2]
      totUnexposed <- DiseaseUnexposed + ControlUnexposed
      nctrl[i] <- totUnexposed
      pctrl[i] <- DiseaseUnexposed
      DiseaseExposed <- mymatrix[nonreferencerow,1]
      ControlExposed <- mymatrix[nonreferencerow,2]
      totExposed <- DiseaseExposed + ControlExposed
      ntrt[i] <- totExposed
      ptrt[i] <- DiseaseExposed
    }
    names <- as.character(seq(1,numstrata))
    myMH <- meta.MH(ntrt, nctrl, ptrt, pctrl, conf.level=0.95, names=names,statistic="OR")
    
    # metaplot(myMH$logOR, myMH$selogOR, nn=myMH$selogOR^-2, myMH$names,
    #          summn=myMH$logMH, sumse=myMH$selogMH, sumnn=myMH$selogMH^-2,
    #          logeffect=T, colors=meta.colors(box="#34186f",lines="blue", zero ="red", 
    #                                          summary="#ff864c", text="black"))
    
    # tabletext_less<-cbind(c("","Study",myMH$names,NA,"Summary"))
    # 
    # m<- c(NA,NA,exp(myMH$logOR),NA,exp(myMH$logMH))
    # l<- exp(c(NA,NA,myMH$logOR,NA,myMH$logMH)-c(NA,NA,myMH$selogOR,NA,myMH$selogMH)*1.96)
    # u<- exp(c(NA,NA,myMH$logOR,NA,myMH$logMH)+c(NA,NA,myMH$selogOR,NA,myMH$selogMH)*1.96)
    # 
    # forestplot(tabletext_less,m,l,u,zero=1,is.summary=c(TRUE,TRUE,rep(FALSE,(length(mylist)+1)),TRUE),
    #            col=meta.colors(box="#916cdf",line="#34186f", summary="#ff864c"),
    #            xlab="\nOdds ratio with 95% confidence interval\n(<1=no effect, 1=treatment has effect)", 
    #            clip=c(-0.01,1000), boxsize = 1.5)
  }
  
  
  
  
  
  ## Non-Small Cell Lung Cancer Treatment introduction
  output$nsclc=renderText("About 80% to 85% of lung cancers are non-small cell lung cancer (NSCLC). The typical treatments include chemotherapy, radiation therapy and targeted therapy. 
                          Gefitinib and Erlotinib are two kind of medicine used in NSCLC targeted therapy. In the two comparisons, Gefitinib represents treatment groups.")
  
  # drug 1: Gefitinib vs chemotherapy
  gvc1 <- matrix(c(42,37,48,53),nrow=2,byrow=TRUE)
  gvc2 <- matrix(c(18,12,26,32),nrow=2,byrow=TRUE)
  gvc_list = list(gvc1, gvc2)
  # makeForestPlotForRCTs(gvc_list)
  
  output$plot1 = renderUI({
    img(src = "gvc.PNG", width = "80%", algin = "middle")
  })
  # output$table1 = renderPlot(makeTable(gvc_list))
  # output$plot1 = renderPlot(makeForestPlot(gvc_list))
  
  # drug 2: Gefitinib vs Erlotinib 
  gve1 <- matrix(c(28,6,22,12),nrow=2,byrow=TRUE)
  gve2 <- matrix(c(36,14,38,12),nrow=2,byrow=TRUE)
  gve3 <- matrix(c(16,19,21,14),nrow=2,byrow=TRUE)
  gve_list = list(gve1, gve2, gve3)
  #  makeForestPlot(gve_list)
  output$plot2 = renderUI({
    img(src = "gve.PNG", width = "80%", algin = "middle")
  })
  
  observeEvent(input$comments1, {
    toggle(id= "nsclc_comments")
  })
  
  ## Malaria Treatment
  output$mala = renderText("Artemisinin is a plant-derived compound, isolated from the Artemisia annua, sweet wormwood a herb employed in Chinese herbal medicine. 
                           This compound (along with its derivative drugs), is the World Health Organization's recommended treatment against malaria caused by Plasmodium falciparum. 
                           Quinine, isolated from cinchona bark, is the first meditation to treat malaria.
                           In the two comparisons, artesunate-based therapies represent treatment groups.")
  
  
  #drug 1: Artemisinin-based combination therapies vs. Quinine
  avq1 <- matrix(c(37,26,2,15),nrow=2,byrow=TRUE)
  avq2 <- matrix(c(64,34,2,8),nrow=2,byrow=TRUE)
  avq3 <- matrix(c(137,122,1,3),nrow=2,byrow=TRUE)
  avq_list = list(avq1, avq2, avq3)
  output$plot3 = renderUI({
    img(src = "avq.PNG", width = "85%", algin = "middle")
  })
  
  
  
  #drug 2: artemether vs. Quinine
  amvq1 <- matrix(c(6,10,45,42),nrow=2,byrow=TRUE)
  amvq2 <- matrix(c(18,8,71,63),nrow=2,byrow=TRUE)
  amvq3 <- matrix(c(11,14,43,35),nrow=2,byrow=TRUE)
  amvq4 <- matrix(c(1,2,17,17),nrow=2,byrow=TRUE)
  amvq5 <- matrix(c(10,12,73,69),nrow=2,byrow=TRUE)
  amvq6 <- matrix(c(59,62,229,226),nrow=2,byrow=TRUE)
  amvq7 <- matrix(c(3,2,35,37),nrow=2,byrow=TRUE)
  amvq_list = list(amvq1, amvq2, amvq3, amvq4, amvq5, amvq6, amvq7)
  output$plot4 = renderUI({
    img(src = "amvq.PNG", width = "85%", algin = "middle")
  })
  
  observeEvent(input$comments2, {
    toggle(id= "mala_comments")
  })
  ## Vaccines Immunogenicity
  
  output$vacc = renderText("A combined measles-mumps-rubella-varicella (MMRV) vaccine is expected to facilitate universal immunization against these 4 diseases. 
                           Here randomized controlled trials (RCTs) were conducted to compare single MMRV dose with measles-mumps-rubella vaccine with varicella vaccine (MMR + V). 
                           All included studies reported seroconversion rate as serological response outcome. 
                           Seroconversion rate was defined as percent of subjects initially seronegative (with titers \u2264 assay cut-offs), who developed postvaccination antibody titers above the assay cut-off levels.
                           In the three comparisons, MMRV represents treatment groups.")
  #1: measles
  mea1 <- matrix(c(289,141,4,1),nrow=2,byrow=TRUE)
  mea2 <- matrix(c(1107,540,9,15),nrow=2,byrow=TRUE)
  mea3 <- matrix(c(73,68,1,6),nrow=2,byrow=TRUE)
  mea4 <- matrix(c(1114,181,29,9),nrow=2,byrow=TRUE)
  mea12 <- matrix(c(290,145,12,0),nrow=2,byrow=TRUE)#contain 0
  mea5 <- matrix(c(980,349,9,1),nrow=2,byrow=TRUE)
  mea6 <- matrix(c(299,106,7,0),nrow=2,byrow=TRUE)#contain 0
  mea7 <- matrix(c(2437,841,72,20),nrow=2,byrow=TRUE)
  mea8 <- matrix(c(125,113,9,9),nrow=2,byrow=TRUE)
  mea9 <- matrix(c(10,7,0,1),nrow=2,byrow=TRUE)#contain 0
  mea10 <- matrix(c(633,199,37,14),nrow=2,byrow=TRUE)
  mea11 <- matrix(c(294,155,6,1),nrow=2,byrow=TRUE)
  mea_list = list(mea1, mea2, mea3, mea4, mea5, mea7, mea8, mea10, mea11)
  output$plot5 = renderUI({
    img(src = "mea.PNG", width = "90%", algin = "middle")
  })
  
  
  # makeForestPlotForRCTs(mea_list)
  
  
  
  #2: mumps
  mum1 <- matrix(c(287,137,12,4),nrow=2,byrow=TRUE)
  mum2 <- matrix(c(927,516,167,28),nrow=2,byrow=TRUE)
  mum3 <- matrix(c(70,69,2,4),nrow=2,byrow=TRUE)
  mum4 <- matrix(c(992,173,117,9),nrow=2,byrow=TRUE)
  mum5 <- matrix(c(292,148,3,2),nrow=2,byrow=TRUE)
  mum6 <- matrix(c(1002,350,10,1),nrow=2,byrow=TRUE)
  mum7 <- matrix(c(272,101,30,4),nrow=2,byrow=TRUE)
  mum8 <- matrix(c(2409,854,100,18),nrow=2,byrow=TRUE)
  mum9 <- matrix(c(113,108,20,10),nrow=2,byrow=TRUE)
  mum10 <- matrix(c(10,8,0,0),nrow=2,byrow=TRUE)#contain 0
  mum11 <- matrix(c(613,191,37,16),nrow=2,byrow=TRUE)
  mum12 <- matrix(c(262,145,33,9),nrow=2,byrow=TRUE)
  mum_list = list(mum1, mum2, mum3, mum4, mum5, mum6, mum7, mum8, mum9, mum11, mum12)
  output$plot6 = renderUI({
    img(src = "mum.PNG", width = "90%", algin = "middle")
  })
  # makeForestPlotForRCTs(mum_list)
  
  
  
  #3: rubella
  rub1 <- matrix(c(288,141,10,0),nrow=2,byrow=TRUE)#contain 0
  rub2 <- matrix(c(1114,552,3,3),nrow=2,byrow=TRUE)
  rub3 <- matrix(c(73,74,1,0),nrow=2,byrow=TRUE)#contain 0
  rub4 <- matrix(c(1148,189,1,0),nrow=2,byrow=TRUE)#contain 0
  rub5 <- matrix(c(289,142,15,11),nrow=2,byrow=TRUE)
  rub6 <- matrix(c(1004,352,11,4),nrow=2,byrow=TRUE)
  rub7 <- matrix(c(303,106,3,0),nrow=2,byrow=TRUE)#contain 0
  rub8 <- matrix(c(2501,859,31,7),nrow=2,byrow=TRUE)
  rub9 <- matrix(c(113,108,20,10),nrow=2,byrow=TRUE)#contain 0
  rub10 <- matrix(c(10,8,0,0),nrow=2,byrow=TRUE)#contain 0
  rub11 <- matrix(c(665,208,2,4),nrow=2,byrow=TRUE)
  rub12 <- matrix(c(297,157,1,0),nrow=2,byrow=TRUE)#contain 0
  rub_list = list(rub2,rub5, rub6, rub8, rub11)
  output$plot7 = renderUI({
    img(src = "rub.PNG", width = "85%", algin = "middle")
  })
  # makeForestPlotForRCTs(rub_list)
  
  observeEvent(input$comments3, {
    toggle(id= "vacc_comments")
  })
  #check answer
  
  
  #closing for SERVER DON'T DELET####      
})
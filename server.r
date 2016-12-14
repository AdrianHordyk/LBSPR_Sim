
library(shiny)
library(shinyBS)
library(LBSPR)
library(gridExtra)
library(ggplot2)
library(dplyr)


shinyServer(function(input, output, clientData, session) {

  output$InputPars <- renderUI({
    times <- input$defPars
    div(id=letters[(times %% length(letters)) + 1],
	# div(id='biopars',
	h3("Biological Parameters"),
	h4("Life history ratios"),
  	fluidRow(
	  column(8, 
	    radioButtons("dorelLinf", "Asymptotic Length",
		  choices=c("Relative" = TRUE,
		           "Absolute" = FALSE), inline=TRUE)),
	  column(4,
	     conditionalPanel(condition="input.dorelLinf == 'TRUE'",
	      uiOutput("CurrLinf", style="padding-top:25px;")
		))
	),				   
	fluidRow(
	column(6,
	  sliderInput("MK", label=tags$i("M/K ratio"), value=1.5, min=0.5, max=3, step=0.1)
	  # textInput("MK", label=tags$i("M/K ratio"), value=1.5)
	),
	conditionalPanel(condition="input.dorelLinf == 'TRUE'",
	  column(6,
	    sliderInput("relLinf", label = HTML(paste0(tags$i("L", tags$sub("50")), "/",tags$i("L"), tags$sub(HTML("&infin;")))), value=0.66,
		  step=0.01, min=0.3, max=0.8))
	    # textInput("relLinf", label = HTML(paste0(tags$i("L", tags$sub("50")), "/",tags$i("L"), tags$sub(HTML("&infin;")))), value=0.66))
	),
	 column(6,
	 conditionalPanel(condition="input.dorelLinf == 'FALSE'",
	   sliderInput("SetLinf", label = HTML(paste0(tags$i("L"), tags$sub(HTML("&infin;")))), value=100, 
		  step=5, min=50, max=200))
	   # textInput("SetLinf", label = HTML(paste0(tags$i("L"), tags$sub(HTML("&infin;")))), value=100))
	)),
	h4("Length-at-Maturity"),
	fluidRow(
	column(6,
	  sliderInput("L50", label = tags$i(HTML(paste0("L", tags$sub("50")))), 
	    value=66, min=15, max=0.8*200, step=1)
	  # textInput("L50", label = tags$i(HTML(paste0("L", tags$sub("50")))), value=66)
	),
	column(6,
	  sliderInput("dL95", label = tags$i(HTML(paste0("dL", tags$sub("95")))), 
	    value=10, min=1, step=1, max=100)
	  # textInput("dL95", label = tags$i(HTML(paste0("L", tags$sub("d95")))), value=70)
	),

	column(12, 
	  conditionalPanel(condition="input.perRec == 'FALSE'",
	    fluidRow(column(12,
	      sliderInput("steep", "Steepness", min=0.25, max=1, step=0.05, value=0.6))),
	  conditionalPanel(condition="input.perRec == 'TRUE'")
    ))
	))
  })
  
   getLinf <- reactive({
    if (!is.null(input$dorelLinf) && input$dorelLinf == TRUE) {
	  tryLinf <- as.numeric(input$L50) / as.numeric(input$relLinf)
	  if (length(tryLinf)>0) {
       if(is.na(tryLinf)) return(NULL)
	   return(tryLinf)
	   # return(round(tryLinf,2))
      }
	} else {
	  return(as.numeric(input$SetLinf))
	}
  })
  
  output$CurrLinf <- renderUI({
    myLinf <- getLinf()
	if(!is.numeric(myLinf)) return("")
    myLinf <- round(getLinf(),2)
    HTML(paste0(tags$i("L"), tags$sub(HTML("&infin;"))), "=", myLinf)
  }) 
  
  output$FishingPars <- renderUI({
    times <- input$defPars
    div(id=letters[(times %% length(letters)) + 1],
	# div(id="fishpars",
	tags$hr(),
	h3("Exploitation Parameters"),
	fluidRow(
	column(5,
      radioButtons("FMorSPR", "SPR or F/M?",
	    choices=c("SPR" = TRUE,
	           "F/M" = FALSE), inline=TRUE)),
    column(7, 			   
	  conditionalPanel(condition="input.FMorSPR == 'TRUE'",
	    sliderInput("SPR", "SPR", min=0, max=1, step=0.01, value=0.4))
	),
	column(7,
	  conditionalPanel(condition="input.FMorSPR == 'FALSE'",
	   sliderInput("FM", "FM", min=0, max=3, step=0.1, value=1))
	)),

	h4("Selectivity-at-Length"),
	fluidRow(
	column(6,
	  sliderInput("SL50", label = tags$i(HTML(paste0("SL", tags$sub("50")))), 
	    value=40, step=1, min=1, max=100)
	  # textInput("SL50", label = tags$i(HTML(paste0("SL", tags$sub("50")))), value=40)
	),
	column(6,
	  sliderInput("dSL95", label = tags$i(HTML(paste0("dSL", tags$sub("95")))), 
	    value=10, min=1, step=1, max=100)
	  # textInput("dSL95", label = tags$i(HTML(paste0("dSL", tags$sub("95")))), value=50)
	)
	)
	)
  })
  
  runSim <- reactive({
    LB_pars <- new("LB_pars")
    LB_pars@MK <- as.numeric(input$MK)
    LB_pars@Linf <- as.numeric(getLinf())
    LB_pars@L50 <- as.numeric(input$L50)
    LB_pars@L95 <- as.numeric(input$dL95) + as.numeric(input$L50)
	LB_pars@SL50 <- as.numeric(input$SL50)
    LB_pars@SL95 <- as.numeric(input$dSL95) + as.numeric(input$SL50)
	LB_pars@Steepness <- as.numeric(input$steep)
	if (!is.null(input$perRec) && input$perRec == "TRUE") LB_pars@Steepness <- 0.999999
	if (length(LB_pars@Steepness) > 0 && LB_pars@Steepness > 0.99) LB_pars@Steepness <- 0.999999
	if (length(LB_pars@Steepness) > 0 && LB_pars@Steepness <= 0.2) LB_pars@Steepness <- 0.20001
	LB_pars@BinWidth <- LB_pars@Linf/15
	if (!is.null(input$FMorSPR) && input$FMorSPR == "TRUE") {
	  LB_pars@SPR <- as.numeric(input$SPR) 
	} else {
	  LB_pars@FM <- as.numeric(input$FM)
	}
	if (length(LB_pars@Linf) < 1 ) return(NULL)
    sim <- LBSPRsim(LB_pars, Control=list(ngtg=63))
    sim
  })
  
  output$SizeComp <- renderPlot({
    if (input$Ltype == "TRUE") lf.type <- "catch"
	if (input$Ltype == "FALSE") lf.type <- "pop"
	if (input$perRec == "TRUE") perRec <- TRUE
	if (input$perRec == "FALSE") perRec <- FALSE
	sim <- runSim()
	if (is.null(sim)) return(NULL)
    # p1 <- plotSim(sim, type=input$plots, lf.type=lf.type, perRec=perRec, 
	        # incSPR=TRUE, size.pt=input$size.pt)
    p1 <- plotSim(sim, lf.type=lf.type, perRec=perRec, 
	        incSPR=TRUE)			
	p1
  })
  
  output$Loading <- renderText({
    if(!is.null(runSim())) return("")
	if(is.null(runSim()))  return("Loading...")
  })
  
  output$dnloadImage <- downloadHandler(
    filename = function() {
		 'Image.png'
    },
    content = function(file) {
	if (input$Ltype == "TRUE") lf.type <- "catch"
	if (input$Ltype == "FALSE") lf.type <- "pop"
	if (input$perRec == "TRUE") perRec <- TRUE
	if (input$perRec == "FALSE") perRec <- FALSE
      # ggsave(file, plot = plotSim(runSim(), type=input$plots, lf.type=lf.type, 	  
	    # perRec=perRec, incSPR=TRUE, size.pt=input$size.pt), device = "png")
      ggsave(file, plot = plotSim(runSim(), lf.type=lf.type, 	  
	    perRec=perRec, incSPR=TRUE), device = "png")		
    }
  )  
})



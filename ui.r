library(shiny)
library(shinyBS)

shinyUI(tagList(tags$head(includeScript("google-analytics.js")),
  navbarPage("LBSPR Simulation Modeling", id="navbar",
  tabPanel("Instructions",
	  h3("Instructions")
  ),
  tabPanel("Simulation Model",
     # titlePanel("Upload Data File"),
    sidebarLayout(
	  sidebarPanel(
		uiOutput("InputPars"),
		# conditionalPanel(condition="input.dorelLinf == 'TRUE'",
		  # h4("Asymptotic Length"),
	      # uiOutput("CurrLinf", style="padding-bottom:25px;")
		# ),
		uiOutput("FishingPars"),
		actionButton("defPars", "Reset Default Parameters", icon("gear")),
		tags$hr(),
		p(paste("LBSPR Version: ", packageVersion("LBSPR")))
      ),
   	  mainPanel(
	  	h4(textOutput("Loading"), style = "color:red"),
		column(12, plotOutput("SizeComp", height=700, width="100%")),
		fluidRow(downloadButton("dnloadImage", label = "Download", class = NULL), style="padding-bottom: 25px;"),
	    column(12,
	      wellPanel(
		    h4("Controls"), 
			fluidRow(column(3,
	          radioButtons("Ltype", "Catch or Population?", 
		        choices=c("Catch" = TRUE, "Population" = FALSE), inline=FALSE)
			),
            column(3, 				   
    	      radioButtons("perRec", "Per-Recruit?", 
		        choices=c("No"=FALSE, "Yes"=TRUE), inline=FALSE)
			))#,
            # column(4,
    	      # radioButtons("plots", "Plots", 
		        # choices=c("All"="all", "Length Frequency"="len.freq", 
				# "Maturity/Selectivty"="maturity.select",
				 # "Growth"="growth", "Yield Curve"="yield.curve"))
			# )),
			# fluidRow(
			  # column(4, sliderInput("size.pt", label="Point Size", value=5, min=0, max=10, step=1))
			# )
          ), tags$hr()			
		)
	)
  )
 )
 )
))




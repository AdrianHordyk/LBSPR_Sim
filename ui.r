library(shiny)
library(shinyBS)

shinyUI(tagList(tags$head(includeScript("google-analytics.js")),
  navbarPage("LBSPR Simulation Modeling", id="navbar",
  tabPanel("Instructions",
	  h3("Instructions")#,
	  # p("To use the LBSPR Application you will need: 1) length composition data from your fishery (either raw measurements or counts), and 2) estimates of the life history parameters. The following paragraphs outline the steps to use the LBSPR Application.  Each heading refers to a tab on the menu."),
	  # h4("Upload Data"),
	  # p("The first step is to upload a CSV (comma separated variable) file containing length data.  The file must be in CSV format and contain only numeric values except for the header row which can contain labels.  Multiple years of data should be placed in seperate columns."),
	  # p("Length frequency data must have the midpoints of the length classes (the length bins) in the first column, and numeric values for all counts (i.e., all columns are the same length). Length measurements should be raw numbers, each column representing a different year."),
	  # p("A number of example data files have been included. Download the CSV files to see the contents of these files"),
	  # h4("Fit Model"),
	  # p("Enter the life history parameters for your species, and check that the length frequency distribution looks correct.  If  everything is correct, run the LBSPR model."),
	  # p("Use the example life history parameters for the example data files"),
	  # h4("Examine Results"),
	  # p("The estimated parameters of the LBSPR model in tabular and graphical format. All figures can be downloaded. The estimated parameters can be downloaded in CSV format.")
  # ),
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
			)#,
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




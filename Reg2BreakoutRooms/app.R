#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(rsconnect) #to connect to shinyapps.io
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united")
   , tabPanel("Yalla"
              , tags$h1("Zoom - Registration to Pre-assigned rooms (constrained)")
              , tags$div(checked=NA
                        #, tags$a(href="https://www.linkedin.com/in/elfersol/", "by Laurent Fernandez - linkedin")
                        , tags$h6("by Laurent Fernandez")
                        , tags$br()
                )
   )

    # Sidebar with a slider input for number of bins 
    , sidebarPanel(
            fileInput(
                inputId = "i_upload_csv"
                , label = "Upload Zoom registration file"
                , multiple = TRUE
                , accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
                , width = NULL
                , buttonLabel = "Browse..."
                , placeholder = "No file selected"
            )
            , uiOutput("select")
            , uiOutput("maxbyroom")
            , uiOutput("random")
            , uiOutput("csvfilename")
            , uiOutput("go")
            , tags$br()
            , uiOutput("dl")
        )
        , mainPanel(tags$div(checked=NA,
            tags$h4("HOW TO USE")
            , tags$p("1. Download the registration file (.csv) from Zoom")
            , tags$p("2. Upload on this page")
            , tags$p("3. Set parameters (you can constrain room assignment with any field from the registration file that you upload - an option allows to randomize assignment. If unchecked, the program just follows the order of appearance in the registration file")
            , tags$p("4. Launch calculation, check in the table that shows")
            , tags$p("5. Download the breakout rooms file (.csv) and upload in Zoom")
            , tags$p("IMPORTANT: If a participant isn't assigned any room, it means that this person hasn't set any value for the constraint column when registering. This person will still be able to attend the Zoom meeting, but will have to be manually assigned a room (if needed).")
            , uiOutput("summary")
            , uiOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observe({
        if (is.null(input$i_upload_csv)) return()
        df_csv <- read.csv(input$i_upload_csv$datapath)
        output$select <- renderUI({
            selectInput(
                inputId = "i_constraint"
                , label = "Select constraint column"
                , choices = { names(df_csv) }
                , multiple = FALSE
                , selectize = TRUE
            )
        })
        
        output$maxbyroom <- renderUI({
            numericInput("i_maxnbroom", "Max participants by room", value = 5, step = 1, width = "200px")
        })
        
        output$random <- renderUI({
            checkboxInput("i_randomize", "Randomize participants order", value = TRUE, width = NULL)
        })
        
        output$csvfilename <- renderUI({
            textInput("i_filename", "Output csv file name (csv extension automatically added)", value = "BreakoutRoomsSetup", width = NULL, placeholder = NULL)
        })
        
        output$go <- renderUI({
            actionButton("i_go", "Generate file")
        })
        
    })
    observeEvent(input$i_go, {
        df_csv <- read.csv(input$i_upload_csv$datapath)
        
        NB_MAX_BY_GROUP <- input$i_maxnbroom
        CONSTRAINT_COL <- which(colnames(df_csv)==input$i_constraint)
        RANDOMIZE <- input$i_randomize

        # Sel only useful columns from registration file
        df <- cbind(df_csv["Email"],df_csv[CONSTRAINT_COL])
        
        # Add empty column and place in first position
        df[,3] <- ""
        df <- df[,c(2,3,1)]
        
        # Rename columns
        names(df)<- c("RegConstraint","Pre-assign Room Name","Email Address")
        
        if (RANDOMIZE==TRUE) {df <- df[sample(1:nrow(df)), ]}
        
        # Set up constraint table with counter
        constraint_count_nb<- aggregate(df$RegConstraint, by=list(df$RegConstraint), FUN=length)
        #constraint_reg_values <- df %>% distinct(RegConstraint)
        df_c <- data.frame(matrix(NA, nrow=nrow(constraint_count_nb), ncol=3))
        df_c[,1] <- constraint_count_nb$Group.1 # constraint values
        df_c[,2] <- constraint_count_nb$x # number of values
        df_c[,3] <- 0 # counter for loop
        
        # Assign room numbers / constraint: max nb of people by room
        n=1
        # for (i in 1:nrow(df_c)) { # distinct constraint values in constraint table
        for (i in 1:nrow(df_c)) {
            for (j in 1:nrow(df)) {
                print(paste0(i,",",j,",",df[j,1],",",df_c[i,1] ))
                if (df[j,1]==df_c[i,1]) {
                    print("match")
                    df_c[i,3] <- df_c[i,3]+1
                    if (df[j,1]!="") {
                        print(paste0("room",n))
                        df[j,2] <- paste0("room",n)
                        # increment room number if all emails attributed for a given constraint or if pass max number of participants by room
                        if (df_c[i,3]==df_c[i,2] || (df_c[i,3]/NB_MAX_BY_GROUP)%%1==0) {
                            n=n+1
                        }
                    }
                }
            }
        }
        
        # Reorder by room number for lisibility
        df <- df[order(df[1]),]
        print(df)

        # overview info
        info_rooms <- aggregate(df[,2], by=list(df[,2],df[,1]), FUN=length)
        names(info_rooms) <- c("Room","Constraint","Count")

        # File to setup pre-assign breakout rooms in zoom
        df_zoom_setup <- df [2:3]
        
        #### Final outputs
        
        # Breakout room files (download)
        output$dl <- renderUI({
            downloadButton('dl_zoom_setup', 'Download breakout rooms file')
        })

        output$dl_zoom_setup <- downloadHandler(
            filename = paste0(input$i_filename,".csv")
            , content = function(file) {
                write.table(df_zoom_setup, file, sep=",", row.names=FALSE, quote=FALSE)
            }
        )

        #summary of rooms
        output$summary <- renderUI({
            tags$b("Summary")})
        output$table <- renderUI({
            tableOutput("table_info")
        })
        output$table_info <- renderTable(info_rooms)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

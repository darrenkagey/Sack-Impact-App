#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

### load packages
library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(rsconnect)

### load data
load("sacks_allowed.RData")
load("blocker_sacks_allowed.RData")
load("qb_sacks_allowed.RData")

### Building User Interface
ui<-fluidPage(
    titlePanel(
      p( 
        h2("NFL Sack Allowed EPA and WPA", align = "left"),
        h4("2023 Regular Season", align = "left"),
        h5("Source: PFF and nflfastr", align = "left")
        )
      ),
  tabsetPanel(
    
    # panel with all players who gave sacks
    tabPanel("All Sacks Allowed",
             sidebarLayout( 
               sidebarPanel(
                 h4("Offensive Characteristics"), # label
                 
                 ## Input Functions
                 
                 # Offensive Player Selection Input
                 pickerInput(inputId = "all_sacks.off_player",
                             label = "Choose The Offensive Player(s) (If Desired):",
                             choices = sort(unique(sacks_allowed$off_player)),
                             selected = sort(unique(sacks_allowed$off_player)),
                             multiple = TRUE,
                             options = pickerOptions(liveSearch = TRUE,  # make searchable
                                                     liveSearchStyle = "contains",
                                                     actionsBox = TRUE)), # select/deselect all
                 
                 # Offensive Position Selection Input
                 pickerInput(inputId = "all_sacks.off_position",
                             label = "Position of Offensive Player(s):",
                             choices = c("QB", "HB", "FB", "WR", "TE", "LT", "LG", "C", "RG", "RT"),
                             selected = c("QB", "HB", "FB", "WR", "TE", "LT", "LG", "C", "RG", "RT"),
                             multiple = TRUE,
                             options = pickerOptions(actionsBox = TRUE)), # select/deselect all
                 
                 # Offensive Team Selection Input
                 pickerInput(inputId = "all_sacks.off.team",
                             label = "Offensive Team(s):",
                             choices = sort(unique(sacks_allowed$off.team)),
                             selected = sort(unique(sacks_allowed$off.team)),
                             multiple = TRUE,
                             options = pickerOptions(liveSearch = TRUE, # make searchable
                                                     liveSearchStyle = "contains",
                                                     actionsBox = TRUE)), # select/deselect all
                 
                 h4("Defensive Characteristics"), # label
                 
                 # Defensive Team Selection Input
                 pickerInput(inputId = "all_sacks.def.team",
                             label="Defensive Team:",
                             choices = sort(unique(sacks_allowed$def.team)),
                             selected = sort(unique(sacks_allowed$def.team)),
                             multiple = TRUE,
                             options = pickerOptions(liveSearch = TRUE, # make searchable
                                                     liveSearchStyle = "contains",
                                                     actionsBox = TRUE)), # select/deselect all
                 
                 # Defensive Personnel Selection Input
                 checkboxGroupInput(inputId = "all_sacks.defensive_personnel",
                                    label = "Defensive Personnel:",
                                    choices = c("0-3 DBs", "Base", "Nickel", "Dime", "7+ DBs"),
                                    selected = c("0-3 DBs", "Base", "Nickel", "Dime", "7+ DBs"),
                                    inline = TRUE),
                 
                 # Defensive Player Technique Selection Input
                 pickerInput(inputId= "all_sacks.def_player_tech",
                             # hyperlink the label to a defensive technique chart for reference
                            label = tags$a(href = "https://docs.google.com/spreadsheets/d/1AKrA80h8MZOQUCGGSYmZ0YJMO5FWcqVR1GQ6oOx4ifA/edit#gid=495059619",
                                            "Defensive Techniques"),
                            choices = c("0", "1", "A", "2i", "2", "3", "B", "4i", "4", "5", "C", "6i",                                        
                                         "6", "7", "D", "8i", "8", "8o", "E", "Yi", "Y", "Yo", "9",      
                                         "Corner Position", "Not Listed"),
                            selected = c("0", "1", "A", "2i", "2", "3", "B", "4i", "4", "5", "C",  "6i",                                       
                                         "6", "7", "D", "8i", "8", "8o", "E", "Yi", "Y", "Yo", "9",                                            
                                         "Corner Position", "Not Listed"),
                            multiple = TRUE,
                            options = pickerOptions(actionsBox = TRUE)), # select/deselect all
                 
                 h4("Play(s) Characteristics"), # label
                 
                 # Quarter Selection Input
                 checkboxGroupInput(inputId= "all_sacks.quarter",
                                    label = "Choose The Quarter(s):",
                                    choices = sort(unique(sacks_allowed$quarter)),
                                    selected = sort(unique(sacks_allowed$quarter)),
                                    inline = TRUE),
                 
                 # Down Selection Input
                 checkboxGroupInput(inputId= "all_sacks.down",
                                    label = "Choose The Down(s):",
                                    choices = c("1", "2", "3", "4", "2pt"),
                                    selected = c("1", "2", "3", "4", "2pt"),
                                    inline= TRUE),
                 
                 # Yards To Go Selection Input
                 sliderInput(inputId = "all_sacks.distance",
                             label = "Choose Yards to Go for a 1st Down",
                             value=34, min=0, max=34),
                 
                 # Field Position Selection Input
                 sliderInput(inputId = "all_sacks.yardline_100",
                             label = "Choose Distance from Endzone (in Yards)",
                             value=99, min=1, max=99),
                 
                 # Dropback Type Selection Input
                 pickerInput(inputId = "all_sacks.dropback_type",
                             label = "Choose The Dropback Type(s):",
                             choices= c("Straight Drop", "Roll Out", "Scramble", 
                                        "Scramble off of Roll Out", "Flea Flicker", "WR Pass", 
                                        "RB Pass", "Backward then Forward Pass"),
                             selected = c("Straight Drop", "Roll Out", "Scramble", 
                                          "Scramble off of Roll Out", "Flea Flicker", "WR Pass", 
                                          "RB Pass", "Backward then Forward Pass"),
                             multiple = TRUE,
                             options = pickerOptions(actionsBox = TRUE)), # select/deselect all
                 
                 # Blitzing Selection Input
                 checkboxGroupInput(inputId = "all_sacks.blitz",
                                    label = "Blitzing Behavior:",
                                    choices = c("Blitz", "No Blitz"),
                                    selected = c("Blitz", "No Blitz"),
                                    inline = TRUE),
                 
                 # Play Action Selection Input
                 checkboxGroupInput(inputId = "all_sacks.play_action",
                                    label = "Offensive Play Type(s):",
                                    choices = c("Play Action", "Not Play Action"),
                                    selected = c("Play Action", "Not Play Action"),
                                    inline = TRUE),
                 
                 # QB Time To Throw Selection Input
                 checkboxGroupInput(inputId = "all_sacks.time_to_throw_group",
                                    label = "How Long the QB Holds The Ball:",
                                    choices = sort(unique(sacks_allowed$time_to_throw_group)),
                                    selected = sort(unique(sacks_allowed$time_to_throw_group)),
                                    inline = TRUE)
               ),
               
               ## Output Function
               
               mainPanel(
                 DTOutput(outputId = "All_Sacks_Table") # Datatable
               )
             )  
    ),
    
    # panel with blockers who gave up sacks
    tabPanel("Sacks Allowed by Blockers (Non-Throwers)",
             sidebarLayout( 
               sidebarPanel(
                 h4("Offensive Characteristics"), #label
                 
                 ## Input Functions
                 
                 # Offensive Player Selection Input
                 pickerInput(inputId = "blocker_sacks.off_player",
                             label = "Choose The Offensive Player(s) (If Desired):",
                             choices = sort(unique(blocker_sacks_allowed$off_player)),
                             selected = sort(unique(blocker_sacks_allowed$off_player)),
                             multiple = TRUE,
                             options = pickerOptions(liveSearch = TRUE, # make searchable
                                                     liveSearchStyle = "contains",
                                                     actionsBox = TRUE)), # select/deselect all
                 
                 # Offensive Position Selection Input 
                 pickerInput(inputId = "blocker_sacks.off_position",
                             label = "Position of Offensive Player(s):",
                             choices = c("HB", "FB", "WR", "TE", "LT", "LG", "C", "RG", "RT"),
                             selected = c("HB", "FB", "WR", "TE", "LT", "LG", "C", "RG", "RT"),
                             multiple = TRUE,
                             options = pickerOptions(actionsBox = TRUE)), # select/deselect all
                 
                 # Offensive Team Selection Input
                 pickerInput(inputId = "blocker_sacks.off.team",
                             label = "Offensive Team(s):",
                             choices = sort(unique(blocker_sacks_allowed$off.team)),
                             selected = sort(unique(blocker_sacks_allowed$off.team)),
                             multiple = TRUE,
                             options = pickerOptions(liveSearch = TRUE, # make searchable
                                                     liveSearchStyle = "contains",
                                                     actionsBox = TRUE)), # select/deselect all
                 
                 h4("Defensive Characteristics"), # label
                 
                 # Defensive Team Selection Input
                 pickerInput(inputId = "blocker_sacks.def.team",
                             label="Defensive Team(s):",
                             choices = sort(unique(blocker_sacks_allowed$def.team)),
                             selected = sort(unique(blocker_sacks_allowed$def.team)),
                             multiple = TRUE,
                             options = pickerOptions(liveSearch = TRUE, # make searchable
                                                     liveSearchStyle = "contains",
                                                     actionsBox = TRUE)), # select/deselect all
                 
                 # Defensive Personnel Selection Input
                 checkboxGroupInput(inputId = "blocker_sacks.defensive_personnel",
                                    label = "Defensive Personnel:",
                                    choices = c("0-3 DBs", "Base", "Nickel", "Dime", "7+ DBs"),
                                    selected = c("0-3 DBs", "Base", "Nickel", "Dime", "7+ DBs"),
                                    inline = TRUE),
                                       
                 # Defensive Player Technique Selection Input
                 pickerInput(inputId= "blocker_sacks.def_player_tech",
                             # hyperlink the label to a defensive technique chart for reference
                             label = tags$a(href = "https://docs.google.com/spreadsheets/d/1AKrA80h8MZOQUCGGSYmZ0YJMO5FWcqVR1GQ6oOx4ifA/edit#gid=495059619",
                                            "Defensive Techniques"),
                             choices = c("0", "1", "A", "2i", "2", "3", "B", "4i", "4", "5", "C", "6i",                                        
                                         "6", "7", "D", "8i", "8", "8o", "E", "Yi", "Y", "Yo", "9",                                            
                                         "Corner Position", "Not Listed"),
                             selected = c("0", "1", "A", "2i", "2", "3", "B", "4i", "4", "5", "C", "6i",                                        
                                          "6", "7", "D", "8i", "8", "8o", "E", "Yi", "Y", "Yo", "9",                                            
                                          "Corner Position", "Not Listed"),
                             multiple = TRUE,
                             options = pickerOptions(actionsBox = TRUE)), # select/deselect all
                 
                 h4("Play(s) Characteristics"), # label
                 
                 # Quarter Selection Input
                 checkboxGroupInput(inputId= "blocker_sacks.quarter",
                                    label = "Choose The Quarter(s):",
                                    choices = sort(unique(blocker_sacks_allowed$quarter)),
                                    selected = sort(unique(blocker_sacks_allowed$quarter)),
                                    inline = TRUE),
                 # Down Selection Input
                 checkboxGroupInput(inputId= "blocker_sacks.down",
                                    label = "Choose The Down(s):",
                                    choices = c("1", "2", "3", "4", "2pt"),
                                    selected = c("1", "2", "3", "4", "2pt"),
                                    inline= TRUE),
                 
                 # Yards To Go Selection Input
                 sliderInput(inputId = "blocker_sacks.distance",
                             label = "Choose Yards to Go for a 1st Down",
                             value=34, min=0, max=34),
                 # Field Position Selection Input
                 sliderInput(inputId = "blocker_sacks.yardline_100",
                             label = "Choose Distance from Endzone (in Yards)",
                             value=99, min=1, max=99),
                 # Dropback Type Selection Input
                 pickerInput(inputId = "blocker_sacks.dropback_type",
                             label = "Choose The Dropback Type(s):",
                             choices= c("Straight Drop", "Roll Out", "Scramble", 
                                        "Scramble off of Roll Out", "Flea Flicker"),
                             selected = c("Straight Drop", "Roll Out", "Scramble", 
                                          "Scramble off of Roll Out", "Flea Flicker"),
                             multiple = TRUE,
                             options = pickerOptions(actionsBox = TRUE)), # select/deselect all
                 
                 # Blitzing Selection Input
                 checkboxGroupInput(inputId = "blocker_sacks.blitz",
                                    label = "Blitzing Behavior:",
                                    choices = c("Blitz", "No Blitz"),
                                    selected = c("Blitz", "No Blitz"),
                                    inline = TRUE),
                 
                 # Play Action Selection Input
                 checkboxGroupInput(inputId = "blocker_sacks.play_action",
                                    label = "Offensive Play Type(s):",
                                    choices = c("Play Action", "Not Play Action"),
                                    selected = c("Play Action", "Not Play Action"),
                                    inline = TRUE),
                 # QB Time To Throw Selection Input
                 checkboxGroupInput(inputId = "blocker_sacks.time_to_throw_group",
                                    label = "How Long the QB Holds The Ball:",
                                    choices = sort(unique(blocker_sacks_allowed$time_to_throw_group)),
                                    selected = sort(unique(blocker_sacks_allowed$time_to_throw_group)),
                                    inline = TRUE)
               ),
               
               ## Output Function
               
               mainPanel(
                 DTOutput(outputId = "Blocker_Sacks_Table") # Datatable
               )
             )  
    ),
    
    # panel with passers who gave up sacks
    tabPanel("Sacks Allowed by Passers",
             sidebarLayout( 
               sidebarPanel(
                 h4("Offensive Characteristics"), # label
                 
                 ## Input Functions
                 
                 # Offensive Player Selection Input
                 pickerInput(inputId = "qb_sacks.off_player",
                             label = "Choose The Offensive Player(s) (If Desired):",
                             choices = sort(unique(qb_sacks_allowed$off_player)),
                             selected = sort(unique(qb_sacks_allowed$off_player)),
                             multiple = TRUE,
                             options = pickerOptions(liveSearch = TRUE, # make searchable
                                                     liveSearchStyle = "contains",
                                                     actionsBox = TRUE)), # select/deselect all
                 
                 # Offensive Position Selection Input
                 pickerInput(inputId = "qb_sacks.off_position",
                             label = "Position of Offensive Player(s):",
                             choices = c("QB", "HB", "WR"),
                             selected = c("QB", "HB", "WR"),
                             multiple = TRUE,
                             options = pickerOptions(actionsBox = TRUE)), # select/deselect all
                 
                 # Offensive Team Selection Input
                 pickerInput(inputId = "qb_sacks.off.team",
                             label = "Offensive Team(s):",
                             choices = sort(unique(qb_sacks_allowed$off.team)),
                             selected = sort(unique(qb_sacks_allowed$off.team)),
                             multiple = TRUE,
                             options = pickerOptions(liveSearch = TRUE, # make searchable
                                                     liveSearchStyle = "contains",
                                                     actionsBox = TRUE)), # select/deselect all
                 
                 h4("Defensive Characteristics"), # label
                 
                 # Defensive Team Selection Input
                 pickerInput(inputId = "qb_sacks.def.team",
                             label="Defensive Team(s):",
                             choices = sort(unique(qb_sacks_allowed$def.team)),
                             selected = sort(unique(qb_sacks_allowed$def.team)),
                             multiple = TRUE,
                             options = pickerOptions(liveSearch = TRUE, # make searchable
                                                     liveSearchStyle = "contains",
                                                     actionsBox = TRUE)), # select/deselect all
                 
                 # Defensive Personnel Selection Input
                 checkboxGroupInput(inputId = "qb_sacks.defensive_personnel",
                                    label = "Defensive Personnel:",
                                    choices = c("0-3 DBs", "Base", "Nickel", "Dime", "7+ DBs"),
                                    selected = c("0-3 DBs", "Base", "Nickel", "Dime", "7+ DBs"),
                                    inline = TRUE),
                 
                 h4("Play(s) Characteristics"), # label
                 
                 # Quarter Selection Input
                 checkboxGroupInput(inputId= "qb_sacks.quarter",
                                    label = "Choose The Quarter(s):",
                                    choices = sort(unique(qb_sacks_allowed$quarter)),
                                    selected = sort(unique(qb_sacks_allowed$quarter)),
                                    inline = TRUE),
                 
                 # Down Selection Input
                 checkboxGroupInput(inputId= "qb_sacks.down",
                                    label = "Choose The Down(s):",
                                    choices = c("1", "2", "3", "4", "2pt"),
                                    selected = c("1", "2", "3", "4", "2pt"),
                                    inline= TRUE),
                 
                 # Yards To Go Selection Input
                 sliderInput(inputId = "qb_sacks.distance",
                             label = "Choose Yards to Go for a 1st Down To Include",
                             value=27, min=0, max=27),
                 
                 # Field Position Selection Input
                 sliderInput(inputId = "qb_sacks.yardline_100",
                             label = "Choose Distances from Endzone (in Yards) To Include",
                             value=99, min=1, max=99),
                 
                 # Dropback Type Selection Input
                 pickerInput(inputId = "qb_sacks.dropback_type",
                             label = "Choose The Dropback Type(s):",
                             choices= c("Straight Drop", "Roll Out", "Scramble", 
                                        "Scramble off of Roll Out", "Flea Flicker", "WR Pass", 
                                        "RB Pass", "Backward then Forward Pass"),
                             selected = c("Straight Drop", "Roll Out", "Scramble", 
                                          "Scramble off of Roll Out", "Flea Flicker", "WR Pass", 
                                          "RB Pass", "Backward then Forward Pass"),
                             multiple = TRUE,
                             options = pickerOptions(actionsBox = TRUE)), # select/deselect all
                 
                 # Blitzing Selection Input
                 checkboxGroupInput(inputId = "qb_sacks.blitz",
                                    label = "Blitzing Behavior:",
                                    choices = c("Blitz", "No Blitz"),
                                    selected = c("Blitz", "No Blitz"),
                                    inline = TRUE),
                 
                 # Play Action Selection Input
                 checkboxGroupInput(inputId = "qb_sacks.play_action",
                                    label = "Offensive Play Type(s):",
                                    choices = c("Play Action", "Not Play Action"),
                                    selected = c("Play Action", "Not Play Action"),
                                    inline = TRUE),
                 
                 # QB Time To Throw Selection INput
                 checkboxGroupInput(inputId = "qb_sacks.time_to_throw_group",
                                    label = "How Long the QB Holds The Ball:",
                                    choices = sort(unique(qb_sacks_allowed$time_to_throw_group)),
                                    selected = sort(unique(qb_sacks_allowed$time_to_throw_group)),
                                    inline = TRUE)
               ),
               
               ## Output Function
               
               mainPanel(
                 DTOutput(outputId = "QB_Sacks_Table") # Datatable
               )
             )       
    )
  )
)

### Building server
server <- function(input, output) {
  
  # Datatable Output with all players who gave up a sack
  output$All_Sacks_Table <- renderDT({
    datatable(sacks_allowed %>% 
                
                # filter to user selected variable values
                filter(off_player %in% c(input$all_sacks.off_player) &
                         off_position %in% c(input$all_sacks.off_position) & 
                         def.team %in% c(input$all_sacks.def.team) &
                         off.team %in% c(input$all_sacks.off.team) &  
                         def_player_tech %in% c(input$all_sacks.def_player_tech) &
                         defensive_personnel %in% c(input$all_sacks.defensive_personnel) &
                         quarter %in% c(input$all_sacks.quarter) & 
                         down %in% c(input$all_sacks.down) & 
                         distance <= input$all_sacks.distance & 
                         yardline_100 <= input$all_sacks.yardline_100 & 
                         play_action %in% c(input$all_sacks.play_action) & 
                         blitz %in% c(input$all_sacks.blitz) & 
                         dropback_type %in% c(input$all_sacks.dropback_type) &                        
                         time_to_throw_group %in% c(input$all_sacks.time_to_throw_group)) %>% 
                
                # add sack allowed count
                add_count(off_player, name = "filtered.sacks.allowed") %>% 
                
                # group by player
                group_by(off_player, off_position, off.team, filtered.sacks.allowed) %>% 
                
                # calculate total epa and wpa for each player in filtered data
                summarise(total_epa = sum(epa), total_wpa = sum(wpa)),
              
              # set column names
              colnames = c("Player", "Position", "Off. Team", "Sacks Allowed",
                           "Total EPA Lost", "Total WPA Lost"))
  })
  
  # Datatable Output with blockers who gave up sacks        
  output$Blocker_Sacks_Table <- renderDT({
    datatable(blocker_sacks_allowed %>% 
                
                # filter to user selected variable values    
                filter(off_player %in% c(input$blocker_sacks.off_player) & 
                         off_position %in% c(input$blocker_sacks.off_position) & 
                         off.team %in% c(input$blocker_sacks.off.team) & 
                         def.team %in% c(input$blocker_sacks.def.team) &
                         def_player_tech %in% c(input$blocker_sacks.def_player_tech) &  
                         defensive_personnel %in% c(input$blocker_sacks.defensive_personnel) &
                         quarter %in% c(input$blocker_sacks.quarter) & 
                         down %in% c(input$blocker_sacks.down) & 
                         distance <= input$blocker_sacks.distance & 
                         yardline_100 <= input$blocker_sacks.yardline_100 & 
                         play_action %in% c(input$blocker_sacks.play_action) & 
                         blitz %in% c(input$blocker_sacks.blitz) & 
                         dropback_type %in% c(input$blocker_sacks.dropback_type) &                     
                         time_to_throw_group %in% c(input$blocker_sacks.time_to_throw_group)) %>% 
                
                # add sack allowed count
                add_count(off_player, name = "filtered.sacks.allowed") %>% 
                
                # group by player
                group_by(off_player, off_position, off.team, filtered.sacks.allowed) %>% 
                
                # calculate total epa and wpa for each player in filtered data
                summarise(total_epa = sum(epa), total_wpa = sum(wpa)),
              
              # set column names
              colnames = c("Player", "Position", "Off. Team", "Sacks Allowed",
                           "Total EPA Lost", "Total WPA Lost"))
  })
  
  # Datatable Output with passers who gave up sacks
  output$QB_Sacks_Table <- renderDT({
    datatable(qb_sacks_allowed %>% 
                
                # filter to user selected variable values       
                filter(off_player %in% c(input$qb_sacks.off_player) & 
                         off_position %in% c(input$qb_sacks.off_position) & 
                         off.team %in% c(input$qb_sacks.off.team) & 
                         def.team %in% c(input$qb_sacks.def.team) &
                         defensive_personnel %in% c(input$qb_sacks.defensive_personnel) &
                         quarter %in% c(input$qb_sacks.quarter) & 
                         down %in% c(input$qb_sacks.down) & 
                         distance <= input$qb_sacks.distance & 
                         yardline_100 <= input$qb_sacks.yardline_100 & 
                         play_action %in% c(input$qb_sacks.play_action) & 
                         blitz %in% c(input$qb_sacks.blitz) & 
                         dropback_type %in% c(input$qb_sacks.dropback_type) &                
                         time_to_throw_group %in% c(input$qb_sacks.time_to_throw_group)) %>% 
                
                # add sack allowed count
                add_count(off_player, name = "filtered.sacks.allowed") %>% 
                
                # group by player
                group_by(off_player, off_position, off.team, filtered.sacks.allowed) %>% 
                
                # calculate total epa and wpa for each player in filtered data
                summarise(total_epa = sum(epa), total_wpa = sum(wpa)),
              
              # set column names
              colnames = c("Player", "Position", "Off. Team", "Sacks Allowed",
                           "Total EPA Lost", "Total WPA Lost"))
  })
}



# Run the app
shinyApp(ui=ui, server=server)



library(shiny)
library(bslib)
library(shinyWidgets)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = bs_theme(version = 5, bootswatch = "sandstone", primary = "#2C3E4C", secondary = "#2C3E4C", fg ="#2C3E4C", bg ="#F2F3F4"  ),

  h1("WAR Analysis / 2020 Nagorno-Karabakh War", style = "font-family: monospace; font-size: 30px; color: #2C3E4C;" ),
  navbarPage( "Plots by type of dataset",
  
    navbarMenu( "World Data",

        tabPanel("Yearly Count Of Wars By The Begining Date", 
                 sidebarLayout(sidebarPanel(sliderInput("range", "Select year range", min = 1946, max = 2019, value = c(1946, 2019),round = TRUE)),
                               mainPanel(tabsetPanel(tabPanel("Plot", plotOutput("pvPlot", height = "500px")),
                                                     tabPanel("Table", DT::DTOutput("pvTable")))))),
        
        tabPanel("Total Political violence 1946-2019", 
                 sidebarLayout(sidebarPanel(selectInput("war_type", "Select the type of the war", choices = c("International Independence" = "IN","Ethnic Violence"="EV","Civil War"="CW","International War/Government Does't Except"="IW*",
                                                                                                              "Civil Violence"="CV","Ethnic War"="EW","International Violence"="IV","International War"="IW","International War/Government Does't Except"="IV*"))),
                               mainPanel(tabsetPanel(
                                   tabPanel("Total Deaths Plot", plotly::plotlyOutput('pvTD', height = "500px")),
                                   tabPanel("War Magnitudes Plot", plotly::plotlyOutput('pvM', height = "500px")),
                                   tabPanel("Table", DT::DTOutput("pvTableType"))
                               ))))
        ),
        navbarMenu("Weapons' data",
        tabPanel("Army Supplies",
                 sidebarLayout(sidebarPanel(selectInput("country", "Select Country", choices = c("Azerbaijan", "Armenia"))),
                               mainPanel(plotOutput("supplies", height = "500px"))
                 )),
        
        tabPanel("Armenia's Ordered Weapons 1990-2020",
                 sidebarLayout(sidebarPanel(selectInput("weapon", "Select Weapon Type", selected = "Anti-tank missile", 
                                                        choices = c("Anti-tank missile", "APV", "FGA aircraft", 
                                                                    "Ground attack ac", "IFV", "Mobile SAM system", 
                                                                    "Portable SAM", "SAM", "SAM system", 
                                                                    "Self-propelled MRL", "SSM", "SSM TEL",
                                                                    "Tank", "Tank destroyer", "Towed gun", 
                                                                    "Trainer aircraft", "Transport aircraft"))),
                               mainPanel(tabsetPanel(
                                   tabPanel("Orders of Armenia",plotOutput('am_orders', height = "500px")),
                                   tabPanel("Table", DT::DTOutput('ord_am'))
                               )))),
        
        tabPanel("Azerbaijan's Ordered Weapons 1990-2020",
                 sidebarLayout(sidebarPanel(selectInput("weapon_az", "Select Weapon Type", selected = "Anti-tank missile", 
                                                        choices = c("Air search radar", "Anti-tank missile","APC", "APV","ASM" ,"Combat helicopter","FAC", "Fighter aircraft", 
                                                                    "Ground attack ac", "Guided rocket", "Guided rocket/SMM", "Helicopter turret", "IFV", "Loitering munition",
                                                                    "Mortar", "Patrol Craft",
                                                                    "Portable SAM", "SAM", "SAM system",
                                                                    "Self-propelled gun", "Self-propelled mortar",
                                                                    "Self-propelled MRL", "SSM", "SSM launcher", "SSM/ASM",
                                                                    "Tank", "Tank destroyer", "Towed gun", 
                                                                    "Trainer aircraft", "Transport aircraft", "Transport helicopter", "UAV" ))),
                               mainPanel(tabsetPanel(
                                   tabPanel("Orders of Azeirbaijan",plotOutput('az_orders', height = "500px")),
                                   tabPanel("Table", DT::DTOutput('ord_az'))
                               ))))
        ),
        navbarMenu("Military Expenditure data",
        tabPanel("Military Expenditure Of Countries", 
                 sidebarLayout(
                     sidebarPanel(sliderInput("year", "Select year range", min = 1992, max = 2020, value = c(1992, 2020))),
                     mainPanel(tabsetPanel(
                         tabPanel("GDP Plot", plotly::plotlyOutput("gdpPlot", height = "500px")),
                         tabPanel("Constant Price Expediture", plotly::plotlyOutput("exp_plot", height = "500px")),
                         tabPanel("Table", DT::DTOutput("gdp_table"))
                     ))
                 )
        )),
        navbarMenu("Incidents data",
        tabPanel("Reported War Incidents",
                 sidebarLayout(sidebarPanel(selectInput("type", "Select Type", selected = "Special Operations", 
                                                        choices = c("Special Operations", "Drones", "Heavy Weaponry"))),
                               mainPanel(tabsetPanel(
                                   tabPanel("Plot",plotly::plotlyOutput('plotByType', height = "500px")),
                                   tabPanel("Table", DT::DTOutput('table_type'))
                               )))),
        
        tabPanel("Killed/Wounded Plot",
                 sidebarLayout(sidebarPanel(selectInput("type_w", "Select Type", selected = "Combatants Wounded", 
                                                        choices = c("Combatants Wounded", "Combatants Killed", "Non-combatants Wounded", "Non-combatants Killed"))),
                               mainPanel(tabsetPanel(
                                   tabPanel("Plot",plotly::plotlyOutput('plotByTypeW', height = "500px")),
                                   tabPanel("Table", DT::DTOutput('table_type_w'))
                               )))),
        
        tabPanel("Armenians Killed/Wounded After Ceasefire Plot",
                 sidebarLayout(sidebarPanel(selectInput("type_c", "Select Type", selected = "Combatants Wounded", 
                                                        choices = c("Combatants Wounded", "Combatants Killed", "Non-combatants Wounded", "Non-combatants killed"))),
                               mainPanel(tabsetPanel(
                                   tabPanel("Plot",plotly::plotlyOutput('plotByTypeC', height = "500px")),
                                   tabPanel("Table", DT::DTOutput('table_type_c'))
                               ))))
 )    
)
)
)

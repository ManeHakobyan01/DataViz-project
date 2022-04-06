#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library("readxl")
library("wesanderson")
library(dplyr)
library(tidyverse)
library(scales)
library(viridis)
library(gridExtra)
library(ggcorrplot)
library(gridExtra)
library("RColorBrewer")
library(htmlwidgets)
library(hrbrthemes)

incidents <- read_excel("Incidents2015-2020.xlsx")
pv <- read_excel("Political violence.xlsx")
armySupplies <- read_excel("ArmsSuplies2011-20.xlsx")
gdp <- read_excel("GDPPercentageAMAZ.xlsx")
exp <- read_excel("ConstantPriceExpediture.xlsx")
cas <- read_excel("Casualties2015-2020.xlsx")
Orders_AM <- read_excel("Deals_Armenia.xlsx")
Orders_AZ <- read_excel("Azeirbaijan's ordered army supplies.xlsx")
after_c <- read_excel("ArmenianCasualtiesAfterceasfire.xlsx")

addUnits <- function(n) {
    labels <- ifelse(n < 1000, n, # less than thousands
                     
                     ifelse(n < 1e6, paste0(round(n/1e3), 'k'), # in thousands
                            ifelse(n < 1e9,
                                   ifelse(round(n/1e6)==round(n/1e6,digits=1),
                                          paste0(round(n/1e6), 'M'), # in millions
                                          paste0(round(n/1e6,digits=1), 'M')), # in 1.x millions
                                   ifelse(n < 1e12,
                                          ifelse(round(n/1e9)==round(n/1e9,digits=1),
                                                 paste0(round(n/1e9), 'B'), # in billions
                                                 paste0(round(n/1e9,digits=1), 'B')), # in 1.x billions
                                          ifelse(n < 1e15,
                                                 ifelse(round(n/1e12)==round(n/1e12,digits=1),
                                                        paste0(round(n/1e12), 'T'), # in trillions
                                                        paste0(round(n/1e12,digits=1), 'T')), # in 1.x trillions
                                                 'too big!'
                                          )))))
    
    return(labels)
} 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    plot_by_types <- function() {incidents %>% 
            filter(Type == input$type) %>%
            ggplot( aes(y = Value, x = Date)) + 
            geom_col(position = "stack", stat = "identity")+
            geom_col(fill = "#263e63") +
            theme_bw() }
    
    
    
    output$plotByType <- plotly::renderPlotly({
        plot_by_types()
    })
    output$table_type <- DT::renderDT({
        incidents %>% 
            filter(Type == input$type)
    })
    
    output$pvPlot <- renderPlot ({
        df1= pv %>%
            count(Begin) 
        inprange<- input$range
        df = df1[df1$Begin<= inprange[2] & df1$Begin >= inprange[1],]
        max_n = max(df$n)
        max_year = df$Begin[df$n == max_n]
        ggplot(df, aes(x = Begin, y=n))+
            geom_line(stat='identity', color = "#48929B") + 
            geom_point(color = "#011f4b") +
            scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13))+
            scale_x_continuous(breaks= c(1945,1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2020))+
            annotate(geom = "point", x = max_year, y = max_n, color="red") +
            annotate(geom = "text", x = max_year, y = max_n, label = "Maximum Peak", hjust = -0.2, color="red")+
            labs(title = "Yearly Count of Wars By The Beginning Date 1946-2019", x = "", y = "",
                 caption= "Data source: CSP - Major Episodes of Political Violence, 1946-2019, 2020")+
            theme_minimal()+
            theme(plot.title = element_text(hjust=0.5, size=20),
                  axis.text.x = element_text(angle = 40,color= "grey50" , face = "bold"))
    })
    
    output$pvTable <- DT::renderDT({
        pv %>% 
            filter(Begin == input$range)
    })
    
    pv_total <- function() {pv %>% 
            filter(Type == input$war_type) %>% 
            ggplot(aes(y= Region, x = Deaths))+
            geom_point(color = "#48929B")+
            geom_line(color = "#011f4b")+
            labs(title = "Total Death Count by Continents, 1945-2017.", x = "", y = "",
                 caption= "Data source: CSP - Major Episodes of Political Violence, 1946-2019, 2020")+
            scale_x_continuous(label=addUnits)+
            theme_minimal()+
            theme(plot.title = element_text(size=15, face= "bold",hjust=0.5))
    }
    
    output$pvTD <- plotly::renderPlotly({
        pv_total()
    })
    
    pv_mag <- function() {pv %>% 
            filter(Type == input$war_type) %>% 
            ggplot(aes(y = Region, x = Mag))+
            geom_point(color = "#48929B")+
            geom_line(color = "#011f4b")+
            labs(title = "Magnitudes of Wars by Continents, 1945-2017.", x = "", y = "",
                 caption= "Data source: CSP - Major Episodes of Political Violence, 1946-2019, 2020")+
            theme_minimal()+
            theme(plot.title = element_text(size=15, face= "bold", hjust=0.5))
    }
    
    output$pvM <- plotly::renderPlotly({
        pv_mag()
    })
    
    output$pvTableType <- DT::renderDT({
        pv %>% 
            filter(Type == input$war_type)
    })
    
    output$supplies <- renderPlot({
        armySupplies %>% 
            filter(Country == input$country) %>% 
            ggplot( aes(x = Country, y= Percentage, fill=Supplier))+
            geom_bar(stat='identity', position='dodge')+
            scale_fill_brewer(palette = "Blues", limits = c("Russia", "Israel", "Belarus", "Jordan","Turkey", "Ukraine", "Other"))+
            scale_y_continuous(labels =function(x) paste0(x, "%"), breaks = c(0,20,40,60,80,100))+
            labs(x ="", y="", title = "Major Suppliers to Both of the Countries",
                 caption= "Data source: sipri.org", fill= "Country Suppliers:")+
            theme_minimal()+
            theme(panel.grid.major.x = element_blank(),
                  legend.title = element_text(face ="bold", size =10),
                  legend.direction="vertical",
                  plot.caption = element_text(size=12,vjust= 0.5, hjust = 1.4),
                  plot.title = element_text(hjust = 1, size =20))
    })
    
    func_gdp <- function() {
        gdp %>% filter(Date<= input$year[2] & Date >= input$year[1]) %>%
            ggplot(aes(x = Date, y = Percentage, fill = Country)) +
            geom_bar(stat='identity', width = 0.5, position = "dodge")+scale_fill_manual(values = c("#317589", "#89C4F4"))+theme_minimal()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    }
    
    exp_func <- function() {
        exp %>% filter(Date<= input$year[2] & Date >= input$year[1]) %>%
            ggplot(aes(x = Date, y = Value, fill = Country)) +
            geom_bar(stat='identity', width = 0.5, position = "dodge")+
            scale_fill_manual(values = c("#317589", "#89C4F4"))+theme_minimal()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    output$gdpPlot <- plotly::renderPlotly({
        func_gdp()
    })
    
    output$exp_plot <- plotly::renderPlotly({
        exp_func()
    })
    
    
    output$gdp_table <- DT::renderDT({
        gdp %>% filter(Date<= input$year[2] & Date >= input$year[1])
    })
    
    
    plot_by_types_w <- function() {cas %>% 
            filter(Type == input$type_w) %>%
            ggplot( aes(y = Count, x = Date, fill = Country)) + 
            geom_col(position = "dodge", stat = "identity")+
            scale_fill_manual(values = c("#317589", "#89C4F4")) +
            theme(legend.position = "none",)+
            theme_bw() }
    
    
    
    output$plotByTypeW <- plotly::renderPlotly({
        plot_by_types_w()
    })
    output$table_type_w <- DT::renderDT({
        cas %>% 
            filter(Type == input$type_w)
    })
    
    output$am_orders <- renderPlot({Orders_AM %>%
            filter(Weapon_desc == input$weapon) %>%
            ggplot(aes(x=`Second-Hand`, y = Delievered)) +
            geom_bar(stat = "identity")+ geom_col(aes(fill = `Second-Hand`)) +
            scale_fill_manual(values = c("#317589", "#89C4F4"))+
            labs(x = "Second Hand Weapons ",
                 y = "Total of Weapons",
                 title = "Armenia's Ordered Weapons between 1990-2020 years",
                 caption = "Data Source: EVN Report",
            )+
            theme_minimal()+
            theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5),
                  axis.title.y= element_text(hjust = 0.5, size=12),
                  plot.title = element_text(size = 16,hjust = 0.5),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.title.x = element_blank(),
                  legend.position = "none",
                  plot.caption = element_text(size=12, color= "red",vjust= 0.5),
                  legend.title = element_text(face ="bold", size =10))
    })
    
    
    output$ord_am <- DT::renderDT({
        Orders_AM %>%
            filter(Weapon_desc == input$weapon)
    })
    
    output$az_orders <- renderPlot({Orders_AZ %>%
            filter(Weapon_type == input$weapon_az) %>%
            ggplot(aes(x=`Second-hand`, y = Delivered)) +
            geom_bar(stat = "identity")+ geom_col(aes(fill = `Second-hand`)) +
            scale_fill_manual(values = c("#317589", "#89C4F4"))+
            labs(x = "Second Hand Weapons ",
                 y = "Total of Weapons",
                 title = "Azeirbaijan's Ordered Weapons between 1990-2020 years",
                 caption = "Data Source: EVN Report",
            )+
            theme_minimal()+
            theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5),
                  axis.title.y= element_text(hjust = 0.5, size=12),
                  plot.title = element_text(size = 16,hjust = 0.5),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.title.x = element_blank(),
                  legend.position = "none",
                  plot.caption = element_text(size=12, color= "red",vjust= 0.5),
                  legend.title = element_text(face ="bold", size =10))
    })
    
    
    output$ord_az <- DT::renderDT({
        Orders_AZ %>%
            filter(Weapon_type == input$weapon_az)
    })
    
    plot_by_types_c <- function() {after_c %>% 
            filter(Type == input$type_c) %>%
            ggplot( aes(y = Count, x = Date)) + 
            geom_col(position = "stack", stat = "identity")+
            geom_col(fill = "#6497b1") +
            theme_bw() }
    
    
    
    output$plotByTypeC <- plotly::renderPlotly({
        plot_by_types_c()
    })
    output$table_type_c <- DT::renderDT({
        after_c %>% 
            filter(Type == input$type_c)
    })

})

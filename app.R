
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)



package_check <- require("grid")
if (package_check == FALSE) {
  install.packages('grid')
}
library("grid") 

package_check <- require("png")
if (package_check == FALSE) {
  install.packages('png')
}
library("png") 


package_check <- require("gridExtra")
if (package_check == FALSE) {
  install.packages('gridExtra')
}
library("gridExtra") 

package_check <- require("shinythemes")
if (package_check == FALSE) {
  install.packages('shinythemes')
}
library("shinythemes")  

package_check <- require("reshape2")
if (package_check == FALSE) {
  install.packages('reshape2')
}
library("reshape2")  

package_check <- require("RColorBrewer")
if (package_check == FALSE) {
  install.packages('RColorBrewer"')
}
library("RColorBrewer") 
 

package_check <- require("plotly")
if (package_check == FALSE) {
  install.packages('plotly')
}
library("plotly") 

##########################################################################
#  Functions                                                             #
##########################################################################
stratify <- function(data, column, n){
  results <- data.frame()
  for (label in unique(data[column])[[1]] ){
    temp <- data[data[column]==label,]
    if (nrow(temp) > n){
      rand_idx <- sample(1:nrow(temp), n)
      results <- rbind(results, temp[rand_idx,])  
    }else{
      results <- rbind(results, temp)
    }
  }
  return(results)
}

##########################################################################
#  DATA MANIPULATION FOR BAR PLOT                                        #
##########################################################################

pitch_data = read.csv('all_pitches.csv')

pitch_data2 <-pitch_data 


pitch_data['call'] <- ifelse(pitch_data$called==1, 'Strike', 'Ball')
pitch_data['Count'] <- paste(pitch_data$balls_before, pitch_data$strikes_before, sep="-")
pitch_data <- stratify(pitch_data, 'Count', 10000)


pitch_data2$balls_before <- as.character(pitch_data2$balls_before)
pitch_data2$strikes_before <- as.character(pitch_data2$strikes_before)
pitch_data2$count1 <- paste(pitch_data2$balls_before, pitch_data2$strikes_before, sep= '-')




pitch_data2$Z_check <- 2
pitch_data2$Z_check <- pitch_data2$plate_z


apply_zcheck <- function(x){
  ifelse(x > 3.5, 0, ifelse(x < 1.5, 0, 1))
}



pitch_data2 <- data.frame(pitch_data2[c(1,2,3,4,5,6,7,8,9)], apply(pitch_data2[10], 2, apply_zcheck) )
pitch_data2$X_check <- 2
pitch_data2$X_check <- pitch_data2$plate_x


apply_xcheck <- function(x){
  ifelse(x > 1.678/2, 0, ifelse(x < -1.678/2, 0, 1))
}


pitch_data2 <- data.frame(pitch_data2[c(1,2,3,4,5,6,7,8,9,10)], apply(pitch_data2[11], 2, apply_xcheck) )
pitch_data2$RB <- pitch_data2$Z_check + pitch_data2$X_check


apply_RBcheck <- function(x){
  ifelse(x ==2, 1, 0)
}

pitch_data2 <- data.frame(pitch_data2[c(1,2,3,4,5,6,7,8,9,10,11)], apply(pitch_data2[12], 2, apply_RBcheck) )
pitch_data2$Classified <- pitch_data2$called == pitch_data2$RB
pitch_table2 <- table(pitch_data2$bat_side, pitch_data2$count, pitch_data2$called, pitch_data2$Classified)


strike_df_mis <- data.frame(pitch_table2[1:2, 1:12, 2, 1])
strike_df_cor <- data.frame(pitch_table2[1:2, 1:12, 2, 2])
ball_df_mis <- data.frame(pitch_table2[1:2, 1:12, 1, 1])
ball_df_cor <- data.frame(pitch_table2[1:2, 1:12, 1, 2])

combined_df2 <- data.frame(strike_df_mis, strike_df_cor, ball_df_mis, ball_df_cor)
combined_df2 <- subset(combined_df2, select = -c(Var1.1, Var2.1, Var1.2, Var2.2, Var1.3, Var2.3))


colnames(combined_df2) <- c('Bat_side', 'Count', 'No_strikes_mis', 'No_strikes_cor', 'No_balls_mis', 'No_balls_cor')
combined_df2$Strike_cor <- round((combined_df2$No_strikes_cor / (combined_df2$No_strikes_cor + combined_df2$No_balls_cor + combined_df2$No_strikes_mis + combined_df2$No_balls_mis )),3)
combined_df2$Ball_cor <- round((combined_df2$No_balls_cor / (combined_df2$No_strikes_cor + combined_df2$No_balls_cor + combined_df2$No_strikes_mis + combined_df2$No_balls_mis )),3)
combined_df2$Strike_mis <- round((combined_df2$No_strikes_mis / (combined_df2$No_strikes_cor + combined_df2$No_balls_cor + combined_df2$No_strikes_mis + combined_df2$No_balls_mis )),3)
combined_df2$Ball_mis <- round((combined_df2$No_balls_mis / (combined_df2$No_strikes_cor + combined_df2$No_balls_cor + combined_df2$No_strikes_mis + combined_df2$No_balls_mis )),3)
combined_df2 <- subset(combined_df2, select = -c(No_strikes_mis, No_balls_mis, No_strikes_cor, No_balls_cor))
pitch_melt2 <- melt(combined_df2, id=c('Bat_side', 'Count'))
colnames(pitch_melt2) <- c('Bat_side', 'Count', 'Situation', 'Proportion')
pitch_melt2$Called <- ifelse(pitch_melt2$Situation == 'Strike_cor', 'Strike', ifelse(pitch_melt2$Situation == 'Strike_mis', 'Strike', 'Ball'))
pitch_melt2$Called <- factor(pitch_melt2$Called)
pitch_melt2$Classified <- ifelse(pitch_melt2$Situation == 'Strike_cor', 'Correct', ifelse(pitch_melt2$Situation == 'Ball_cor', 'Correct', 'Incorrect'))
pitch_melt2$Classified <- factor(pitch_melt2$Classified)

pitch_melt_r2 <- pitch_melt2[pitch_melt2$Bat_side == 'R',]
pitch_melt_l2 <- pitch_melt2[pitch_melt2$Bat_side == 'L',]




##########################################################
#  DATA MANIPULATION FOR HEAT MAP                        #
##########################################################



pitch_data2$X_Region <- 2
pitch_data2$X_Region <- pitch_data2$plate_x
pitch_data2$Y_Region <- 2
pitch_data2$Y_Region <- pitch_data2$plate_z

apply_regionx <- function(x){
  ifelse(x < -1.5, 1, ifelse(x < -1.0, 2, ifelse(x < -0.5, 3,
                                                 ifelse(x < 0.0, 4, ifelse(x < 0.5, 5,
                                                                           ifelse(x< 1.0, 6,
                                                                                  ifelse(x< 1.5, 7, 8)))))))
}

apply_regiony <- function(x){
  ifelse(x > 4, 1, ifelse(x > 3.5, 2, ifelse(x > 3.0, 3,
                                             ifelse(x > 2.5, 4, ifelse(x >2.0, 5,
                                                                       ifelse(x> 1.5, 6,
                                                                              ifelse(x> 1.0, 7, 8)))))))
}


pitch_data2 <- data.frame(pitch_data2[c(1,2,3,4,5,6,7,8,9,10,11,12,13,15)], apply(pitch_data2[14], 2, apply_regionx) )
pitch_data2 <- data.frame(pitch_data2[c(1,2,3,4,5,6,7,8,9,10,11,12,13,15)], apply(pitch_data2[14], 2, apply_regiony) )





g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}


ellipseDescript <- HTML("<br><u>PLOT EXPLANATION</u><br>Hyper gradient boosting was used to determine the decision boundary between a Strike and a Ball using 2015 & 2016 pitch data. The figure on the left compares empirical <u>superelliptical</u> strike zones to the strike zone as defined by the Major-League Baseball rule book.<br><br><u>KEY DISCOVERY #1</u><br>The right hand-batter strike zone is in general larger than the left hand-batter strike zone.<br><br><u>KEY DISCOVERY #2</u><br>The strikezone expands with each ball and contracts with each strike. Strike contraction is generally larger than ball expansion.  View 0-2, 0-0 and 3-0 to see progression.")
rectDescript <- HTML("<br><u>PLOT EXPLANATION</u><br>Hyper gradient boosting was used to determine the decision boundary between a Strike and a Ball using 2015 & 2016 pitch data. The figure on the left compares empirical <u>rectangular</u> strike zones to the strike zone as defined by the Major-League Baseball rule book.<br><br><u>KEY DISCOVERY #1</u><br>The right hand-batter strike zone is in general larger than the left hand-batter strike zone.<br><br><u>KEY DISCOVERY #2</u><br>The strikezone expands with each ball and contracts with each strike.  Strike contraction is generally larger than ball expansion. View 0-2, 0-0 and 3-0 to see progression.")
SZDescript <- HTML("<br><u>PLOT EXPLANATION</u><br>Closeups of the rectangle and superellipse strike zones are shown for selectable stances and strike zones.  Random samples of 10,000 pitches for each stance/count are included with the empirical strike zones. Called balls shown in red.  Called strikes shown in blue")
heatmapDescript <- HTML("<br><u>PLOT EXPLANATION</u><br>Heatmap showing total pitch count for different stances/counts. Grid is divided into equally spaced 6x6 inch square regions. Interesting counts to view are 0-2 and 3-0. Note the extreme differences in the pitch locations. ")


######################################### copy this code too
squircles <- read.csv("Squircles_all.csv")
squircles['Count'] <- paste(squircles$Balls, "-", squircles$Strikes, sep="")

rectangles <- read.csv("Rect_all.csv")
rectangles['Count'] <- paste(rectangles$Balls, "-", rectangles$Strikes, sep="")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme('darkly'), headerPanel("Major League Baseball Strikezone Visualization Tool"),
                tabsetPanel(
                  tabPanel('Rectangle SZ',
                           ####################################### Beginning of TabPanel
                           fluidRow(column(2, offset=0,
                                           radioButtons(inputId = "stanceSelection2",
                                                        label = "Select Batter Stance", 
                                                        choiceNames = c("Right Hand", "Left Hand", "Both"),
                                                        choiceValues = c("R", "L", "B")
                                           ),
                                           checkboxGroupInput(inputId="countSelect2", 
                                                              label="Select the Count", 
                                                              choices  = c("0-0",
                                                                           "0-1",
                                                                           "0-2",
                                                                           "1-0",
                                                                           "1-1",
                                                                           "1-2",
                                                                           "2-0",
                                                                           "2-1",
                                                                           "2-2",
                                                                           "3-0",
                                                                           "3-1",
                                                                           "3-2"),
                                                              inline   = T,
                                                              selected= c("0-0")),
                                           plotOutput("legend2", height = 300, inline = T)
                           ),
                           column(7, offset=0, plotOutput('image3')),
                           column(3, offset=0, rectDescript)
                           )
                           
                  ),
                  tabPanel('Superellipse SZ',
                           ####################################### Beginning of TabPanel
                           fluidRow(column(2, offset=0,
                                           radioButtons(inputId = "stanceSelection",
                                                        label = "Select Batter Stance", 
                                                        choiceNames = c("Right Hand", "Left Hand", "Both"),
                                                        choiceValues = c("R", "L", "B")
                                           ),
                                           checkboxGroupInput(inputId="countSelect", 
                                                              label="Select the Count", 
                                                              choices  = c("0-0",
                                                                           "0-1",
                                                                           "0-2",
                                                                           "1-0",
                                                                           "1-1",
                                                                           "1-2",
                                                                           "2-0",
                                                                           "2-1",
                                                                           "2-2",
                                                                           "3-0",
                                                                           "3-1",
                                                                           "3-2"),
                                                              inline   = T,
                                                              selected= c("0-0")),
                                           plotOutput("legend", height = 300, inline = T)
                           ),
                           column(7, offset=0, plotOutput('image2')),
                           column(3, offset=0, ellipseDescript)
                           )
                           
                  ), # tab panel 
                 
                  tabPanel('Strike Zone Close-Up', 
                           fluidRow(column(2, offset=0,  radioButtons(inputId = "stanceSelection3",
                                                                      label = "Select Batter Stance", 
                                                                      choiceNames = c("Right Hand", "Left Hand"),
                                                                      choiceValues = c("R", "L"),
                                                                      selected = "R"
                           ),
                           selectInput(inputId="countSelect3", 
                                       label="Select the Count", 
                                       choices  = c("0-0",
                                                    "0-1",
                                                    "0-2",
                                                    "1-0",
                                                    "1-1",
                                                    "1-2",
                                                    "2-0",
                                                    "2-1",
                                                    "2-2",
                                                    "3-0",
                                                    "3-1",
                                                    "3-2"),
                                       selected= "3-0")
                           ),
                           column(7, offset=0, plotlyOutput("pitchplot", width = 500, height = 550)),
                           column(3, offset = 0, SZDescript)
                           ))
                
                  ######################################## end of TabPanel
                )
) # end fluid page

##################################### We need this theme two
theme1 <- theme(panel.background = element_rect(fill = "transparent", colour = NA),
                plot.background = element_rect(fill = "transparent", colour = NA),
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),
                panel.border = element_rect(fill = 'transparent', colour = NA),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                legend.background = element_rect(fill='grey96'),
                legend.key = element_blank(),
                legend.text = element_text(size = 14),
                legend.key.height = unit(5, "mm"),
                legend.key.width = unit(20, "mm"),
                legend.title = element_text(size = 15, face="bold" )
)



############################ Read In Images #########################
imgR <- readPNG('images/right_snip.png')
imgL <- readPNG('images/left_snip.png')
imgB <- readPNG('images/both_sides.png')

shiftR = 0.95
shiftL = -1
shiftB = 0.1



# Define server logic required to draw a histogram
server <- function(input, output) {
  
      #################################################################
      #                       Stance Selector                         #
      #################################################################
      Stance_select <- reactive({
        switch(input$batside,
               "Right" = pitch_melt_r2,
               "Left" = pitch_melt_l2
               )
      })
      
      #################################################################
      #               Main Plotter Function (Rectangles)              #
      #################################################################
      plotter2 <- reactive({
        
        
        if (input$stanceSelection2=="B"){
          g <- rasterGrob(imgB, interpolate=TRUE)
          
          p <- ggplot(data=rectangles[rectangles$Count %in% input$countSelect2,])  
          p <- p + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
          p <- p + geom_path(aes(x=x+shiftB,y=y-3, color=Count, linetype=Side), size=1, show.legend = FALSE)
          p <- p + geom_rect(xmin=(-1.678/2)+shiftB,xmax = (1.678/2)+shiftB, ymax = 3.5-3, ymin = 1.5-3, fill='transparent', color='white')
          p <- p + annotate("text", x = mean(squircles$x)+shiftB, y = 1.4-3, 
                            label = paste('Rulebook SZ in White'),
                            colour = "white", size=4, fontface=2)
        }else {
          p <- ggplot(data=rectangles[rectangles$Count %in% input$countSelect2 & rectangles$Side == input$stanceSelection2,])
          
          if(input$stanceSelection2=='L'){
            g <- rasterGrob(imgL, interpolate=TRUE)
            p <- p + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
            p <- p + geom_path(aes(x=x+shiftL,y=y-3, color=Count), size=1, show.legend = FALSE)
            p <- p + geom_rect(xmin=(-1.678/2)+shiftL,xmax = (1.678/2)+shiftL, ymax = 3.5-3, ymin = 1.5-3, fill='transparent', color='white')
            p <- p + annotate("text", x = mean(squircles$x)+shiftL, y = 1.4-3, 
                              label = paste('Rulebook SZ in White'),
                              colour = "white", size=4, fontface=2)
          }else{
            g <- rasterGrob(imgR, interpolate=TRUE)
            p <- p + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
            p <- p + geom_path(aes(x=x+.95,y=y-3, color=Count), size=1, show.legend = FALSE)
            p <- p + geom_rect(xmin=(-1.678/2)+shiftR,xmax = (1.678/2)+shiftR, ymax = 3.5-3, ymin = 1.5-3, fill='transparent', color='white')
            p <- p + annotate("text", x = mean(squircles$x)+shiftR, y = 1.4-3, 
                              label = paste('Rulebook SZ in White'),
                              colour = "white", size=4, fontface=2)
          }
        }
        
        p <- p + theme1
        p <- p + coord_cartesian(xlim=c(-4,4), ylim=c(-4,4))
        p
        
      })
      
      
     
     #################################################################
     #                  Main Plotter Function (Ellipse)              #
     #################################################################
     plotter <- reactive({

       
       if (input$stanceSelection=="B"){
         g <- rasterGrob(imgB, interpolate=TRUE)
         
         p <- ggplot(data=squircles[squircles$Count %in% input$countSelect,])  
         p <- p + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
         p <- p + geom_path(aes(x=x+shiftB,y=y-3, color=Count, linetype=Side), size=1, show.legend = FALSE)
         p <- p + geom_rect(xmin=(-1.678/2)+shiftB,xmax = (1.678/2)+shiftB, ymax = 3.5-3, ymin = 1.5-3, fill='transparent', color='white')
         p <- p + annotate("text", x = mean(squircles$x)+shiftB, y = 1.4-3, 
                           label = paste('Rulebook SZ in White'),
                           colour = "white", size=4, fontface=2)
       }else {
         p <- ggplot(data=squircles[squircles$Count %in% input$countSelect & squircles$Side == input$stanceSelection,])
         
         if(input$stanceSelection=='L'){
           g <- rasterGrob(imgL, interpolate=TRUE)
           p <- p + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
           p <- p + geom_path(aes(x=x+shiftL,y=y-3, color=Count), size=1, show.legend = FALSE)
           p <- p + geom_rect(xmin=(-1.678/2)+shiftL,xmax = (1.678/2)+shiftL, ymax = 3.5-3, ymin = 1.5-3, fill='transparent', color='white')
           p <- p + annotate("text", x = mean(squircles$x)+shiftL, y = 1.4-3, 
                             label = paste('Rulebook SZ in White'),
                             colour = "white", size=4, fontface=2)
         }else{
           g <- rasterGrob(imgR, interpolate=TRUE)
           p <- p + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
           p <- p + geom_path(aes(x=x+.95,y=y-3, color=Count), size=1, show.legend = FALSE)
           p <- p + geom_rect(xmin=(-1.678/2)+shiftR,xmax = (1.678/2)+shiftR, ymax = 3.5-3, ymin = 1.5-3, fill='transparent', color='white')
           p <- p + annotate("text", x = mean(squircles$x)+shiftR, y = 1.4-3, 
                             label = paste('Rulebook SZ in White'),
                             colour = "white", size=4, fontface=2)
         }
       }
       
       p <- p + theme1
       p <- p + coord_cartesian(xlim=c(-4,4), ylim=c(-4,4))
       p
       
     })
     
     #################################################################
     #                       legend maker function 2                 #
     #################################################################
     
     make_legend2 <- reactive({
       if(length(input$countSelect2) > 0){
         if (input$stanceSelection2=="B"){
           p <- ggplot(data=rectangles[rectangles$Count %in% input$countSelect2,])  
           p <- p + geom_path(aes(x=x,y=y, color=Count, linetype=Side), size=1, show.legend = T)
         }else{
           p <- ggplot(data=rectangles[rectangles$Count %in% input$countSelect2 & rectangles$Side == input$stanceSelection2,])
           p <- p + geom_path(aes(x=x,y=y, color=Count), size=1, show.legend = T)
         }
         
         p <- p + theme1
         grid.arrange(g_legend(p), widths=c(12)) 
       }
       
     })
     
     #################################################################
     #                       legend maker function 1                 #
     #################################################################
     
     make_legend <- reactive({
       if(length(input$countSelect) > 0){
         if (input$stanceSelection=="B"){
           p <- ggplot(data=squircles[squircles$Count %in% input$countSelect,])  
           p <- p + geom_path(aes(x=x,y=y, color=Count, linetype=Side), size=1, show.legend = T)
         }else{
           p <- ggplot(data=squircles[squircles$Count %in% input$countSelect & squircles$Side == input$stanceSelection,])
           p <- p + geom_path(aes(x=x,y=y, color=Count), size=1, show.legend = T)
         }
         
         p <- p + theme1
         grid.arrange(g_legend(p), widths=c(12)) 
       }
       
     })
     #################################################################
     #                       Summary Plot                            #
     #################################################################
     output$summary <- renderPlotly(
       
       { data_used <- Stance_select()
       
       p = ggplot(data=data_used, aes(x = Called,
                                      y = Proportion, fill = Classified)) +
         geom_bar(colour = 'black', stat = 'identity') +
         
         facet_wrap(~Count) +
         ggtitle("Likelihood of Taken Pitch Being Called Strike or Ball") +
         theme(axis.text.x = element_text(color='white'),strip.text = element_text(face = 'bold', size = 12))   +
         theme(axis.title = element_blank()) + theme(axis.text.y = element_text(size=12, color='white'), axis.text.x = element_text(size=12, color='white')) +
         scale_fill_manual(values=c("#999999", "firebrick3")) + 
         theme(plot.title = element_text(hjust = 0.0, colour = 'white')) + 
         theme(panel.background = element_rect(fill='grey13'), plot.background = element_rect(fill='grey13')) +
         theme(legend.key = element_blank(), legend.background = element_blank(), legend.title = element_text(color='white'),
               legend.text = element_text(color='white'))
       
       ggplotly(p, tooltip = c('Proportion')) 
       })
    
     #################################################################
     #                    plot pitches                               #
     #################################################################
     plot_pitches <- reactive({
       theme2 <- theme(
         plot.background = element_rect(fill='grey13'),
         panel.background = element_rect(fill='grey13'),
         panel.grid.major = element_line(colour = 'grey50'),
         axis.title = element_blank(),
         axis.text = element_text(color='white'),
         panel.border = element_rect(color='grey50', fill='transparent')
       )
       
       
       p <- ggplot() +  geom_point(data=pitch_data[pitch_data$Count==input$countSelect3 & pitch_data$bat_side==input$stanceSelection3,], aes(x=plate_x, y=plate_z, fill=as.factor(called) ), alpha=0.5,pch=21, stroke=.2, show.legend = FALSE)
       p <- p + geom_path(data=squircles[squircles$Count==input$countSelect3 & squircles$Side==input$stanceSelection3,], aes(x=x, y=y,color=Count), size=1,color='white', show.legend = FALSE)
       p <- p + geom_path(data=rectangles[rectangles$Count==input$countSelect3 & rectangles$Side==input$stanceSelection3,], aes(x=x,y=y, color=Count), size=1, color='yellow', linetype=2, show.legend = FALSE)
       p <- p + scale_fill_manual(values=c('indianred','dodgerblue'))
       p <- p + theme2
       p <- p + coord_cartesian(xlim=c(-2,2))
       g <- ggplotly(p, tooltip = c('plate_z', 'plate_x', 'call'))
       g <- hide_legend(g)
     })
     

     #################################################################
     #                       Render Plots                            #
     #################################################################
     output$image2 <- renderPlot({
       plotter()
     }, height = 600, width = 550, bg = 'transparent')
    
     ########################################################################
     output$legend <- renderPlot({
       make_legend()
     }, height = 350, width = 100, bg = "grey13")
     
     #######################################################################
     output$image3 <- renderPlot({
       plotter2()
     }, height = 600, width = 550, bg = 'transparent')
     
     ########################################################################
     output$legend2 <- renderPlot({
       make_legend2()
     }, height = 350, width = 100, bg = "grey13")
     ########################################################################
     output$pitchplot <- renderPlotly({
       plot_pitches()
     })
     ########################################################################

     
}

# Run the application 
shinyApp(ui = ui, server = server)


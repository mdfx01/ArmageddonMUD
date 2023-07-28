library(ggpubr)
library(shiny)
options(warn=-1)

class <- read.csv('class_data.csv', colClasses=c(rep('character',6), 'numeric'))
subclass <- read.csv('subclass_data.csv', colClasses=c(rep('character',4), 'numeric', 'character'))

classlist <- c("", unique(class$class))

subclasslist <- c("", unique(subclass$subclass))


# input <- data.frame(selected.class1 ='Fighter',
#                     selected.subclass1 = '',
#                     selected.class2 ='Soldier',
#                     selected.subclass2 = '',
#                     selected.class3 = '',
#                     selected.subclass3 = '',
#                     selected.class4 ='Craftsperson',
#                     selected.subclass4 = '',
#                     selected.class5 = 'Artisan',
#                     selected.subclass5 = ''
#                     )
# 


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MarshallDFX's ArmageddonMud Skill Comparison Graph"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          actionButton("crimbutton", "Criminal"),
          actionButton("wildbutton", "Wilderness"),
          actionButton("citybutton", "City"),
          actionButton("heavybutton", "Heavy Combat"),
          actionButton("lightbutton", "Light Combat"),
          actionButton("mixedbutton", "Mixed"),
          actionButton("lmbutton", "Light Mercantile"),
          actionButton("hmbutton", "Heavy Mercantile"),
          actionButton("clearbutton", "Clear All"),
            selectInput(
                "selected.class1",
                "Class1",
                classlist,
                selected = "Fighter",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            ),
            selectInput(
                "selected.subclass1",
                "Subclass1",
                subclasslist,
                selected = "",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            ),
            selectInput(
                "selected.class2",
                "Class2",
                classlist,
                selected = "Soldier",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            ),
            selectInput(
                "selected.subclass2",
                "Subclass2",
                subclasslist,
                selected = "",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            ),
            selectInput(
                "selected.class3",
                "Class3",
                classlist,
                selected = "Labourer",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            ),
            selectInput(
                "selected.subclass3",
                "Subclass3",
                subclasslist,
                selected = "",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            ),
            selectInput(
                "selected.class4",
                "Class4",
                classlist,
                selected = "Craftsperson",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            ),
            selectInput(
                "selected.subclass4",
                "Subclass4",
                subclasslist,
                selected = "",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            ),
            selectInput(
                "selected.class5",
                "Class5",
                classlist,
                selected = "Artisan",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            ),
            selectInput(
                "selected.subclass5",
                "Subclass5",
                subclasslist,
                selected = "",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            ),
            
            width=2),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("plotCombat", height="750px"),
          plotOutput("plotWeapon", height="300px"),
          plotOutput("plotManipulation", height="400px"),
          plotOutput("plotPerception", height="500px"),
          plotOutput("plotStealth", height="400px"),
          plotOutput("plotBarter", height="200px"),
          plotOutput("plotCraft", height="1100px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # testdat <- reactive({
    #     alldat()
    # })
    
    dat1 <- reactive({
        if(input$selected.class1 == "" & input$selected.subclass1 == ""){
            dat1 <- data.frame(skill_group=character(),
                               skill=character(),
                               skill_number=double())
            dat1
        }else{
            class.filter1 <- class$class == input$selected.class1
            subclass.filter1 <- subclass$subclass == input$selected.subclass1
            dat1 <- rbind(class[class.filter1, c('skill_group', 'skill', 'skill_number')],
                          subclass[subclass.filter1,c('skill_group', 'skill', 'skill_number') ])
            dat1 <- aggregate(skill_number~skill_group+skill, data=dat1, FUN=max)
            dat1$classcombo <- paste(input$selected.class1, input$selected.subclass1)
            dat1$skill_group <- as.factor(dat1$skill_group)
            dat1 
        }     
    })
    
       dat2 <- reactive({
        if(input$selected.class2 == "" & input$selected.subclass2 == ""){
            dat2 <- data.frame(skill_group=character(),
                               skill=character(),
                               skill_number=double())
            dat2
        }else{
            class.filter2 <- class$class == input$selected.class2
            subclass.filter2 <- subclass$subclass == input$selected.subclass2
            dat2 <- rbind(class[class.filter2, c('skill_group', 'skill', 'skill_number')],
                          subclass[subclass.filter2,c('skill_group', 'skill', 'skill_number') ])
            dat2 <- aggregate(skill_number~skill_group+skill, data=dat2, FUN=max)
            dat2$classcombo <- paste(input$selected.class2, input$selected.subclass2)
            dat2$skill_group <- as.factor(dat2$skill_group)
            dat2 
        }     
    })
    
       dat3 <- reactive({
           if(input$selected.class3 == "" & input$selected.subclass3 == ""){
               dat3 <- data.frame(skill_group=character(),
                                  skill=character(),
                                  skill_number=double())
               dat3
           }else{
               class.filter3 <- class$class == input$selected.class3
               subclass.filter3 <- subclass$subclass == input$selected.subclass3
               dat3 <- rbind(class[class.filter3, c('skill_group', 'skill', 'skill_number')],
                             subclass[subclass.filter3,c('skill_group', 'skill', 'skill_number') ])
               dat3 <- aggregate(skill_number~skill_group+skill, data=dat3, FUN=max)
               dat3$classcombo <- paste(input$selected.class3, input$selected.subclass3)
               dat3$skill_group <- as.factor(dat3$skill_group)
               dat3 
           }     
       })
    
       dat4 <- reactive({
           if(input$selected.class4 == "" & input$selected.subclass4 == ""){
               dat4 <- data.frame(skill_group=character(),
                                  skill=character(),
                                  skill_number=double())
               dat4
           }else{
               class.filter4 <- class$class == input$selected.class4
               subclass.filter4 <- subclass$subclass == input$selected.subclass4
               dat4 <- rbind(class[class.filter4, c('skill_group', 'skill', 'skill_number')],
                             subclass[subclass.filter4,c('skill_group', 'skill', 'skill_number') ])
               dat4 <- aggregate(skill_number~skill_group+skill, data=dat4, FUN=max)
               dat4$classcombo <- paste(input$selected.class4, input$selected.subclass4)
               dat4$skill_group <- as.factor(dat4$skill_group)
               dat4 
           }     
       })
    
       dat5 <- reactive({
           if(input$selected.class5 == "" & input$selected.subclass5 == ""){
               dat5 <- data.frame(skill_group=character(),
                                  skill=character(),
                                  skill_number=double())
               dat5
           }else{
               class.filter5 <- class$class == input$selected.class5
               subclass.filter5 <- subclass$subclass == input$selected.subclass5
               dat5 <- rbind(class[class.filter5, c('skill_group', 'skill', 'skill_number')],
                             subclass[subclass.filter5,c('skill_group', 'skill', 'skill_number') ])
               dat5 <- aggregate(skill_number~skill_group+skill, data=dat5, FUN=max)
               dat5$classcombo <- paste(input$selected.class5, input$selected.subclass5)
               dat5$skill_group <- as.factor(dat5$skill_group)
               dat5 
           }     
       })
    
    alldat <- reactive({
        alldat <- rbind(dat1(), dat2(), dat3(), dat4(), dat5())
  
        
        selected.list <- c(paste(input$selected.class1, input$selected.subclass1),
                           paste(input$selected.class2, input$selected.subclass2),
                           paste(input$selected.class3, input$selected.subclass3),
                           paste(input$selected.class4, input$selected.subclass4),
                           paste(input$selected.class5, input$selected.subclass5))
       selected.list <- selected.list[!(selected.list %in% " ")]
       selected.list <- selected.list[!duplicated(selected.list)]
     
        
        alldat$classcombo <- factor(alldat$classcombo, levels=selected.list)
        alldat
    })

    # output$testPrint <- renderPrint({
    #     print(testdat())
    # })
    
    output$plotCombat <- renderPlot({
      plotdat <- alldat()

       skillgroup <- 'Combat'
      ggdotchart(plotdat[plotdat$skill_group == skillgroup, ], x="skill", y="skill_number", group="classcombo", rotate=TRUE,
                 color = "classcombo", main=skillgroup, legend.title = "Class/Subclass", legend='right',
                 add = "segments",  dot.size=3,  position = position_dodge(-0.6),
                 xlab='',  ggtheme = theme_classic(), sorting='none') + 
        coord_flip(ylim = c(0, 4)) + 
          scale_y_continuous(name ="",  labels=c("0"="Novice", "1"="Apprentice", "2"="Journeyman", "3"="Advanced", "4"="Master"))  +theme(text=element_text(size=20))
    })
    
    output$plotCraft <- renderPlot({
        plotdat <- alldat()
        
        skillgroup <- 'Craft'
        ggdotchart(plotdat[plotdat$skill_group == skillgroup, ], x="skill", y="skill_number", group="classcombo", rotate=TRUE,
                   color = "classcombo", main=skillgroup, legend.title = "Class/Subclass", legend='right',
                   add = "segments", position = position_dodge(-0.6), dot.size=3,
                   xlab='',  ggtheme = theme_classic(), sorting='none') + 
          coord_flip(ylim = c(0, 4)) + 
            scale_y_continuous(name ="",  labels=c("0"="Novice", "1"="Apprentice", "2"="Journeyman", "3"="Advanced", "4"="Master"))  +theme(text=element_text(size=20))
    })
    
    output$plotManipulation <- renderPlot({
        plotdat <- alldat()
        
        skillgroup <- 'Manipulation'
        ggdotchart(plotdat[plotdat$skill_group == skillgroup, ], x="skill", y="skill_number", group="classcombo", rotate=TRUE,
                   color = "classcombo", main=skillgroup, legend.title = "Class/Subclass", legend='right',
                   add = "segments", position = position_dodge(-0.6), dot.size=3,
                   xlab='',  ggtheme = theme_classic(), sorting='none') + 
          coord_flip(ylim = c(0, 4)) + 
            scale_y_continuous(name ="",  labels=c("0"="Novice", "1"="Apprentice", "2"="Journeyman", "3"="Advanced", "4"="Master"))  +theme(text=element_text(size=20))
    })
    
    output$plotPerception <- renderPlot({
        plotdat <- alldat()
        
        skillgroup <- 'Perception'
        ggdotchart(plotdat[plotdat$skill_group == skillgroup, ], x="skill", y="skill_number", group="classcombo",
                     rotate=TRUE,
                   color = "classcombo", main=skillgroup, legend.title = "Class/Subclass", legend='right',
                   add = "segments", position = position_dodge(-0.6), dot.size=3,
                   xlab='',  ggtheme = theme_classic(), sorting='none') + 
          coord_flip(ylim = c(0, 4)) + 
          scale_y_continuous(name ="",  labels=c("0"="Novice", "1"="Apprentice", "2"="Journeyman", "3"="Advanced", "4"="Master"))  +theme(text=element_text(size=20))
    })
    
    output$plotStealth <- renderPlot({
        plotdat <- alldat()
        
        skillgroup <- 'Stealth'
        ggdotchart(plotdat[plotdat$skill_group == skillgroup, ], x="skill", y="skill_number", group="classcombo", rotate=TRUE,
                   color = "classcombo", main=skillgroup, legend.title = "Class/Subclass", legend='right',
                   add = "segments", position = position_dodge(-0.6), dot.size=3,
                   xlab='',  ggtheme = theme_classic(), sorting='none') + 
          coord_flip(ylim = c(0, 4)) + 
            scale_y_continuous(name ="",  labels=c("0"="Novice", "1"="Apprentice", "2"="Journeyman", "3"="Advanced", "4"="Master"))  +theme(text=element_text(size=20))
    })
    
    output$plotWeapon <- renderPlot({
        plotdat <- alldat()
        
        skillgroup <- 'Weapon'
        ggdotchart(plotdat[plotdat$skill_group == skillgroup, ], x="skill", y="skill_number", group="classcombo", rotate=TRUE,
                   color = "classcombo", main=skillgroup, legend.title = "Class/Subclass", legend='right',
                   add = "segments", position = position_dodge(-0.6), dot.size=3,
                   xlab='',  ggtheme = theme_classic(), sorting='none') + 
          coord_flip(ylim = c(0, 4)) + 
            scale_y_continuous(name ="",  labels=c("0"="Novice", "1"="Apprentice", "2"="Journeyman", "3"="Advanced", "4"="Master"))  +theme(text=element_text(size=20))
    })
    
    output$plotBarter <- renderPlot({
        plotdat <- alldat()
        
        skillgroup <- 'Barter'
        ggdotchart(plotdat[plotdat$skill_group == skillgroup, ], x="skill", y="skill_number", group="classcombo", rotate=TRUE,
                   color = "classcombo", main=skillgroup, legend.title = "Class/Subclass", legend='right',
                   add = "segments", position = position_dodge(-0.6), dot.size=3,
                   xlab='',  ggtheme = theme_classic(), sorting='none') + 
          coord_flip(ylim = c(0, 4)) + 
            scale_y_continuous(name ="",  labels=c("0"="Novice", "1"="Apprentice", "2"="Journeyman", "3"="Advanced", "4"="Master"))  +theme(text=element_text(size=20))
    })
    

    observeEvent(input$crimbutton, {
      updateTextInput(session, "selected.class1", value = "Enforcer")
      updateTextInput(session, "selected.class2", value = "Infiltrator")
      updateTextInput(session, "selected.class3", value = "Miscreant")
      updateTextInput(session, "selected.class4", value = "Pilferer")
      updateTextInput(session, "selected.class5", value = "Fence")
      updateTextInput(session, "selected.subclass1", value = "")
      updateTextInput(session, "selected.subclass2", value = "")
      updateTextInput(session, "selected.subclass3", value = "")
      updateTextInput(session, "selected.subclass4", value = "")
      updateTextInput(session, "selected.subclass5", value = "")
       })
    observeEvent(input$wildbutton, {
      updateTextInput(session, "selected.class1", value = "Raider")
      updateTextInput(session, "selected.class2", value = "Scout")
      updateTextInput(session, "selected.class3", value = "Stalker")
      updateTextInput(session, "selected.class4", value = "Adventurer")
      updateTextInput(session, "selected.class5", value = "Dune.Trader")
      updateTextInput(session, "selected.subclass1", value = "")
      updateTextInput(session, "selected.subclass2", value = "")
      updateTextInput(session, "selected.subclass3", value = "")
      updateTextInput(session, "selected.subclass4", value = "")
      updateTextInput(session, "selected.subclass5", value = "")
    })  
    observeEvent(input$citybutton, {
      updateTextInput(session, "selected.class1", value = "Fighter")
      updateTextInput(session, "selected.class2", value = "Soldier")
      updateTextInput(session, "selected.class3", value = "Labourer")
      updateTextInput(session, "selected.class4", value = "Craftsperson")
      updateTextInput(session, "selected.class5", value = "Artisan")
      updateTextInput(session, "selected.subclass1", value = "")
      updateTextInput(session, "selected.subclass2", value = "")
      updateTextInput(session, "selected.subclass3", value = "")
      updateTextInput(session, "selected.subclass4", value = "")
      updateTextInput(session, "selected.subclass5", value = "")
    })    
    
    observeEvent(input$heavybutton, {
      updateTextInput(session, "selected.class1", value = "Enforcer")
      updateTextInput(session, "selected.class2", value = "Raider")
      updateTextInput(session, "selected.class3", value = "Fighter")
      updateTextInput(session, "selected.class4", value = "")
      updateTextInput(session, "selected.class5", value = "")
      updateTextInput(session, "selected.subclass1", value = "")
      updateTextInput(session, "selected.subclass2", value = "")
      updateTextInput(session, "selected.subclass3", value = "")
      updateTextInput(session, "selected.subclass4", value = "")
      updateTextInput(session, "selected.subclass5", value = "")
    })
    
    observeEvent(input$lightbutton, {
      updateTextInput(session, "selected.class1", value = "Infiltrator")
      updateTextInput(session, "selected.class2", value = "Scout")
      updateTextInput(session, "selected.class3", value = "Soldier")
      updateTextInput(session, "selected.class4", value = "")
      updateTextInput(session, "selected.class5", value = "")
      updateTextInput(session, "selected.subclass1", value = "")
      updateTextInput(session, "selected.subclass2", value = "")
      updateTextInput(session, "selected.subclass3", value = "")
      updateTextInput(session, "selected.subclass4", value = "")
      updateTextInput(session, "selected.subclass5", value = "")
    })
    
    observeEvent(input$mixedbutton, {
      updateTextInput(session, "selected.class1", value = "Miscreant")
      updateTextInput(session, "selected.class2", value = "Stalker")
      updateTextInput(session, "selected.class3", value = "Labourer")
      updateTextInput(session, "selected.class4", value = "")
      updateTextInput(session, "selected.class5", value = "")
      updateTextInput(session, "selected.subclass1", value = "")
      updateTextInput(session, "selected.subclass2", value = "")
      updateTextInput(session, "selected.subclass3", value = "")
      updateTextInput(session, "selected.subclass4", value = "")
      updateTextInput(session, "selected.subclass5", value = "")
    })
    
    observeEvent(input$lmbutton, {
      updateTextInput(session, "selected.class1", value = "Pilferer")
      updateTextInput(session, "selected.class2", value = "Adventurer")
      updateTextInput(session, "selected.class3", value = "Craftsperson")
      updateTextInput(session, "selected.class4", value = "")
      updateTextInput(session, "selected.class5", value = "")
      updateTextInput(session, "selected.subclass1", value = "")
      updateTextInput(session, "selected.subclass2", value = "")
      updateTextInput(session, "selected.subclass3", value = "")
      updateTextInput(session, "selected.subclass4", value = "")
      updateTextInput(session, "selected.subclass5", value = "")
    })
    
    observeEvent(input$hmbutton, {
      updateTextInput(session, "selected.class1", value = "Fence")
      updateTextInput(session, "selected.class2", value = "Dune.Trader")
      updateTextInput(session, "selected.class3", value = "Artisan")
      updateTextInput(session, "selected.class4", value = "")
      updateTextInput(session, "selected.class5", value = "")
      updateTextInput(session, "selected.subclass1", value = "")
      updateTextInput(session, "selected.subclass2", value = "")
      updateTextInput(session, "selected.subclass3", value = "")
      updateTextInput(session, "selected.subclass4", value = "")
      updateTextInput(session, "selected.subclass5", value = "")
    })
    
    observeEvent(input$clearbutton, {
      updateTextInput(session, "selected.class1", value = "")
      updateTextInput(session, "selected.class2", value = "")
      updateTextInput(session, "selected.class3", value = "")
      updateTextInput(session, "selected.class4", value = "")
      updateTextInput(session, "selected.class5", value = "")
      updateTextInput(session, "selected.subclass1", value = "")
      updateTextInput(session, "selected.subclass2", value = "")
      updateTextInput(session, "selected.subclass3", value = "")
      updateTextInput(session, "selected.subclass4", value = "")
      updateTextInput(session, "selected.subclass5", value = "")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

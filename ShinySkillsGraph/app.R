library(ggpubr)
library(shiny)
library(shinythemes)
library(ggdark)
options(warn=-1)

class <- read.csv('class_data.csv', colClasses=c(rep('character',6), 'numeric'))
subclass <- read.csv('subclass_data.csv', colClasses=c(rep('character',4), 'numeric', 'character'))

classlist <- c("", unique(class$class))
subclasslist <- c("", unique(subclass$subclass))

classcombos <-  read.csv('widecombos.csv', colClasses=c(rep('character',3), rep('numeric',101)))

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
ui <- navbarPage("MarshallDFX's AmrageddonMUD Skill Tools", theme = shinytheme("cyborg"),
              

    # Application title
    tabPanel("Graphical Skill Comparison",
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
          plotOutput("plotCraft", height="1100px"),
          plotOutput("plotPerk", height="750px")
        )
    )

    ),
    tabPanel("Skill Picker", 
             sidebarLayout(
               sidebarPanel(
                 h5('1=apprentice, 2=journeyman, 3=advanced, 4=master'),
             h4("Combat"),
             sliderInput("skill.Archery", "Archery", 0, 4, 0, step = 1),
             sliderInput("skill.Backstab", "Backstab", 0, 4, 0, step = 1),
             sliderInput("skill.Bash", "Bash", 0, 4, 0, step = 1),
             sliderInput("skill.Blind.Fighting", "Blind.Fighting", 0, 4, 0, step = 1),
             sliderInput("skill.Blowgun.Use", "Blowgun.Use", 0, 4, 0, step = 1),
             sliderInput("skill.Charge", "Charge", 0, 4, 0, step = 1),
             sliderInput("skill.Crossbow.Use", "Crossbow.Use", 0, 4, 0, step = 1),
             sliderInput("skill.Disarm", "Disarm", 0, 4, 0, step = 1),
             sliderInput("skill.Dual.Wield", "Dual.Wield", 0, 4, 0, step = 1),
             sliderInput("skill.Flee", "Flee", 0, 4, 0, step = 1),
             sliderInput("skill.Guarding", "Guarding", 0, 4, 0, step = 1),
             sliderInput("skill.Hack", "Hack", 0, 4, 0, step = 1),
             sliderInput("skill.Kick", "Kick", 0, 4, 0, step = 1),
             sliderInput("skill.Parry", "Parry", 0, 4, 0, step = 1),
             sliderInput("skill.Rescue", "Rescue", 0, 4, 0, step = 1),
             sliderInput("skill.Riposte", "Riposte", 0, 4, 0, step = 1),
             sliderInput("skill.Sap", "Sap", 0, 4, 0, step = 1),
             sliderInput("skill.Shield.Use", "Shield.Use", 0, 4, 0, step = 1),
             sliderInput("skill.Sling.Use", "Sling.Use", 0, 4, 0, step = 1),
             sliderInput("skill.Subdue", "Subdue", 0, 4, 0, step = 1),
             sliderInput("skill.Threaten", "Threaten", 0, 4, 0, step = 1),
             sliderInput("skill.Throw", "Throw", 0, 4, 0, step = 1),
             sliderInput("skill.Two.Handed", "Two.Handed", 0, 4, 0, step = 1),
             
             h4("Weapon"),
             sliderInput("skill.Bludgeoning.Weapons", "Bludgeoning.Weapons", 0, 4, 0, step = 1),
             sliderInput("skill.Chopping.Weapons", "Chopping.Weapons", 0, 4, 0, step = 1),
             sliderInput("skill.Piercing.Weapons", "Piercing.Weapons", 0, 4, 0, step = 1),
             sliderInput("skill.Slashing.Weapons", "Slashing.Weapons", 0, 4, 0, step = 1),
             h4("Manipulation"),
             sliderInput("skill.Bandage", "Bandage", 0, 4, 0, step = 1),
             sliderInput("skill.Pick", "Pick", 0, 4, 0, step = 1),
             sliderInput("skill.Pilot", "Pilot", 0, 4, 0, step = 1),
             sliderInput("skill.Poisoning", "Poisoning", 0, 4, 0, step = 1),
             sliderInput("skill.Ride", "Ride", 0, 4, 0, step = 1),
             sliderInput("skill.Skinning", "Skinning", 0, 4, 0, step = 1),
             sliderInput("skill.Sleight.of.Hand", "Sleight.of.Hand", 0, 4, 0, step = 1),
             sliderInput("skill.Steal", "Steal", 0, 4, 0, step = 1),
             h4("Perception"),
             sliderInput("skill.Direction.Sense", "Direction.Sense", 0, 4, 0, step = 1),
             sliderInput("skill.Forage", "Forage", 0, 4, 0, step = 1),
             sliderInput("skill.Hunt.city", "Hunt.city", 0, 4, 0, step = 1),
             sliderInput("skill.Hunt.wilderness", "Hunt.wilderness", 0, 4, 0, step = 1),
             sliderInput("skill.Listen.city", "Listen.city", 0, 4, 0, step = 1),
             sliderInput("skill.Listen.wilderness", "Listen.wilderness", 0, 4, 0, step = 1),
             sliderInput("skill.Peek", "Peek", 0, 4, 0, step = 1),
             sliderInput("skill.Scan", "Scan", 0, 4, 0, step = 1),
             sliderInput("skill.Search", "Search", 0, 4, 0, step = 1),
             sliderInput("skill.Watch", "Watch", 0, 4, 0, step = 1),
             h4('Stealth'),
             sliderInput("skill.Climb", "Climb", 0, 4, 0, step = 1),
             sliderInput("skill.Hide.city", "Hide.city", 0, 4, 0, step = 1),
             sliderInput("skill.Hide.wilderness", "Hide.wilderness", 0, 4, 0, step = 1),
             sliderInput("skill.Sneak.city", "Sneak.city", 0, 4, 0, step = 1),
             sliderInput("skill.Sneak.wilderness", "Sneak.wilderness", 0, 4, 0, step = 1),
             h4('Barter'),
             sliderInput("skill.Haggle", "Haggle", 0, 4, 0, step = 1),
             sliderInput("skill.Value", "Value", 0, 4, 0, step = 1),
             h4('Crafting'),
             sliderInput("skill.Armor.Making", "Armor.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Armor.Repair", "Armor.Repair", 0, 4, 0, step = 1),
             sliderInput("skill.Axe.Making", "Axe.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Bandage.Making", "Bandage.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Basketweaving", "Basketweaving", 0, 4, 0, step = 1),
             sliderInput("skill.Bow.Making", "Bow.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Brew", "Brew", 0, 4, 0, step = 1),
             sliderInput("skill.Clayworking", "Clayworking", 0, 4, 0, step = 1),
             sliderInput("skill.Clothworking", "Clothworking", 0, 4, 0, step = 1),
             sliderInput("skill.Club.Making", "Club.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Cooking", "Cooking", 0, 4, 0, step = 1),
             sliderInput("skill.Dyeing", "Dyeing", 0, 4, 0, step = 1),
             sliderInput("skill.Feather.Working", "Feather.Working", 0, 4, 0, step = 1),
             sliderInput("skill.Fletchery", "Fletchery", 0, 4, 0, step = 1),
             sliderInput("skill.Floristry", "Floristry", 0, 4, 0, step = 1),
             sliderInput("skill.Instrument.Making", "Instrument.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Jewelry.Making", "Jewelry.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Knife.Making", "Knife.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Leatherworking", "Leatherworking", 0, 4, 0, step = 1),
             sliderInput("skill.Lumberjacking", "Lumberjacking", 0, 4, 0, step = 1),
             sliderInput("skill.Pick.Making", "Pick.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Spear.Making", "Spear.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Stonecrafting", "Stonecrafting", 0, 4, 0, step = 1),
             sliderInput("skill.Sword.Making", "Sword.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Tanning", "Tanning", 0, 4, 0, step = 1),
             sliderInput("skill.Tent.Making", "Tent.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Tool.Making", "Tool.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Wagon.Making", "Wagon.Making", 0, 4, 0, step = 1),
             sliderInput("skill.Woodworking", "Woodworking", 0, 4, 0, step = 1),
             h4('Perks'),
             sliderInput("skill.Alcohol.Tolerance.Perk", "Alcohol.Tolerance.Perk", 0, 1, 0, step = 1),
             sliderInput("skill.Better.Movement.Regeneration", "Better.Movement.Regeneration", 0, 1, 0, step = 1),
             sliderInput("skill.City.Forage", "City.Forage", 0, 1, 0, step = 1),
             sliderInput("skill.City.Hunt", "City.Hunt", 0, 1, 0, step = 1),
             sliderInput("skill.City.Stealth", "City.Stealth", 0, 1, 0, step = 1),
             sliderInput("skill.Customcraft", "Customcraft", 0, 1, 0, step = 1),
             sliderInput("skill.Hitch.2.Mounts", "Hitch.2.Mounts", 0, 1, 0, step = 1),
             sliderInput("skill.Learn.Languages.Perk", "Learn.Languages.Perk", 0, 1, 0, step = 1),
             sliderInput("skill.Learn.Ride.Faster", "Learn.Ride.Faster", 0, 1, 0, step = 1),
             sliderInput("skill.Pain.Tolerance.Perk", "Pain.Tolerance.Perk", 0, 1, 0, step = 1),
             sliderInput("skill.Tame.Mounts", "Tame.Mounts", 0, 1, 0, step = 1),
             sliderInput("skill.Wilderness.Forage", "Wilderness.Forage", 0, 1, 0, step = 1),
             sliderInput("skill.Wilderness.Hunt", "Wilderness.Hunt", 0, 1, 0, step = 1),
             sliderInput("skill.Wilderness.Quit", "Wilderness.Quit", 0, 1, 0, step = 1),
             sliderInput("skill.Wilderness.Stealth", "Wilderness.Stealth", 0, 1, 0, step = 1)
             
             
               ),
             mainPanel(
               tableOutput('tableSkillPicker')
             )
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
                 xlab='',   sorting='none') + 
        coord_flip(ylim = c(0, 4)) + 
         dark_theme_gray() +  scale_y_continuous(name ="",  labels=c("0"="Novice", "1"="Apprentice", "2"="Journeyman", "3"="Advanced", "4"="Master"))  +
        theme(text=element_text(size=20)  )  
 
    })
    
    output$plotCraft <- renderPlot({
        plotdat <- alldat()
        
        skillgroup <- 'Craft'
        ggdotchart(plotdat[plotdat$skill_group == skillgroup, ], x="skill", y="skill_number", group="classcombo", rotate=TRUE,
                   color = "classcombo", main=skillgroup, legend.title = "Class/Subclass", legend='right',
                   add = "segments", position = position_dodge(-0.6), dot.size=3,
                   xlab='',  sorting='none') +  dark_theme_gray() +
          coord_flip(ylim = c(0, 4)) + 
            scale_y_continuous(name ="",  labels=c("0"="Novice", "1"="Apprentice", "2"="Journeyman", "3"="Advanced", "4"="Master"))  +theme(text=element_text(size=20))
    })
    
    output$plotManipulation <- renderPlot({
        plotdat <- alldat()
        
        skillgroup <- 'Manipulation'
        ggdotchart(plotdat[plotdat$skill_group == skillgroup, ], x="skill", y="skill_number", group="classcombo", rotate=TRUE,
                   color = "classcombo", main=skillgroup, legend.title = "Class/Subclass", legend='right',
                   add = "segments", position = position_dodge(-0.6), dot.size=3,
                   xlab='', sorting='none') +  dark_theme_gray() +
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
                   xlab='',  sorting='none') +  dark_theme_gray() +
          coord_flip(ylim = c(0, 4)) + 
          scale_y_continuous(name ="",  labels=c("0"="Novice", "1"="Apprentice", "2"="Journeyman", "3"="Advanced", "4"="Master"))  +theme(text=element_text(size=20))
    })
    
    output$plotStealth <- renderPlot({
        plotdat <- alldat()
        
        skillgroup <- 'Stealth'
        ggdotchart(plotdat[plotdat$skill_group == skillgroup, ], x="skill", y="skill_number", group="classcombo", rotate=TRUE,
                   color = "classcombo", main=skillgroup, legend.title = "Class/Subclass", legend='right',
                   add = "segments", position = position_dodge(-0.6), dot.size=3,
                   xlab='', sorting='none') +  dark_theme_gray() +
          coord_flip(ylim = c(0, 4)) + 
            scale_y_continuous(name ="",  labels=c("0"="Novice", "1"="Apprentice", "2"="Journeyman", "3"="Advanced", "4"="Master"))  +theme(text=element_text(size=20))
    })
    
    output$plotWeapon <- renderPlot({
        plotdat <- alldat()
        
        skillgroup <- 'Weapon'
        ggdotchart(plotdat[plotdat$skill_group == skillgroup, ], x="skill", y="skill_number", group="classcombo", rotate=TRUE,
                   color = "classcombo", main=skillgroup, legend.title = "Class/Subclass", legend='right',
                   add = "segments", position = position_dodge(-0.6), dot.size=3,
                   xlab='', sorting='none') +  dark_theme_gray() +
          coord_flip(ylim = c(0, 4)) + 
            scale_y_continuous(name ="",  labels=c("0"="Novice", "1"="Apprentice", "2"="Journeyman", "3"="Advanced", "4"="Master"))  +theme(text=element_text(size=20))
    })
    
    output$plotBarter <- renderPlot({
        plotdat <- alldat()
        
        skillgroup <- 'Barter'
        ggdotchart(plotdat[plotdat$skill_group == skillgroup, ], x="skill", y="skill_number", group="classcombo", rotate=TRUE,
                   color = "classcombo", main=skillgroup, legend.title = "Class/Subclass", legend='right',
                   add = "segments", position = position_dodge(-0.6), dot.size=3,
                   xlab='',  sorting='none') +  dark_theme_gray() +
          coord_flip(ylim = c(0, 4)) + 
            scale_y_continuous(name ="",  labels=c("0"="Novice", "1"="Apprentice", "2"="Journeyman", "3"="Advanced", "4"="Master"))  +theme(text=element_text(size=20))
    })
    
    
    output$plotPerk <- renderPlot({
      plotdat <- alldat()
      skillgroup <- 'Perk'
      ggdotchart(plotdat[plotdat$skill_group == skillgroup, ], x="skill", y="skill_number", group="classcombo", rotate=TRUE,
                 color = "classcombo", main=skillgroup, legend.title = "Class/Subclass", legend='right',
                 add = "segments", position = position_dodge(-0.6), dot.size=3, 
                 xlab='',  sorting='none') +  dark_theme_gray() +
        coord_flip(ylim = c(0, 4)) + 
        scale_y_continuous(name ="",  labels=c("0"="No", "1"="Yes", "", "", ""))  +theme(text=element_text(size=20))
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
    
  skillpickerdat <- reactive({ 
    
    skillpickerdat <- classcombos[classcombos$Haggle >= input$skill.Haggle & 
                                    classcombos$Value >= input$skill.Value & 
                                    classcombos$Archery >= input$skill.Archery & 
                                    classcombos$Backstab >= input$skill.Backstab & 
                                    classcombos$Bash >= input$skill.Bash & 
                                    classcombos$Blind.Fighting >= input$skill.Blind.Fighting & 
                                    classcombos$Blowgun.Use >= input$skill.Blowgun.Use & 
                                    classcombos$Charge >= input$skill.Charge & 
                                    classcombos$Crossbow.Use >= input$skill.Crossbow.Use & 
                                    classcombos$Disarm >= input$skill.Disarm & 
                                    classcombos$Dual.Wield >= input$skill.Dual.Wield & 
                                    classcombos$Flee >= input$skill.Flee & 
                                    classcombos$Guarding >= input$skill.Guarding & 
                                    classcombos$Hack >= input$skill.Hack & 
                                    classcombos$Kick >= input$skill.Kick & 
                                    classcombos$Parry >= input$skill.Parry & 
                                    classcombos$Rescue >= input$skill.Rescue & 
                                    classcombos$Riposte >= input$skill.Riposte & 
                                    classcombos$Sap >= input$skill.Sap &
                                    
                                    classcombos$Shield.Use >= input$skill.Shield.Use & 
                                    classcombos$Sling.Use >= input$skill.Sling.Use & 
                                    classcombos$Subdue >= input$skill.Subdue & 
                                    classcombos$Threaten >= input$skill.Threaten & 
                                    classcombos$Throw >= input$skill.Throw & 
                                    classcombos$Two.Handed >= input$skill.Two.Handed & 
                                    classcombos$Armor.Making >= input$skill.Armor.Making & 
                                    classcombos$Armor.Repair >= input$skill.Armor.Repair & 
                                    classcombos$Axe.Making >= input$skill.Axe.Making & 
                                    classcombos$Bandage.Making >= input$skill.Bandage.Making & 
                                    classcombos$Basketweaving >= input$skill.Basketweaving & 
                                    classcombos$Bow.Making >= input$skill.Bow.Making & 
                                    classcombos$Brew >= input$skill.Brew & 
                                    classcombos$Clayworking >= input$skill.Clayworking & 
                                    classcombos$Clothworking >= input$skill.Clothworking & 
                                    classcombos$Club.Making >= input$skill.Club.Making & 
                                    classcombos$Cooking >= input$skill.Cooking & 
                                    classcombos$Dyeing >= input$skill.Dyeing & 
                                    classcombos$Feather.Working >= input$skill.Feather.Working & 
                                    classcombos$Fletchery >= input$skill.Fletchery & 
                                    classcombos$Floristry >= input$skill.Floristry & 
                                    
                                    classcombos$Instrument.Making >= input$skill.Instrument.Making & 
                                    classcombos$Jewelry.Making >= input$skill.Jewelry.Making & 
                                    classcombos$Knife.Making >= input$skill.Knife.Making & 
                                    classcombos$Leatherworking >= input$skill.Leatherworking & 
                                    classcombos$Lumberjacking >= input$skill.Lumberjacking & 
                                    classcombos$Pick.Making >= input$skill.Pick.Making & 
                                    classcombos$Spear.Making >= input$skill.Spear.Making & 
                                    classcombos$Stonecrafting >= input$skill.Stonecrafting & 
                                    classcombos$Sword.Making >= input$skill.Sword.Making & 
                                    classcombos$Tanning >= input$skill.Tanning & 
                                    
                                    
                                    classcombos$Tent.Making >= input$skill.Tent.Making & 
                                    classcombos$Tool.Making >= input$skill.Tool.Making & 
                                    classcombos$Wagon.Making >= input$skill.Wagon.Making & 
                                    classcombos$Woodworking >= input$skill.Woodworking & 
                                    
                                    classcombos$Bandage >= input$skill.Bandage & 
                                    classcombos$Pick >= input$skill.Pick & 
                                    classcombos$Pilot >= input$skill.Pilot & 
                                    classcombos$Poisoning >= input$skill.Poisoning & 
                                    classcombos$Ride >= input$skill.Ride & 
                                    classcombos$Skinning >= input$skill.Skinning & 
                                    classcombos$Sleight.of.Hand >= input$skill.Sleight.of.Hand & 
                                    classcombos$Steal >= input$skill.Steal & 
                                    classcombos$Direction.Sense >= input$skill.Direction.Sense & 
                                    classcombos$Forage >= input$skill.Forage & 
                                    classcombos$Hunt.city >= input$skill.Hunt.city & 
                                    classcombos$Hunt.wilderness >= input$skill.Hunt.wilderness & 
                                    classcombos$Listen.city >= input$skill.Listen.city & 
                                    classcombos$Listen.wilderness >= input$skill.Listen.wilderness & 
                                    classcombos$Peek >= input$skill.Peek & 
                                    classcombos$Scan >= input$skill.Scan & 
                                    classcombos$Search >= input$skill.Search & 
                                    classcombos$Watch >= input$skill.Watch & 
                                    classcombos$Alcohol.Tolerance.Perk >= input$skill.Alcohol.Tolerance.Perk & 
                                    classcombos$Better.Movement.Regeneration >= input$skill.Better.Movement.Regeneration & 
                                    
                                    classcombos$City.Forage >= input$skill.City.Forage & 
                                    classcombos$City.Hunt >= input$skill.City.Hunt & 
                                    classcombos$City.Stealth >= input$skill.City.Stealth & 
                                    classcombos$Customcraft >= input$skill.Customcraft & 
                                    classcombos$Hitch.2.Mounts >= input$skill.Hitch.2.Mounts & 
                                    classcombos$Learn.Languages.Perk >= input$skill.Learn.Languages.Perk & 
                                    classcombos$Learn.Ride.Faster >= input$skill.Learn.Ride.Faster & 
                                    classcombos$Pain.Tolerance.Perk >= input$skill.Pain.Tolerance.Perk & 
                                    classcombos$Tame.Mounts >= input$skill.Tame.Mounts & 
                                    classcombos$Wilderness.Forage >= input$skill.Wilderness.Forage & 
                                    classcombos$Wilderness.Hunt >= input$skill.Wilderness.Hunt & 
                                    classcombos$Wilderness.Quit >= input$skill.Wilderness.Quit & 
                                    classcombos$Wilderness.Stealth >= input$skill.Wilderness.Stealth & 
                                    classcombos$Climb >= input$skill.Climb & 
                                    classcombos$Hide.city >= input$skill.Hide.city & 
                                    classcombos$Hide.wilderness >= input$skill.Hide.wilderness & 
                                    classcombos$Sneak.city >= input$skill.Sneak.city & 
                                    classcombos$Sneak.wilderness >= input$skill.Sneak.wilderness & 
                                    classcombos$Bludgeoning.Weapons >= input$skill.Bludgeoning.Weapons & 
                                    classcombos$Chopping.Weapons >= input$skill.Chopping.Weapons & 
                                    classcombos$Piercing.Weapons >= input$skill.Piercing.Weapons & 
                                    classcombos$Slashing.Weapons >= input$skill.Slashing.Weapons, 'classcombo']
    
    skillpickerdat
    
  })
  
  output$tableSkillPicker <- renderTable(skillpickerdat())
    
}

# Run the application 
shinyApp(ui = ui, server = server)

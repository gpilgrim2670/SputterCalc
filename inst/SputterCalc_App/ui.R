library(shiny)

# Define UI for application
shinyUI(fluidPage(
    fluidRow(
        # first column is gun 1
        # numericInput for each gun parameter
        # should add limits
        column(width = 2,
               h4(strong("Gun 1")),
               numericInput("Rate_1", label = "Rate", value = 1000, step = 1),
               numericInput("Epsilon_1", label = "Epsilon", value = 1, step = 0.1),
               numericInput("Disc_1_1", label = "Disc 1", value = 0, step = 0.1),
               numericInput("Disc_2_1", label = "Disc 2", value = 1, step = 0.1),
               numericInput("Rad_1_1", label = "Rad 1", value = 1.4375, step = 0.0001),
               numericInput("Rad_2_1", label = "Rad 2", value = 3, step = 0.1),
               numericInput("Ins_1", label = "Ins", value = 2.375, step = 0.001),
               numericInput("Fing_1", label = "Finger", value = 4.8125, step = 0.0001),
               numericInput("Knuc_1", label = "Knuckle", value = 0.95, step = 0.00001),
               numericInput("Rot_1", label = "Rot", value = 1, step = 0.1)),
        # second column is main plot
        # numericInput for Interval, which will be the same for both guns
        # displays plots
        column(width = 8,
               h4(strong("Deposition Plot")),
               numericInput("Interval", label = "Interval", value = 0.2),
               # selectizeInput(inputId = "Coords",
               #                label = "Select Coordinate System",
               #                choices = c("polar", "cartesian")),
               plotOutput(outputId = "main_plot")),
        # third column is gun 2
        # numericInput for each gun parameter
        # should add limits
        column(width = 2,
               h4(strong("Gun 2")),
               numericInput("Rate_2", label = "Rate", value = 1000, step = 1),
               numericInput("Epsilon_2", label = "Epsilon", value = 1, step = 0.1),
               numericInput("Disc_1_2", label = "Disc 1", value = 0, step = 0.1),
               numericInput("Disc_2_2", label = "Disc 2", value = 0, step = 0.1),
               numericInput("Rad_1_2", label = "Rad 1", value = 1.4375, step = 0.0001),
               numericInput("Rad_2_2", label = "Rad 2", value = 3, step = 0.1),
               numericInput("Ins_2", label = "Ins", value = 2.375, step = 0.001),
               numericInput("Fing_2", label = "Finger", value = 4.8125, step = 0.0001),
               numericInput("Knuc_2", label = "Knuckle", value = 0.87266, step = 0.00001),
               numericInput("Rot_2", label = "Rot", value = 1, step = 0.1))
    )
))



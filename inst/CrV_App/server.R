library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Define server logic
shinyServer(function(input, output) {
    Gun_1_df <- reactive({
        Gun_1_df <- dep_data(
            Rate = input$Rate_1,
            Epsilon = input$Epsilon_1,
            Disc_1 = input$Disc_1_1,
            Disc_2 = input$Disc_2_1,
            Rad_1 = input$Rad_1_1,
            Rad_2 = input$Rad_2_1,
            Ins_1 = input$Ins_1,
            Fing_1 = input$Fing_1,
            Knuc_1 = input$Knuc_1,
            Rot_1 = input$Rot_1,
            Interval = input$Interval,
            coord = "polar"
        )

        return(Gun_1_df)
    })

    Gun_2_df <- reactive({
        Gun_2_df <- dep_data(
            Rate = input$Rate_2,
            Epsilon = input$Epsilon_2,
            Disc_1 = input$Disc_1_2,
            Disc_2 = input$Disc_2_2,
            Rad_1 = input$Rad_1_2,
            Rad_2 = input$Rad_2_2,
            Ins_1 = input$Ins_2,
            Fing_1 = input$Fing_2,
            Knuc_1 = input$Knuc_2,
            Rot_1 = input$Rot_2,
            Interval = input$Interval,
            coord = "polar"
        )

        return(Gun_2_df)
    })

    Gun_Both <- reactive({
        Gun_Both <- left_join(Gun_1_df(), Gun_2_df(), by = "radian") %>%
            rowwise() %>%
            mutate(
                dep_sum = sum(dep_sum.x, dep_sum.y),
                Gun_1 = dep_sum.x / dep_sum,
                Gun_2 = dep_sum.y / dep_sum
            ) %>%
            pivot_longer(
                cols = c(Gun_1, Gun_2),
                names_to = "gun",
                values_to = "per"
            )

        return(Gun_Both)

    })


    output$main_plot <- renderPlot({
        Gun_Both() %>%
            ggplot(aes(
                x = radian,
                y = per,
                color = gun
            )) +
            geom_point() +
            geom_line() +
            scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
            theme_bw() +
            labs(y = "Percentage",
                 x = "Radius",
                 color = 'Source')


    })

})

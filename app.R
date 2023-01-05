# Packages ---------------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(bslib)

click <- NULL
# ui.R -------------------------------------------------------------------------

ui <- fluidPage(
    theme = bs_theme(
        bg = "#101010",
        fg = "#FDF7F7",
        primary = "#ED79F9",
        secondary = "#ED79F9",
        "progress-bar-bg" = "#0dcaf0",
        base_font = font_google("Questrial"),
        code_font = font_google("JetBrains Mono")
    ),
    
    titlePanel(h2(
        strong("Grouped and Stacked Barplot"),
        align = "center"
    )),
    
    fileInput(
        inputId = "filedata",
        NULL,
        buttonLabel = "Upload data - Choose csv file",
        placeholder = "No file uploaded",
        width = '25%',
        accept = ".csv"
    ),
    
    
    sidebarLayout(
        position = "left",
        
        fluid = TRUE,
        
        sidebarPanel(
            width = 3,
            
            selectInput(
                inputId = "bar_type",
                label = "1. Select Type of Bar Plot",
                choices = c("dodge", "stack", "fill"),
                selected = "fill"
            ),
            
            selectInput(
                inputId = "x_data",
                label = "2. Variable - 1 (represents the bar)",
                choices = colnames(data()),
            ),
            
            selectInput(
                inputId = "fill_var",
                label = "3. Variable - 2 (splits the bar)",
                choices = colnames(data()),
            ),
            
            selectInput(
                inputId = "score_var",
                label = "4. Numerical Variable",
                choices = colnames(data()),
            ),
            
            radioButtons(
                inputId = "flip_choice",
                label = "5. Flip the axis",
                choices = c("Yes", "No"),
                inline = TRUE,
                selected = "No"
            ),
            
            textInput(inputId = "x_label",
                      label = "6. Enter text for X-axis label"),
            
            textInput(inputId = "y_label",
                      label = "7. Enter text for Y-axis label"),
            
            sliderInput(
                inputId = "axis_size",
                label = "8. Size of All Text ",
                min = 10,
                max = 40,
                value = 20
            ),
            
            sliderInput(
                inputId = "width",
                label = "9. Width of the plot ",
                min = 400,
                max = 1200,
                value = 850
            ),
            
            sliderInput(
                inputId = "height",
                label = "10. height of the plot ",
                min = 400,
                max = 1200,
                value = 600
            )
            
        ),
        
        mainPanel(
            tabsetPanel(
                br(),
                tabPanel("Stacked Plot", plotOutput(outputId = "bar_plot"),),
                tabPanel(
                    "Download Plot",
                    
                    fluidRow(
                        column(
                            3,
                            
                            numericInput("user_width", label = "Enter Width in cm", value = 20)
                        ),
                        
                        
                        column(
                            3,
                            
                            numericInput("user_height", label = "Enter height in cm", value = 20)
                        ),
                        
                        radioButtons(
                            "extension",
                            "Save As:",
                            choices = c("png", "pdf", "jpeg"),
                            inline = TRUE
                        )
                        
                    ),
                    
                    br(),
                    
                    fluidRow(column(
                        width = 3,
                        offset = 3,
                        downloadButton("download", label = "Download plot", class = "butt")
                    )),
                    
                    br(),
                    hr(),
                    includeHTML("./www/down_page.html")
                ),
                
                tabPanel(
                    "Source Data Table",
                    DT::dataTableOutput(outputId = "input_table"),
                ),
                
                tabPanel(
                    "Help",
                    includeHTML("./www/help.html"),
                    tags$head(tags$style(HTML("a {color: #90C7F2}"))),
                    a(href = "https://www.nature.com/articles/nmeth.2807", "click here for a guide to stacked plot")
                ),
                
                tabPanel(
                    "About",
                    h5("Made using SHINY"),
                    img(
                        src = "SHINY.jpeg",
                        width = "100px",
                        height = "80px"
                        
                    )
                )
            )
        )
        
    )
    
    
)




# server.R ---------------------------------------------------------------------


server <- function(input, output, session) {
    # assigning the reactive expression to a variable and calling the variable when required
    
    data <- reactive({
        if (is.null(input$filedata)) {
            read_csv(file = "./data/default_data.csv")
        } else {
            read_csv(req(input$filedata$datapath))
        }
    })
    
    observe({
        updateSelectInput(session, "x_data", choices = colnames(data()))
        
        
    })
    
    
    observe({
        updateSelectInput(session, "fill_var", choices = colnames(data()))
        
        
    })
    
    observe({
        updateSelectInput(session, "score_var", choices = colnames(data()))
        
        
    })
    
    
    output$input_table <- DT::renderDataTable(data())
    
    
    plot_output <- reactive({
        ggplot(data = data() ,
               aes(
                   # req() avoids the error message due to NULL value when starting
                   
                   x = factor(get(req(
                       input$x_data
                   ))),
                   y = get(req(input$score_var)),
                   fill = get(input$fill_var) ,
                   colour = get(input$fill_var)
               )) +
            
            geom_bar(position = input$bar_type , stat = "identity") +
            
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank() ,
                legend.title = element_blank()
            ) +
            
            labs(x = input$x_label, y = input$y_label) +
            
            {
                if (input$flip_choice == "Yes")
                    coord_flip()
            } +
            
            
            
            theme(axis.title = element_text(angle = 0),
                  text = element_text(size = input$axis_size))
        
        
        
    })
    
    output$bar_plot <-
        renderPlot(
            plot_output(),
            width = function()
                input$width,
            height = function()
                input$height
        )
    
    
    output$download <- downloadHandler(
        filename = function() {
            paste0(substr(input$filedata, 1, nchar(input$filedata) - 3), input$extension)
        },
        
        content = function(file) {
            ggsave(
                file,
                plot =  plot_output(),
                device = input$extension,
                width = input$user_width,
                height = input$user_height,
                units = "cm"
            )
        }
        
    )
    
}


# Run the app ------------------------------------------------------------------

shinyApp(ui = ui, server = server)
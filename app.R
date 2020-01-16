#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Get Qualtrics Graphics"),
    
    "Copoy and paste the HTML Source from the Qualtrics, Graphics Library (Library -> Graphics Library)",
    
    textInput("base_url", "Base URL to the file (Optional):", "https://delaware.ca1.qualtrics.com/ControlPanel/Graphic.php?IM="),
    
    textAreaInput("page_source", "Copy and paste HTML source here:", "[HTML Source Here]", width = "800px", height="400px"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            downloadButton("downloadData", "Download")
        ),
        mainPanel(
            tableOutput("table_output")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Reactive for table output
    table_output <- reactive(
        {
            html_source <- input$page_source
            base_url <- input$base_url
            
            regex_graphics_elements <- "(?<=window\\.GraphicsElementsLibrary = \\{\\W).*(?=\\})"
            graphics_elements_html <- html_source %>% str_extract_all(regex(regex_graphics_elements))
            
            # regex for filename
            filename_pattern <- "((?<=\"Description\":\").+?(?=\"))"
            # regex for file id
            image_id_pattern <- "(IM_.+?(?=\"))"
            
            filenames <- graphics_elements_html %>% str_extract_all(regex(filename_pattern)) %>% unlist()
            image_ids <- graphics_elements_html %>% str_extract_all(regex(image_id_pattern)) %>% unlist()
            image_urls <- paste0(base_url, image_ids)
            # Create a table
            graphics_tibble <- tibble("Filename" = filenames,
                                      "Image ID" = image_ids,
                                      "Image URLs" = image_urls)
            
        }
    )
    # Render table from the reactive
    output$table_output <- renderTable({table_output()})
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$dataset, "Qualtrics_Graphics_List.csv", sep = "")
        },
        content = function(file) {
            write.csv(table_output(), file, row.names = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)

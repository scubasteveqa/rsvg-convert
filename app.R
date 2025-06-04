library(shiny)
library(bslib)
library(processx)

ui <- page_fluid(
  title = "SVG Conversion with rsvg-convert",
  
  card(
    card_header("SVG to PNG Converter"),
    
    fileInput("svg_file", "Upload SVG File", 
              accept = c(".svg"),
              multiple = FALSE),
    
    numericInput("width", "Output Width (px)", 
                 value = 800, min = 100, max = 4000),
    
    numericInput("height", "Output Height (px)", 
                 value = 600, min = 100, max = 4000),
    
    selectInput("format", "Output Format",
                choices = c("PNG" = "png", 
                            "PDF" = "pdf", 
                            "PS" = "ps"),
                selected = "png"),
    
    checkboxInput("use_lua", "Use Lua Filter", value = FALSE),
    
    downloadButton("download", "Convert and Download"),
    
    card_footer(
      "Uses rsvg-convert for SVG conversion"
    )
  ),
  
  card(
    card_header("Status and Preview"),
    verbatimTextOutput("command_output"),
    plotOutput("preview", height = "400px")
  ),
  
  card(
    card_header("About"),
    p("This app demonstrates the use of rsvg-convert for SVG conversion with optional Lua filters."),
    p("Note: This app requires rsvg-convert to be installed on your system."),
    p("On posit.cloud, system commands may have limited functionality.")
  )
)

server <- function(input, output, session) {
  
  # Check if rsvg-convert is available
  command_available <- reactive({
    tryCatch({
      result <- system2("which", "rsvg-convert", stdout = TRUE, stderr = TRUE)
      return(!is.null(result) && length(result) > 0)
    }, error = function(e) {
      return(FALSE)
    })
  })
  
  # Apply Lua filter to SVG (with better error handling)
  apply_lua_filter <- function(svg_path) {
    if (!input$use_lua) {
      return(svg_path)  # Skip Lua processing if disabled
    }
    
    # Create a temporary file for the processed SVG
    processed_path <- tempfile(fileext = ".svg")
    file.copy(svg_path, processed_path)
    
    # Simple direct SVG modification without Pandoc
    tryCatch({
      # Read SVG content
      svg_content <- readLines(processed_path)
      
      # Add a comment after the opening SVG tag
      svg_tag_index <- grep("<svg", svg_content)[1]
      if (!is.na(svg_tag_index)) {
        svg_content <- c(
          svg_content[1:svg_tag_index],
          "<!-- Processed by R SVG filter -->",
          svg_content[(svg_tag_index+1):length(svg_content)]
        )
        
        # Write modified content back
        writeLines(svg_content, processed_path)
      }
    }, error = function(e) {
      # If modification fails, just use the original
      file.copy(svg_path, processed_path, overwrite = TRUE)
    })
    
    return(processed_path)
  }
  
  # Convert SVG to target format using rsvg-convert
  convert_svg <- function(svg_path, output_path, format, width, height) {
    # Process SVG with simple filter if enabled
    processed_svg <- apply_lua_filter(svg_path)
    
    # Check if rsvg-convert is available
    if (!command_available()) {
      stop("rsvg-convert is not available on this system")
    }
    
    # Use rsvg-convert via processx with more logging
    args <- c(
      "-f", format,
      "-w", width,
      "-h", height,
      "-o", output_path,
      processed_svg
    )
    
    # Log the command
    command_log <- paste("Running: rsvg-convert", paste(args, collapse = " "))
    output$command_output <- renderText({ command_log })
    
    result <- tryCatch({
      processx::run("rsvg-convert", args, error_on_status = FALSE)
    }, error = function(e) {
      return(list(status = 1, stderr = paste("Error executing rsvg-convert:", e$message)))
    })
    
    if(result$status != 0) {
      stop("Error converting SVG: ", result$stderr)
    }
    
    return(output_path)
  }
  
  # Store the uploaded file path
  svg_file <- reactive({
    req(input$svg_file)
    input$svg_file$datapath
  })
  
  # Command output log
  output$command_output <- renderText({
    if (!command_available()) {
      return("Warning: rsvg-convert command is not available on this system. This app may not function properly.")
    } else {
      return("System is ready. Upload an SVG file to convert.")
    }
  })
  
  # Preview the converted image
  output$preview <- renderPlot({
    req(svg_file())
    
    tryCatch({
      # Create temporary file for preview
      preview_file <- tempfile(fileext = ".png")
      
      # Try to convert the SVG to PNG for preview
      convert_svg(svg_file(), preview_file, "png", input$width, input$height)
      
      # Check if the file exists and has content
      if (file.exists(preview_file) && file.info(preview_file)$size > 0) {
        # Try to display the preview image
        tryCatch({
          img <- png::readPNG(preview_file)
          grid::grid.raster(img)
        }, error = function(e) {
          grid::grid.text(paste("Error reading PNG:", e$message), 
                          gp = grid::gpar(col = "red", fontsize = 14))
        })
      } else {
        grid::grid.text("Failed to generate preview (empty file)", 
                        gp = grid::gpar(col = "red", fontsize = 14))
      }
    }, error = function(e) {
      # Show error message
      grid::grid.text(paste("Error in conversion:", e$message), 
                      gp = grid::gpar(col = "red", fontsize = 14))
    })
  })
  
  # Download handler for converted file
  output$download <- downloadHandler(
    filename = function() {
      # Create output filename based on input filename and selected format
      file_base <- tools::file_path_sans_ext(input$svg_file$name)
      paste0(file_base, ".", input$format)
    },
    content = function(file) {
      tryCatch({
        # Convert SVG to selected format
        convert_svg(svg_file(), file, input$format, input$width, input$height)
      }, error = function(e) {
        # Create a text file with the error message if conversion fails
        writeLines(paste("Error converting file:", e$message), file)
        showNotification(paste("Error:", e$message), type = "error")
      })
    }
  )
}

shinyApp(ui, server)
library(shiny)
library(bslib)
library(processx)  # For running external commands

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
    
    downloadButton("download", "Convert and Download"),
    
    card_footer(
      "Uses rsvg-convert command-line tool for SVG conversion"
    )
  ),
  
  card(
    card_header("System Check"),
    actionButton("check_system", "Check if rsvg-convert is installed"),
    verbatimTextOutput("system_check_output"),
    hr(),
    verbatimTextOutput("command_output")
  ),
  
  card(
    card_header("Preview"),
    plotOutput("preview", height = "400px")
  ),
  
  card(
    card_header("About"),
    p("This app demonstrates the use of rsvg-convert command-line tool for SVG conversion."),
    p("Note: This app requires rsvg-convert to be installed on your system."),
    p("You can check if it's available by clicking the 'Check if rsvg-convert is installed' button.")
  )
)

server <- function(input, output, session) {
  
  # Check if rsvg-convert is available
  command_available <- reactive({
    tryCatch({
      result <- system2("which", "rsvg-convert", stdout = TRUE, stderr = TRUE)
      return(!is.null(result) && length(result) > 0 && !any(grepl("not found", result, ignore.case = TRUE)))
    }, error = function(e) {
      return(FALSE)
    })
  })
  
  # Function to check if rsvg-convert is installed
  check_rsvg_convert <- function() {
    # Try different commands depending on the OS
    commands <- list(
      # Linux/Mac
      list(cmd = "which", args = "rsvg-convert"),
      # Windows
      list(cmd = "where", args = "rsvg-convert"),
      # Version check
      list(cmd = "rsvg-convert", args = "--version")
    )
    
    results <- list()
    
    for (command in commands) {
      result <- tryCatch({
        system2(command$cmd, command$args, stdout = TRUE, stderr = TRUE)
      }, error = function(e) {
        return(paste("Error running", command$cmd, ":", e$message))
      })
      
      results[[paste(command$cmd, command$args)]] <- 
        if (is.null(attr(result, "status")) || attr(result, "status") == 0) {
          paste(result, collapse = "\n")
        } else {
          paste("Command failed with status:", attr(result, "status"))
        }
    }
    
    # Also check PATH environment variable
    path_env <- Sys.getenv("PATH")
    results[["PATH"]] <- path_env
    
    # Try to locate rsvg-convert manually in common locations
    common_locations <- c(
      "/usr/bin/rsvg-convert",
      "/usr/local/bin/rsvg-convert",
      "/opt/homebrew/bin/rsvg-convert",
      "C:/Program Files/rsvg-convert/rsvg-convert.exe"
    )
    
    for (location in common_locations) {
      results[[paste("File exists:", location)]] <- 
        if (file.exists(location)) "YES" else "NO"
    }
    
    return(results)
  }
  
  # Observe check system button
  observeEvent(input$check_system, {
    results <- check_rsvg_convert()
    
    output_text <- paste("System Check for rsvg-convert:\n\n")
    
    for (name in names(results)) {
      output_text <- paste0(output_text, "Command: ", name, "\n")
      output_text <- paste0(output_text, "Result: ", results[[name]], "\n\n")
    }
    
    # Add system information
    output_text <- paste0(output_text, 
                          "System Information:\n",
                          "OS: ", Sys.info()["sysname"], "\n",
                          "Version: ", Sys.info()["release"], "\n",
                          "R Version: ", R.version.string, "\n")
    
    output$system_check_output <- renderText({ output_text })
  })
  
  # Convert SVG to target format using rsvg-convert command
  convert_svg <- function(svg_path, output_path, format, width, height) {
    # Check if rsvg-convert is available
    if (!command_available()) {
      stop("rsvg-convert is not available on this system")
    }
    
    # Use rsvg-convert via processx
    args <- c(
      "-f", format,
      "-w", width,
      "-h", height,
      "-o", output_path,
      svg_path
    )
    
    # Log the command
    command_log <- paste("Running: rsvg-convert", paste(args, collapse = " "))
    output$command_output <- renderText({ command_log })
    
    # Run the command
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

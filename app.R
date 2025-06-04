library(shiny)
library(bslib)
library(rsvg)  # For SVG processing in R
library(grDevices)
library(png)
library(ggplot2)
library(svglite)

ui <- page_fluid(
  title = "SVG Conversion in R",
  
  card(
    card_header("SVG Converter"),
    
    fileInput("svg_file", "Upload SVG File", 
              accept = c(".svg"),
              multiple = FALSE),
    
    numericInput("width", "Output Width (px)", 
                 value = 800, min = 100, max = 4000),
    
    numericInput("height", "Output Height (px)", 
                 value = 600, min = 100, max = 4000),
    
    selectInput("format", "Output Format",
                choices = c("PNG" = "png", 
                           "PDF" = "pdf"),
                selected = "png"),
    
    checkboxInput("add_filter", "Add Simple Filter", value = FALSE),
    
    downloadButton("download", "Convert and Download"),
    
    card_footer(
      "Uses R packages for SVG conversion"
    )
  ),
  
  card(
    card_header("System Check"),
    actionButton("check_system", "Check if rsvg-convert is installed"),
    verbatimTextOutput("system_check_output"),
    hr(),
    verbatimTextOutput("status_output")
  ),
  
  card(
    card_header("Preview"),
    plotOutput("preview", height = "400px")
  ),
  
  card(
    card_header("About"),
    p("This app demonstrates SVG conversion using R packages instead of system commands."),
    p("The app uses the 'rsvg' and 'svglite' packages to process SVG files."),
    p("This version works on posit.cloud without requiring external system commands."),
    p("You can check if rsvg-convert is available on your system using the system check button.")
  )
)

server <- function(input, output, session) {
  
  # Check if required packages are available
  packages_available <- reactive({
    all(c("rsvg", "png", "svglite") %in% installed.packages()[,"Package"])
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
  
  # Process SVG content (add filter if requested)
  process_svg <- function(svg_path) {
    if (!input$add_filter) {
      return(svg_path)  # Skip processing if disabled
    }
    
    # Create a temporary file for the processed SVG
    processed_path <- tempfile(fileext = ".svg")
    
    tryCatch({
      # Read SVG content
      svg_content <- readLines(svg_path)
      
      # Add a simple filter definition after the opening SVG tag
      svg_tag_index <- grep("<svg", svg_content)[1]
      if (!is.na(svg_tag_index)) {
        # Simple blue tint filter
        filter_def <- c(
          "<defs>",
          "  <filter id=\"blue-tint\">",
          "    <feColorMatrix type=\"matrix\" values=\"0.9 0 0 0 0 0 0.9 0 0 0 0 0 1.2 0 0 0 0 0 1 0\"/>",
          "  </filter>",
          "</defs>"
        )
        
        # Apply filter to the SVG content
        svg_content <- c(
          svg_content[1:svg_tag_index],
          filter_def,
          # Modify the root group to use the filter
          gsub("<g", "<g filter=\"url(#blue-tint)\"", svg_content[(svg_tag_index+1):length(svg_content)], fixed = FALSE)
        )
        
        # Write modified content back
        writeLines(svg_content, processed_path)
        return(processed_path)
      } else {
        # If no SVG tag found, return original
        return(svg_path)
      }
    }, error = function(e) {
      # If modification fails, just use the original
      return(svg_path)
    })
  }
  
  # Convert SVG to target format using R packages
  convert_svg <- function(svg_path, output_path, format, width, height) {
    # Process SVG with simple filter if enabled
    processed_svg <- process_svg(svg_path)
    
    # Convert SVG to the target format
    if (format == "png") {
      tryCatch({
        # Use rsvg package to convert SVG to PNG
        rsvg::rsvg_png(processed_svg, output_path, width = width, height = height)
      }, error = function(e) {
        stop("Error converting to PNG: ", e$message)
      })
    } else if (format == "pdf") {
      tryCatch({
        # Use rsvg package to convert SVG to PDF
        rsvg::rsvg_pdf(processed_svg, output_path, width = width/72, height = height/72)
      }, error = function(e) {
        stop("Error converting to PDF: ", e$message)
      })
    } else {
      stop("Unsupported output format: ", format)
    }
    
    return(output_path)
  }
  
  # Store the uploaded file path
  svg_file <- reactive({
    req(input$svg_file)
    input$svg_file$datapath
  })
  
  # Status output
  output$status_output <- renderText({
    if (!packages_available()) {
      return("Warning: Required packages (rsvg, png, svglite) are not all installed. Please install them with install.packages().")
    } else {
      if (!is.null(input$svg_file)) {
        return(paste("Processing file:", input$svg_file$name))
      } else {
        return("System is ready. Upload an SVG file to convert.")
      }
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

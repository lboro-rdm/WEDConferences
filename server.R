server <- function(input, output, session) {
  
  # Reactive function to read and format the data from the CSV file
  booksData <- reactive({
    csv_file <- "combined_data.csv"
    if (file.exists(csv_file)) {
      read.csv(csv_file, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })
  
  # Populate the dropdown with unique collection names
  observe({
    df <- booksData()
    if (!is.null(df)) {
      updateSelectInput(
        session,
        "collectionSelect",
        choices = c("All", unique(df$collection_title)),  # Add "All" option
        selected = "All"
      )
    }
  })
  
  # Handle URL parameters to preselect a conference
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$conference)) {
      conference_name <- query$conference
      df <- booksData()
      if (!is.null(df) && conference_name %in% df$collection_title) {
        updateSelectInput(session, "collectionSelect", selected = conference_name)
      }
    }
  })
  
  # Update the URL when a conference is selected
  observeEvent(input$collectionSelect, {
    if (!is.null(input$collectionSelect) && input$collectionSelect != "All") {
      # Extract the part of the string before the first comma
      conference_number <- sub(",.*", "", input$collectionSelect)
      new_url <- paste0("?conference=", URLencode(conference_number, reserved = TRUE))
      updateQueryString(new_url, mode = "replace")
    } else {
      updateQueryString("", mode = "replace")
    }
  })
  
  # Reactive function to filter books based on inputs
  filteredBooks <- reactive({
    df <- booksData()
    if (is.null(df)) return(NULL)
    
    # Apply filters
    if (!is.null(input$collectionSelect) && input$collectionSelect != "All" && input$collectionSelect != "") {
      df <- df[df$collection_title == input$collectionSelect, ]
    }
    if (!is.null(input$authorSearch) && input$authorSearch != "") {
      df <- df[grepl(input$authorSearch, df$Author, ignore.case = TRUE), ]
    }
    if (!is.null(input$titleSearch) && input$titleSearch != "") {
      df <- df[grepl(input$titleSearch, df$title, ignore.case = TRUE), ]
    }
    
    # Remove duplicate entries based on unique fields (e.g., title and Author)
    if (!is.null(df) && nrow(df) > 0) {
      df <- df %>%
        dplyr::distinct(title, Author, .keep_all = TRUE)
    }
    
    # Sort by title alphabetically
    if (!is.null(df) && nrow(df) > 0) {
      df <- df[order(df$title, decreasing = FALSE), ]
    }
    
    df
  })
  
  # Reactive function to format the filtered data
  formattedBooks <- reactive({
    df <- filteredBooks()
    if (!is.null(df) && nrow(df) > 0) {
      # Create formatted strings with proper links
      formatted_strings <- sapply(1:nrow(df), function(i) {
        # Determine the appropriate link (hdl, doi, or plain text)
        link <- if (df$hdl[i] != "" && !is.na(df$hdl[i])) {
          paste0("<a href='", df$hdl[i], "' style='color: #002c3d; text-decoration: underline;' target='_blank' class='hover-underline'>", df$title[i], "</a>")
        } else if (df$doi[i] != "" && !is.na(df$doi[i])) {
          paste0("<a href='", df$doi[i], "' style='color: #002c3d; text-decoration: underline;' target='_blank' class='hover-underline'>", df$title[i], "</a>")
        } else {
          paste0("<span style='color: #002c3d;'>", df$title[i], "</span>")
        }
        
        # Format the full entry
        paste0(
          "<div style='margin-bottom: 10px;'>", # Add bottom margin
          "<strong>", link, "</strong>. ", 
          "<span style='color: #002c3d;'>", df$Author[i], ". (", 
          df$Year[i], "). Conference no. ", df$collection_title[i], "</span>",
          "</div>"
        )
      })
      
      # Return the formatted strings as a single string
      paste(formatted_strings, collapse = "")
    } else {
      "No results found."
    }
  })
  
  # Render the filtered and formatted books
  output$bookDetails <- renderUI({
    HTML(formattedBooks())
  })
}

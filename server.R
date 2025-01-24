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
      # Create a named vector where names are conference_ids and values are full titles
      choices <- setNames(df$collection_title, df$conference_id)
      choices <- unique(choices)  # Keep only unique titles
      choices <- c("All" = "All", choices)  # Add "All" option at the beginning
      
      updateSelectInput(
        session,
        "collectionSelect",
        choices = choices,  # Use the named vector for choices
        selected = "All"
      )
    }
  })
  
  # Handle URL parameters to preselect a conference
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$conference)) {
      conference_id <- query$conference  # Get conference_id from the URL
      df <- booksData()
      if (!is.null(df) && conference_id %in% df$conference_id) {
        # Get the corresponding conference name
        conference_name <- df$collection_title[df$conference_id == conference_id]
        updateSelectInput(session, "collectionSelect", selected = conference_name)
      }
    }
  })
  
  # Update the URL when a conference is selected
  observeEvent(input$collectionSelect, {
    if (!is.null(input$collectionSelect) && input$collectionSelect != "All") {
      # Get the selected conference title
      selected_conference_title <- input$collectionSelect
      
      # Get the conference ID that corresponds to the selected title
      df <- booksData()
      conference_id <- df$conference_id[df$collection_title == selected_conference_title][1]  # Get the first match
      
      if (!is.null(conference_id) && length(conference_id) > 0) {
        # Set the new URL to just the conference ID
        new_url <- paste0("?conference=", URLencode(as.character(conference_id), reserved = TRUE))
        updateQueryString(new_url, mode = "replace")  # Replace the existing URL
      }
    } else {
      updateQueryString("", mode = "replace")  # Clear the URL when "All" is selected
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
          "<span style='color: #002c3d;'>", df$Author[i], ". Conference no. ", df$collection_title[i], "</span>",
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

# Selenium-based Screenshot Utilities
# Uses RSelenium with proper cookie dialog handling

library(RSelenium)
library(wdman)

# Create screenshots directory if it doesn't exist
if (!dir.exists("screenshots")) {
  dir.create("screenshots")
}

# Function to start Selenium server
start_selenium_server <- function() {
  # Check if selenium server is already running
  tryCatch({
    remDr <- remoteDriver(port = 4444L, browserName = "chrome")
    remDr$open()
    remDr$close()
    return(TRUE)
  }, error = function(e) {
    cat("Starting Selenium server...\n")
    # Try to start selenium server with specific driver
    tryCatch({
      selenium(port = 4444L, version = "latest", chromever = "latest")
      Sys.sleep(5)  # Wait for server to start
      return(TRUE)
    }, error = function(e2) {
      cat("Failed to start Selenium server:", e2$message, "\n")
      return(FALSE)
    })
  })
}

# Function to take screenshot with Selenium and cookie handling
take_screenshot_selenium <- function(url, filename) {
  screenshot_path <- file.path("screenshots", filename)
  
  # Check if screenshot already exists and has content
  if (file.exists(screenshot_path) && file.size(screenshot_path) > 5000) {
    cat("Screenshot already exists:", screenshot_path, "(", round(file.size(screenshot_path)/1024, 1), "KB)\n")
    return(screenshot_path)
  }
  
  cat("Taking screenshot with Selenium for:", url, "\n")
  
  # Remove existing empty file if it exists
  if (file.exists(screenshot_path)) {
    file.remove(screenshot_path)
  }
  
  # Start selenium server
  start_selenium_server()
  
  # Set up Chrome options
  chrome_options <- list(
    args = c(
      "--headless",
      "--no-sandbox",
      "--disable-dev-shm-usage",
      "--disable-gpu",
      "--window-size=1200,800",
      "--user-agent=Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
    )
  )
  
  remDr <- NULL
  
  tryCatch({
    # Start remote driver
    remDr <- remoteDriver(
      remoteServerAddr = "localhost",
      port = 4444L,
      browserName = "chrome",
      extraCapabilities = chrome_options
    )
    
    remDr$open()
    
    # Navigate to URL
    remDr$navigate(url)
    
    # Wait for page to load
    Sys.sleep(5)
    
    # Handle Facebook/Instagram cookie dialogs
    if (grepl("facebook\\.com|instagram\\.com", url, ignore.case = TRUE)) {
      cat("Facebook/Instagram detected - attempting to handle cookie dialog...\n")
      
      # Wait a bit for cookie dialog to appear
      Sys.sleep(3)
      
      # Try to find and click decline/reject buttons
      tryCatch({
        # Look for various decline button patterns
        decline_selectors <- c(
          "button[data-cookiebanner='accept_only_essential_button']",
          "button[aria-label*='Decline']",
          "button[aria-label*='Reject']",
          "button:contains('Decline')",
          "button:contains('Reject')",
          "button:contains('Not now')",
          "button:contains('Only essential')",
          "button:contains('Essential only')",
          "[role='button']:contains('Decline')",
          "[role='button']:contains('Reject')"
        )
        
        decline_clicked <- FALSE
        
        for (selector in decline_selectors) {
          tryCatch({
            element <- remDr$findElement(using = "css selector", value = selector)
            if (!is.null(element)) {
              element$clickElement()
              cat("Clicked decline button with selector:", selector, "\n")
              decline_clicked <- TRUE
              break
            }
          }, error = function(e) {
            # Try xpath if css selector fails
            tryCatch({
              xpath_selector <- paste0("//button[contains(text(), 'Decline') or contains(text(), 'Reject') or contains(text(), 'Not now') or contains(text(), 'Only essential')]")
              element <- remDr$findElement(using = "xpath", value = xpath_selector)
              if (!is.null(element)) {
                element$clickElement()
                cat("Clicked decline button with xpath\n")
                decline_clicked <- TRUE
              }
            }, error = function(e2) {
              # Continue to next selector
            })
          })
        }
        
        if (!decline_clicked) {
          cat("No decline button found, trying to find any clickable element with decline text...\n")
          
          # Try to find any element containing decline text
          tryCatch({
            elements <- remDr$findElements(using = "xpath", value = "//*[contains(text(), 'Decline') or contains(text(), 'Reject') or contains(text(), 'Not now')]")
            for (element in elements) {
              tryCatch({
                element$clickElement()
                cat("Clicked element with decline text\n")
                decline_clicked <- TRUE
                break
              }, error = function(e) {
                # Continue to next element
              })
            }
          }, error = function(e) {
            cat("Could not find any decline elements\n")
          })
        }
        
        if (decline_clicked) {
          cat("Cookie dialog handled successfully\n")
          Sys.sleep(2)  # Wait for dialog to close
        } else {
          cat("Could not find or click decline button - proceeding with screenshot\n")
        }
        
      }, error = function(e) {
        cat("Error handling cookie dialog:", e$message, "\n")
      })
    }
    
    # Wait a bit more for page to settle
    Sys.sleep(3)
    
    # Take screenshot
    remDr$screenshot(file = screenshot_path)
    
    # Check if screenshot was actually created and has content
    if (file.exists(screenshot_path) && file.size(screenshot_path) > 5000) {
      cat("✅ Screenshot saved:", screenshot_path, "(", round(file.size(screenshot_path)/1024, 1), "KB)\n")
    } else {
      cat("⚠️ Screenshot created but appears empty or very small (", 
          ifelse(file.exists(screenshot_path), file.size(screenshot_path), 0), " bytes)\n")
      if (file.exists(screenshot_path)) {
        file.remove(screenshot_path)
      }
    }
    
  }, error = function(e) {
    cat("❌ Failed to take screenshot for", url, ":", e$message, "\n")
  }, finally = {
    # Clean up
    if (!is.null(remDr)) {
      tryCatch({
        remDr$close()
      }, error = function(e) {
        # Ignore cleanup errors
      })
    }
  })
  
  return(screenshot_path)
}

# Function to generate image HTML with Selenium screenshot
generate_image_html_with_selenium_screenshot <- function(url) {
  # Create filename from URL
  filename <- gsub("[^a-zA-Z0-9]", "_", url)
  filename <- paste0(filename, ".png")
  
  # Take screenshot using Selenium
  screenshot_path <- take_screenshot_selenium(url, filename)
  
  # Check if screenshot exists and has content
  if (file.exists(screenshot_path) && file.size(screenshot_path) > 5000) {
    # Generate HTML with screenshot
    paste0(
      '<div class="screenshot-container">',
      '<h5>📸 Post Screenshot (Selenium)</h5>',
      '<img src="', screenshot_path, '" alt="Screenshot of post" style="max-width: 100%; height: auto;" />',
      '<p><a href="', url, '" target="_blank">🔗 View Original Post</a></p>',
      '</div>'
    )
  } else {
    # Fallback if screenshot failed
    paste0(
      '<div class="screenshot-container">',
      '<h5>⚠️ Screenshot Unavailable</h5>',
      '<p>Screenshot could not be generated for this URL.</p>',
      '<p><a href="', url, '" target="_blank">🔗 View Original Post</a></p>',
      '</div>'
    )
  }
}

# Function to batch process all URLs with Selenium
process_all_screenshots_selenium <- function(urls) {
  cat("=== Starting Selenium Screenshot Processing ===\n")
  cat("Total URLs to process:", length(urls), "\n\n")
  
  for (i in seq_along(urls)) {
    url <- urls[i]
    cat(sprintf("[%d/%d] Processing: %s\n", i, length(urls), url))
    
    filename <- gsub("[^a-zA-Z0-9]", "_", url)
    filename <- paste0(filename, ".png")
    
    take_screenshot_selenium(url, filename)
    cat("\n")
  }
  
  cat("=== Selenium Screenshot Processing Complete ===\n")
}

# Debug function to check existing screenshots
check_existing_screenshots <- function() {
  if (!dir.exists("screenshots")) {
    cat("Screenshots directory does not exist.\n")
    return(data.frame())
  }
  
  screenshot_files <- list.files("screenshots", pattern = "\\.png$", full.names = TRUE)
  
  if (length(screenshot_files) == 0) {
    cat("No screenshots found in screenshots/ directory.\n")
    return(data.frame())
  }
  
  cat("Found", length(screenshot_files), "existing screenshots:\n")
  
  screenshot_info <- data.frame(
    file = basename(screenshot_files),
    size_kb = round(file.size(screenshot_files) / 1024, 1),
    size_mb = round(file.size(screenshot_files) / 1024 / 1024, 2),
    exists = TRUE
  )
  
  print(screenshot_info)
  return(screenshot_info)
}

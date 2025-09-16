# Simple Screenshot Utilities
# Uses webshot2 with proper error handling

library(webshot2)

# Create screenshots directory if it doesn't exist
if (!dir.exists("screenshots")) {
  dir.create("screenshots")
}

# Function to take screenshot with better error handling
take_screenshot <- function(url, filename) {
  screenshot_path <- file.path("screenshots", filename)
  
  # Check if screenshot already exists and has content
  if (file.exists(screenshot_path) && file.size(screenshot_path) > 5000) {
    message("Screenshot already exists: ", screenshot_path, " (", round(file.size(screenshot_path)/1024, 1), "KB)")
    return(screenshot_path)
  }
  
  
  # Remove existing empty file if it exists
  if (file.exists(screenshot_path)) {
    file.remove(screenshot_path)
  }
  
  message("Taking screenshot for: ", url)
  
  tryCatch({
    # Take screenshot with appropriate delay based on platform
    if (grepl("facebook\\.com|instagram\\.com", url, ignore.case = TRUE)) {
      message("Facebook/Instagram detected - trying multiple approaches for cookie handling...")
      
      # Try method 1: Bot user agent (often bypasses cookie dialogs)
        tryCatch({
          webshot2::webshot(url, screenshot_path,
                           vwidth = 1200, vheight = 800,
                           delay = 15,
                           useragent = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
          message("Method 1 (bot user agent) succeeded")
        }, error = function(e1) {
          message("Method 1 failed: ", e1$message)
        
        # Try method 2: Mobile viewport with mobile user agent
        tryCatch({
          webshot2::webshot(url, screenshot_path, 
                           vwidth = 375, vheight = 667,
                           delay = 15,
                           useragent = "Mozilla/5.0 (iPhone; CPU iPhone OS 15_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.0 Mobile/15E148 Safari/604.1")
          message("Method 2 (mobile viewport) succeeded")
        }, error = function(e2) {
          message("Method 2 failed: ", e2$message)
          
          # Try method 3: Desktop with longer delay
          tryCatch({
            webshot2::webshot(url, screenshot_path, 
                             vwidth = 1200, vheight = 800,
                             delay = 25)
            message("Method 3 (desktop long delay) succeeded")
          }, error = function(e3) {
            message("Method 3 failed: ", e3$message)
            
            # Try method 4: Smaller desktop viewport
            tryCatch({
              webshot2::webshot(url, screenshot_path, 
                               vwidth = 800, vheight = 600,
                               delay = 20)
              message("Method 4 (smaller desktop) succeeded")
            }, error = function(e4) {
              message("Method 4 failed: ", e4$message)
              
              # Try method 5: Very small viewport
              tryCatch({
                webshot2::webshot(url, screenshot_path, 
                                 vwidth = 600, vheight = 400,
                                 delay = 15)
                message("Method 5 (very small) succeeded")
              }, error = function(e5) {
                message("Method 5 failed: ", e5$message)
                stop("All Facebook screenshot methods failed")
              })
            })
          })
        })
      })
    } else if (grepl("tiktok\\.com", url, ignore.case = TRUE)) {
      message("TikTok detected - using 10 second delay...")
      webshot2::webshot(url, screenshot_path,
                       vwidth = 1200, vheight = 800,
                       delay = 10)
    } else {
      message("Other platform - using 5 second delay...")
      webshot2::webshot(url, screenshot_path,
                       vwidth = 1200, vheight = 800,
                       delay = 5)
    }
    
    # Check if screenshot was actually created and has content
    if (file.exists(screenshot_path) && file.size(screenshot_path) > 5000) {
      message("✅ Screenshot saved: ", screenshot_path, " (", round(file.size(screenshot_path)/1024, 1), "KB)")
    } else {
      message("⚠️ Screenshot created but appears empty or very small (", 
              ifelse(file.exists(screenshot_path), file.size(screenshot_path), 0), " bytes)")
      if (file.exists(screenshot_path)) {
        file.remove(screenshot_path)
      }
    }

  }, error = function(e) {
    message("❌ Failed to take screenshot for ", url, ": ", e$message)
  })
  
  return(screenshot_path)
}

# Function to generate image HTML with screenshot
generate_image_html_with_screenshot <- function(url) {
  # Create filename from URL
  filename <- gsub("[^a-zA-Z0-9]", "_", url)
  filename <- paste0(filename, ".png")
  
  # Take screenshot
  screenshot_path <- take_screenshot(url, filename)
  
  # Check if screenshot exists and has content
  if (file.exists(screenshot_path) && file.size(screenshot_path) > 5000) {
    # Generate HTML with screenshot
    paste0(
      '<div class="screenshot-container">',
      '<h5>📸 Post Screenshot</h5>',
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

# Function to batch process all URLs
process_all_screenshots <- function(urls) {
  cat("=== Starting Screenshot Processing ===\n")
  cat("Total URLs to process:", length(urls), "\n\n")
  
  for (i in seq_along(urls)) {
    url <- urls[i]
    cat(sprintf("[%d/%d] Processing: %s\n", i, length(urls), url))
    
    filename <- gsub("[^a-zA-Z0-9]", "_", url)
    filename <- paste0(filename, ".png")
    
    take_screenshot(url, filename)
    cat("\n")
  }
  
  cat("=== Screenshot Processing Complete ===\n")
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

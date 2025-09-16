# Chromote-based Screenshot Utilities
# Uses chromote with proper cookie dialog handling via JavaScript

library(chromote)

# Create screenshots directory if it doesn't exist
if (!dir.exists("screenshots")) {
  dir.create("screenshots")
}

# Function to take screenshot with chromote and cookie handling
take_screenshot_chromote <- function(url, filename) {
  screenshot_path <- file.path("screenshots", filename)
  
  # Check if screenshot already exists and has content
  if (file.exists(screenshot_path) && file.size(screenshot_path) > 5000) {
    cat("Screenshot already exists:", screenshot_path, "(", round(file.size(screenshot_path)/1024, 1), "KB)\n")
    return(screenshot_path)
  }
  
  cat("Taking screenshot with chromote for:", url, "\n")
  
  # Remove existing empty file if it exists
  if (file.exists(screenshot_path)) {
    file.remove(screenshot_path)
  }
  
  b <- NULL
  
  tryCatch({
    # Start browser
    b <- ChromoteSession$new()
    
    # Set viewport
    b$view(width = 1200, height = 800)
    
    # Navigate to URL
    b$navigate(url)
    
    # Wait for page to load
    b$wait_for("domcontentloaded")
    Sys.sleep(3)
    
    # Handle Facebook/Instagram cookie dialogs
    if (grepl("facebook\\.com|instagram\\.com", url, ignore.case = TRUE)) {
      cat("Facebook/Instagram detected - attempting to handle cookie dialog...\n")
      
      # JavaScript to find and click decline buttons
      js_code <- "
        (function() {
          console.log('Looking for decline buttons...');
          
          // Function to find and click decline buttons
          function findAndClickDecline() {
            // Get all buttons and clickable elements
            var buttons = document.querySelectorAll('button, [role=\"button\"], a, div[tabindex]');
            var declineClicked = false;
            
            for (var i = 0; i < buttons.length; i++) {
              var button = buttons[i];
              var text = (button.textContent || button.innerText || '').toLowerCase();
              var ariaLabel = (button.getAttribute('aria-label') || '').toLowerCase();
              var combinedText = text + ' ' + ariaLabel;
              
              // Look for decline/reject text patterns
              if (combinedText.includes('decline') || 
                  combinedText.includes('reject') || 
                  combinedText.includes('not now') ||
                  combinedText.includes('only essential') ||
                  combinedText.includes('necessary only') ||
                  combinedText.includes('essential only') ||
                  combinedText.includes('decline optional') ||
                  combinedText.includes('decline cookies')) {
                
                console.log('Found decline button:', text);
                try {
                  button.click();
                  console.log('Clicked decline button successfully');
                  declineClicked = true;
                  break;
                } catch (e) {
                  console.log('Error clicking button:', e);
                }
              }
            }
            
            if (!declineClicked) {
              console.log('No decline button found in first pass, trying xpath...');
              
              // Try xpath approach
              var xpath = '//button[contains(translate(text(), \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\", \"abcdefghijklmnopqrstuvwxyz\"), \"decline\") or contains(translate(text(), \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\", \"abcdefghijklmnopqrstuvwxyz\"), \"reject\") or contains(translate(text(), \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\", \"abcdefghijklmnopqrstuvwxyz\"), \"not now\")]';
              var result = document.evaluate(xpath, document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null);
              var element = result.singleNodeValue;
              
              if (element) {
                try {
                  element.click();
                  console.log('Clicked decline button via xpath');
                  declineClicked = true;
                } catch (e) {
                  console.log('Error clicking xpath button:', e);
                }
              }
            }
            
            return declineClicked;
          }
          
          // Try to click decline button
          var clicked = findAndClickDecline();
          
          if (clicked) {
            console.log('Successfully clicked decline button');
            // Wait a bit for dialog to close
            setTimeout(function() {
              console.log('Cookie dialog should be closed now');
            }, 1000);
          } else {
            console.log('Could not find or click decline button');
          }
          
          return clicked;
        })();
      "
      
      # Execute JavaScript
      result <- b$Runtime$evaluate(js_code)
      
      if (result$result$value) {
        cat("Successfully clicked decline button\n")
        Sys.sleep(2)  # Wait for dialog to close
      } else {
        cat("Could not find or click decline button\n")
      }
    }
    
    # Wait a bit more for page to settle
    Sys.sleep(2)
    
    # Take screenshot
    b$screenshot(filename = screenshot_path)
    
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
    if (!is.null(b)) {
      tryCatch({
        b$close()
      }, error = function(e) {
        # Ignore cleanup errors
      })
    }
  })
  
  return(screenshot_path)
}

# Function to generate image HTML with chromote screenshot
generate_image_html_with_chromote_screenshot <- function(url) {
  # Create filename from URL
  filename <- gsub("[^a-zA-Z0-9]", "_", url)
  filename <- paste0(filename, ".png")
  
  # Take screenshot using chromote
  screenshot_path <- take_screenshot_chromote(url, filename)
  
  # Check if screenshot exists and has content
  if (file.exists(screenshot_path) && file.size(screenshot_path) > 5000) {
    # Generate HTML with screenshot
    paste0(
      '<div class="screenshot-container">',
      '<h5>📸 Post Screenshot (Chromote)</h5>',
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

# Function to batch process all URLs with chromote
process_all_screenshots_chromote <- function(urls) {
  cat("=== Starting Chromote Screenshot Processing ===\n")
  cat("Total URLs to process:", length(urls), "\n\n")
  
  for (i in seq_along(urls)) {
    url <- urls[i]
    cat(sprintf("[%d/%d] Processing: %s\n", i, length(urls), url))
    
    filename <- gsub("[^a-zA-Z0-9]", "_", url)
    filename <- paste0(filename, ".png")
    
    take_screenshot_chromote(url, filename)
    cat("\n")
  }
  
  cat("=== Chromote Screenshot Processing Complete ===\n")
}

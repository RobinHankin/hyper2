if (!requireNamespace("httr2", quietly = TRUE)) install.packages("httr2")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
library(httr2)

rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
output_file <- "man_pages_audit.txt"

token <- Sys.getenv("GITHUB_TOKEN")
if (token == "") stop("GITHUB_TOKEN environment variable is missing.")

system_prompt <- "
Act as a critical, first-time reader of the hyper2 package.
You will receive a batch of documentation (.Rd) files demarcated by FILE_PATH headers.
Analyze EACH file individually using the checklist below. Provide highly concise, bullet-point corrections. 
Organize your response clearly by file path.

CHECKLIST FOR EACH FILE:
1. What mathematical or statistical concepts are assumed but not explained?
2. Which notation is introduced without definition?
3. Which terms are used before they are motivated?
4. Which examples require prior knowledge not yet introduced?
5. Which function arguments have an obvious meaning to the author but not to a first-time user?
6. What questions would a careful reader naturally ask after reading this page?
7. Which other Rd pages should be cross-referenced but are not?
8. Is there a one-sentence statement that would greatly improve the reader's understanding?
"

cat("hyper2 Documentation Audit Results\n=============\n", file = output_file)

# Split files into batches of 5
batch_size <- 5
num_files <- length(rd_files)
num_batches <-  ceiling(num_files / batch_size)

for (b in 1:num_batches) {
  start_idx <- (b - 1) * batch_size + 1
  end_idx <- min(b * batch_size, num_files)
  batch_files <- rd_files[start_idx:end_idx]
  
  cat(sprintf("Processing batch %d of %d (Files %d to %d)...\n", b, num_batches, start_idx, end_idx))
  
  # Assemble content for this batch
  bundled_content <- ""
  for (f in batch_files) {
    rd_content <- paste(readLines(f, warn = FALSE), collapse = "\n")
    file_block <- paste0("\n---\nFILE_PATH: ", f, "\n```\n", rd_content, "\n```\n---\n")
    bundled_content <- paste0(bundled_content, file_block)
  }
  
  # Send API request for this batch
  req <- request("https://models.inference.ai.azure.com/chat/completions")
  req <- req_headers(req, Authorization = paste("Bearer", token))
  req <- req_body_json(req, list(
    model = "gpt-4o",
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = bundled_content)
    )
  ))
  
  resp <- req_perform(req)
  result <- resp_body_json(resp)$choices[[1]]$message$content
  
  cat(result, file = output_file, append = TRUE)
  cat("\n\n", file = output_file, append = TRUE)
  
  # Brief pause between batches to protect against HTTP 429
  if (b < num_batches) {
    cat("Pausing for 3 seconds to maintain rate limits...\n")
    Sys.sleep(3)
  }
}

cat("Audit completed successfully.\n")

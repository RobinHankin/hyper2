if (!requireNamespace("httr2", quietly = TRUE)) install.packages("httr2")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
library(httr2)

rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
output_file <- "man_pages_audit.txt"

# Get GitHub token from environment
token <- Sys.getenv("GITHUB_TOKEN")
if (token == "") stop("GITHUB_TOKEN environment variable is missing.")

# Assemble the bundle of all documentation files
bundled_content <- ""
for (f in rd_files) {
  rd_content <- paste(readLines(f, warn = FALSE), collapse = "\n")
  file_block <- paste0("\n---\nFILE_PATH: ", f, "\n```\n", rd_content, "\n```\n---\n")
  bundled_content <- paste0(bundled_content, file_block)
}

system_prompt <- "
Act as a critical, first-time reader of the hyper2 package.
You will receive a collection of separate documentation (.Rd) files demarcated by FILE_PATH headers.
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

# Call GitHub's Free Models API (using GPT-4o) with the entire bundle
req <- request("https://models.inference.ai.azure.com/chat/completions")
req <- req_headers(req, Authorization = paste("Bearer", token))
req <- req_body_json(req, list(
  model = "gpt-4o",
  messages = list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = bundled_content)
  )
))

cat("Sending bundled documentation to GitHub Models API...\n")
resp <- req_perform(req)
result <- resp_body_json(resp)$choices[[1]]$message$content

cat(result, file = output_file, append = TRUE)
cat("\n\nAudit completed successfully.\n", to = "")

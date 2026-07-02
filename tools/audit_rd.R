if (!requireNamespace("httr2", quietly = TRUE)) install.packages("httr2")
library(httr2)

rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
output_file <- "man_pages_audit.txt"

# Get GitHub token from environment
token <- Sys.getenv("GITHUB_TOKEN")
if (token == "") stop("GITHUB_TOKEN environment variable is missing.")

prompt_questions <- "
Act as a critical, first-time reader of the hyper2 package. 
Analyze this documentation file using the following checklist and provide highly concise, bullet-point corrections:
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

for (f in rd_files) {
  cat(paste("\nAuditing:", f, "\n-------------------\n"), file = output_file, append = TRUE)
  
  rd_content <- paste(readLines(f, warn = FALSE), collapse = "\n")
  
  # Call GitHub's Free Models API (using GPT-4o)
  req <- request("https://models.inference.ai.azure.com/chat/completions") |>
    req_headers(Authorization = paste("Bearer", token)) |>
    req_body_json(list(
      model = "gpt-4o",
      messages = list(
        list(role = "system", content = prompt_questions),
        list(role = "user", content = rd_content)
      )
    ))
  
  resp <- req_perform(req)
  result <- resp_body_json(resp)$choices[[1]]$message$content
  
  cat(result, file = output_file, append = TRUE)
  cat("\n\n", file = output_file, append = TRUE)
}

if(!requireNamespace("httr2", quietly = TRUE)){ install.packages("httr2") }
if(!requireNamespace("jsonlite", quietly = TRUE)){ install.packages("jsonlite") }
library("httr2")
library("jsonlite")

rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
output_file <- "man_pages_audit.txt"

token <- Sys.getenv("GITHUB_TOKEN")
if (token == "") stop("GITHUB_TOKEN environment variable is missing.")

prompt_path <- "inst/system_prompt.txt"

if (!file.exists(prompt_path)) {
  stop("System prompt file not found at: ", prompt_path)
}

system_prompt <- readChar(prompt_path, file.info(prompt_path)$size)
system_prompt <- gsub("\\\\n", "\n", system_prompt)



cat("hyper2 Documentation Audit Results\n=============\n", file = output_file)


batch_size <- 3
num_files <- length(rd_files)
num_batches <- ceiling(num_files / batch_size)

for (b in 1:num_batches) {
  start_idx <- (b - 1) * batch_size + 1
  end_idx <- min(b * batch_size, num_files)
  batch_files <- rd_files[start_idx:end_idx]
  
  cat(sprintf("Processing batch %d of %d (Files %d to %d)...\n", b, num_batches, start_idx, end_idx))
  
  bundled_content <- ""
  for (f in batch_files) {
    file_content <- paste(readLines(f, warn = FALSE), collapse = "\n")
    file_block <- paste0("\n---\nFILE_PATH: ", f, "\n", file_content, "\n---\n")
    bundled_content <- paste0(bundled_content, file_block)
  }
  
  req <- request("https://models.inference.ai.azure.com/chat/completions")
  req <- req_retry(req, max_tries = 3, backoff = ~ 10)
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
  
  if (b < num_batches) {
    Sys.sleep(15) 
  }
}

cat("Audit completed successfully.\n")

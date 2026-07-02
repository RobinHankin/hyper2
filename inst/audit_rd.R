if (!requireNamespace("httr2", quietly = TRUE)) install.packages("httr2")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
library(httr2)

rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
output_file <- "man_pages_audit.txt"

token <- Sys.getenv("GITHUB_TOKEN")
if (token == "") stop("GITHUB_TOKEN environment variable is missing.")

R
system_prompt <- "
Act as an expert, highly pedantic CRAN package reviewer auditing documentation (.Rd) files for the hyper2 package.

CONTEXT & AUDIENCE:
- An .Rd file is a formal technical reference manual page, NOT a tutorial, textbook, or introductory guide.
- The reader is a competent statistician or data scientist who already understands likelihood functions, Bradley-Terry, Dirichlet distributions, and log-linear models. 
- The goal of an .Rd page is to be a concise, precise, and accurate contract describing what a function accepts, what it computes, and what it returns.
- The audience is a competent R user
- The audience will be at least partly familiar with the package vignettes.
- The audience might appreciate a pointer to a specific vignette.

CRITICAL FILTER RULES:
- DO NOT complain about standard mathematical, statistical, or package-specific terminology being 'unexplained' or 'unmotivated'.  Assume the reader has the prerequisite background.
- DO NOT complain about standard R syntax, object classes, or conventions.
- DO NOT suggest generic or stylistic 'improvements' just to fill space. 
- IF A PAGE IS CLEAR, CONCISE, AND ACCURATE, OUTPUT ABSOLUTELY NOTHING FOR THAT FILE. Be silent by default.

ONLY report a file if it contains one or more of the following:
1. Glaring omissions
2. Contradictions between the text description and the usage syntax.
3. Broken or missing internal cross-references to other package functions.
4. Missing answers  to questions that my audience (defined above) would be likely to ask.

For any file that you wish to make an observation about, output:
FILE: [path]
- [Highly specific, actionable correction]
"

cat("hyper2 Documentation Audit Results\n=============\n", file = output_file)

# Split files into batches
batch_size <- 2
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
  req <- req_retry(req, max_tries = 3, backoff = ~ 15) # Auto-retry on 429
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
    cat("Pausing for 15 seconds to maintain rate limits...\n")
    Sys.sleep(15)
  }
}

cat("Audit completed successfully.\n")

suppressPackageStartupMessages({
  library("rmarkdown")
  library("digest")
})

this_file <- sys.frame(1)$ofile

if (is.null(this_file)) { stop() }

this_file <- normalizePath(this_file, winslash = "/")

inst_dir <- dirname(this_file)
root <- normalizePath(file.path(inst_dir, ".."), winslash = "/")

inst_root <- file.path(root, "inst")
site_dir <- file.path(root, "site")

if (!dir.exists(site_dir)) dir.create(site_dir, recursive = TRUE)

rmd_files <- list.files(
  inst_root,
  pattern = "\\.Rmd$",
  recursive = TRUE,
  full.names = TRUE
)

message("Found ", length(rmd_files), " Rmd files")

html_files <- character()

for (f in rmd_files) {

  rel <- sub(paste0("^", inst_root, "/?"), "", f)
  out_name <- sub("\\.Rmd$", ".html", rel)

  out_path <- file.path(site_dir, out_name)
  hash_path <- paste0(out_path, ".hash")

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  current_hash <- digest::digest(file = f, algo = "xxhash64")

  if (file.exists(out_path) && file.exists(hash_path)) {
    old_hash <- readLines(hash_path, warn = FALSE)

    if (identical(old_hash, current_hash)) {
      message("Skipping (cached): ", rel)
      html_files <- c(html_files, out_name)
      next
    }
  }

  message("Rendering: ", rel)

  rmarkdown::render(
    input = f,
    output_file = basename(out_path),
    output_dir = dirname(out_path),
    quiet = TRUE,
    envir = new.env()
  )

  writeLines(current_hash, hash_path)

  html_files <- c(html_files, out_name)
}

index_file <- file.path(site_dir, "index.html")

links <- paste0(
  "<li><a href='", html_files, "'>", html_files, "</a></li>",
  collapse = "\n"
)

index_html <- paste0(
  "<!DOCTYPE html>
<html>
<head>
<meta charset='utf-8'>
<title>Hyper2 catalogue</title>
</head>
<body>
<h1>Hyper2 Rmd catalogue</h1>
<p>Automatically generated from inst/*.Rmd files.</p>
<ul>
",
  links,
  "
</ul>
</body>
</html>"
)

writeLines(index_html, index_file)

message("Site built at: ", site_dir)

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

rmd_files <- rmd_files[-grep("f1points", rmd_files)]  # f1points.Rmd takes too long
rmd_files <- rmd_files[-grep("formula1", rmd_files)]  # formula1.Rmd takes too long
rmd_files <- rmd_files[-grep("skeleton", rmd_files)]  # skeleton.Rmd takes too long
rmd_files <- rmd_files[-grep("advantage", rmd_files)]  # home_advantage.Rmd takes too long
rmd_files <- rmd_files[-grep("race3", rmd_files)]  # race3.Rmd takes too long
rmd_files <- rmd_files[-grep("scissors", rmd_files)]  # rock_paper_scissors.Rmd takes too long
rmd_files <- rmd_files[-grep("monster_vs", rmd_files)]  # monster_vs_lambda.Rmd takes too long
rmd_files <- rmd_files[-grep("exponential", rmd_files)]  # exponential_BT.Rmd takes too long
rmd_files <- rmd_files[-grep("global", rmd_files)]  # global_liveability_ranking.Rmd takes too long
rmd_files <- rmd_files[-grep("notthewinner", rmd_files)]  # notthewinner.Rmd has some weird issue where it works fine locally but not on the workflow.
rmd_files <- rmd_files[-grep("simplified", rmd_files)]  # simplified_likelihood.Rmd has an unresolved theoretical issue, better to ignore this
rmd_files <- rmd_files[-grep("sushi", rmd_files)]  # sushi.Rd takes too long
rmd_files <- rmd_files[-grep("surfing", rmd_files)]  # surfing.Rd has some weird bug
rmd_files <- rmd_files[-grep("ternary", rmd_files)]  # ternaryplot.Rmd and ternaryplot_hyper2.Rmd take too long
rmd_files <- rmd_files[-grep("vsl", rmd_files)]  # the vsl sequence takes too long
rmd_files <- rmd_files[-grep("parkrun", rmd_files)]  # takes too long
rmd_files <- rmd_files[-grep("podium", rmd_files)]  # takes too long
rmd_files <- rmd_files[-grep("tennis", rmd_files)]  # some weird bug, runs locally
rmd_files <- rmd_files[-grep("volleyball", rmd_files)]  # some weird bug, runs locally
rmd_files <- rmd_files[-grep("volvo", rmd_files)]  # some weird bug, runs locally
rmd_files <- rmd_files[-grep("zermelo", rmd_files)]  # some weird bug, runs locally

message("Found ", length(rmd_files), " Rmd files")
for(f in rmd_files){message(f)}

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

deploy_site <- function(
  pkg = ".",
  ssh_id = Sys.getenv("id_rsa", ""),
  repo_slug = Sys.getenv("TRAVIS_REPO_SLUG", ""),
  commit_message = "",
  verbose = FALSE,
  ...
) {
  if (!nzchar(ssh_id)) {
    stop("No deploy key found, please setup with `travis::use_travis_deploy()`",
      call. = FALSE
    )
  }
  if (!nzchar(repo_slug)) {
    stop("No repo detected, please supply one with `repo_slug`",
      call. = FALSE
    )
  }
  cli::rule("Deploying site", line = 2)
  ssh_id_file <- "~/.ssh/id_rsa"
  cli::rule("Setting up SSH id", line = 1)
  cli::cat_line("Copying private key to: ", ssh_id_file)
  pkgdown:::write_lines(rawToChar(openssl::base64_decode(ssh_id)), ssh_id_file)
  cli::cat_line("Setting private key permissions to 0600")
  fs::file_chmod(ssh_id_file, "0600")

  dest_dir <- fs::dir_create(fs::file_temp())
  on.exit(fs::dir_delete(dest_dir))
  # pkg <- as_pkgdown(pkg)
  # if (is.null(repo_slug)) {
  #   gh <- rematch2::re_match(pkg$github_url, github_url_rx())
  #   repo_slug <- paste0(gh$owner, "/", gh$repo)
  # }
  pkgdown:::github_clone(dest_dir, repo_slug)
  rmarkdown::render_site()
  rmarkdown::render(input = "curriculum_vitae.Rmd", output_dir = "_site", output_format = "pagedown::html_resume")
  unlink(c("_site/README.html", "_site/DESCRIPTION", "_site/css", "_site/pictures"), recursive = TRUE)
  file.copy(from = list.files("_site", full.names = TRUE), to = dest_dir, overwrite = TRUE, recursive = TRUE)
  commit_message <- sprintf("Built site for %s: %s@%s", commit_message, Sys.Date(), substr(Sys.getenv("TRAVIS_COMMIT"), 1, 7))
  pkgdown:::github_push(dest_dir, commit_message)
  cli::rule("Deploy completed", line = 2)
}

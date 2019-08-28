deploy_site_github <- function(
  pkg = ".",
  tarball = Sys.getenv("PKG_TARBALL", ""),
  ssh_id = Sys.getenv("id_rsa", ""),
  repo_slug = Sys.getenv("TRAVIS_REPO_SLUG", ""),
  commit_message = construct_commit_message(pkg),
  verbose = FALSE,
  ...
) {
  if (!nzchar(tarball)) {
    stop("No built tarball detected, please provide the location of one with `tarball`",
      call. = FALSE
    )
  }
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
  rule("Deploying site", line = 2)
  rule("Installing package", line = 1)
  callr::rcmd("INSTALL", tarball, show = verbose, fail_on_status = TRUE)
  ssh_id_file <- "~/.ssh/id_rsa"
  rule("Setting up SSH id", line = 1)
  cat_line("Copying private key to: ", ssh_id_file)
  write_lines(rawToChar(openssl::base64_decode(ssh_id)), ssh_id_file)
  cat_line("Setting private key permissions to 0600")
  fs::file_chmod(ssh_id_file, "0600")

  dest_dir <- fs::dir_create(fs::file_temp())
  on.exit(fs::dir_delete(dest_dir))
  pkg <- as_pkgdown(pkg)
  if (is.null(repo_slug)) {
    gh <- rematch2::re_match(pkg$github_url, github_url_rx())
    repo_slug <- paste0(gh$owner, "/", gh$repo)
  }
  github_clone(dest_dir, repo_slug)
  # build_site(".", override = list(destination = dest_dir), document = FALSE, preview = FALSE, ...)
  rmarkdown::render(
    input = "curriculum_vitae/curriculum_vitae.Rmd", 
    output_dir = dest_dir, 
    output_file = "cv.html", 
    encoding = "UTF-8"
  )
  rmarkdown::render_site()
  github_push(dest_dir, commit_message)
  
  
  rule("Deploy completed", line = 2)
}

rlang::check_installed(
  "sfDR",
  reason = "rgisDR requires the sfDR package to work. Please install it with `install.packages('sfDR', repos = c('https://adatar-do.r-universe.dev', 'https://cloud.r-project.org'))`.",
  action = \(pkg, ...) install.packages(pkg, repos = c('https://adatar-do.r-universe.dev', 'https://cloud.r-project.org'))
)

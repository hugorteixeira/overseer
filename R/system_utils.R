.scrot_install_command <- function() {
  os_name <- Sys.info()[["sysname"]]
  if (!identical(os_name, "Linux")) {
    return(NULL)
  }

  sudo_prefix <- if (nzchar(Sys.which("sudo"))) "sudo " else ""

  if (nzchar(Sys.which("apt-get"))) {
    return(paste0(
      sudo_prefix, "apt-get update && ",
      sudo_prefix, "apt-get install -y scrot"
    ))
  }
  if (nzchar(Sys.which("dnf"))) {
    return(paste0(sudo_prefix, "dnf install -y scrot"))
  }
  if (nzchar(Sys.which("yum"))) {
    return(paste0(sudo_prefix, "yum install -y scrot"))
  }
  if (nzchar(Sys.which("pacman"))) {
    return(paste0(sudo_prefix, "pacman -S --noconfirm scrot"))
  }
  if (nzchar(Sys.which("zypper"))) {
    return(paste0(sudo_prefix, "zypper --non-interactive install scrot"))
  }
  if (nzchar(Sys.which("apk"))) {
    return(paste0(sudo_prefix, "apk add scrot"))
  }

  NULL
}

.print_scrot_instructions <- function() {
  os_name <- Sys.info()[["sysname"]]
  if (!identical(os_name, "Linux")) {
    message("`scrot` is a Linux tool and is not available on this OS.")
    message("Use a Linux environment or switch to a supported screenshot tool.")
    return(invisible(NULL))
  }

  message("`scrot` is required. Install it using one of the commands below:")
  message("  Debian/Ubuntu: sudo apt-get update && sudo apt-get install -y scrot")
  message("  Fedora: sudo dnf install -y scrot")
  message("  RHEL/CentOS: sudo yum install -y scrot")
  message("  Arch: sudo pacman -S --noconfirm scrot")
  message("  openSUSE: sudo zypper --non-interactive install scrot")
  message("  Alpine: sudo apk add scrot")
}

.ensure_scrot <- function(action = c("install", "instructions", "error"), verbose = TRUE) {
  action <- match.arg(action)

  scrot_path <- Sys.which("scrot")
  if (nzchar(scrot_path)) {
    return(scrot_path)
  }

  if (!interactive() && action == "install") {
    .print_scrot_instructions()
    return(NULL)
  }

  if (action == "error") {
    stop("`scrot` is required but was not found in PATH.")
  }

  if (action == "instructions") {
    .print_scrot_instructions()
    return(NULL)
  }

  install_cmd <- .scrot_install_command()
  if (is.null(install_cmd)) {
    .print_scrot_instructions()
    return(NULL)
  }

  if (verbose) {
    message("Attempting to install `scrot` using: ", install_cmd)
  }

  status <- system(install_cmd)
  scrot_path <- Sys.which("scrot")
  if (identical(status, 0L) && nzchar(scrot_path)) {
    if (verbose) {
      message("`scrot` installed successfully.")
    }
    return(scrot_path)
  }

  if (verbose) {
    message("Automatic installation did not succeed.")
  }
  .print_scrot_instructions()
  NULL
}

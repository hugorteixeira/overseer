\name{overseer}
\alias{overseer}
\title{Capture screen changes and log model responses}
\usage{
overseer(
  instructions,
  output_dir = "~/.overseer",
  min_interval = 5,
  max_interval = 300,
  change_threshold = 0.05,
  image_quality = 85,
  max_width = 1920,
  save_screenshots = FALSE,
  service = "openai",
  model = "gpt-5-mini",
  temp = 1,
  reasoning = NULL,
  genflow_label = NULL,
  verbose = TRUE,
  debug = FALSE,
  debug_every = 1,
  debug_save = FALSE,
  genflow_args = list()
)
}
\arguments{
  \item{instructions}{Character or object. Main prompt passed to \code{genflow::gen_txt}.}
  \item{output_dir}{Character. Directory where screenshots and logs are saved. Defaults to \code{"~/.overseer"} when NULL or empty.}
  \item{min_interval}{Numeric. Minimum delay between checks, in seconds.}
  \item{max_interval}{Numeric. Maximum delay between checks, in seconds.}
  \item{change_threshold}{Numeric between 0 and 1. Fraction of changed pixels required to trigger.}
  \item{image_quality}{Numeric between 1 and 100. Compression quality for saved images.}
  \item{max_width}{Numeric. Maximum width for saved images; larger images are resized.}
  \item{save_screenshots}{Logical. When TRUE, save processed screenshots under \code{output_dir/data/YYYY/MM}.}
  \item{service}{Character. Provider name for \code{genflow::gen_txt}.}
  \item{model}{Character. Model identifier for \code{genflow::gen_txt}.}
  \item{temp}{Numeric. Sampling temperature for \code{genflow::gen_txt}.}
  \item{reasoning}{Character or NULL. Reasoning mode for \code{genflow::gen_txt}.}
  \item{genflow_label}{Character or NULL. Optional label for \code{genflow::gen_txt} outputs.}
  \item{verbose}{Logical. Emit progress messages when TRUE.}
  \item{debug}{Logical. When TRUE, prints change metrics every \code{debug_every} iterations.}
  \item{debug_every}{Numeric. Frequency for debug output (in iterations).}
  \item{debug_save}{Logical. When TRUE, copies the current screenshot to \code{output_dir/debug} during debug output.}
  \item{genflow_args}{List. Additional arguments passed to \code{genflow::gen_txt}.}
}
\value{
This function runs indefinitely and is called for its side effects.
}
\description{
Captures screenshots with \code{scrot}, detects visual changes, saves a masked
image of the change, sends the image to \code{genflow::gen_txt}, and writes each
response to \code{output_dir/data/YYYY/data_YYYYMMDD_HHMMSS.txt} and also
rebuilds \code{output_dir/data/YYYY/data_YYYY.txt} in chronological order with
the oldest messages at the top. When \code{save_screenshots} is TRUE, processed screenshots
are saved under \code{output_dir/data/YYYY/MM}.
}
\details{
This function requires the \code{scrot} binary to be available in PATH and the
\code{magick} and \code{genflow} packages to be installed. The \code{output_dir}
must be writable.
}
\examples{
\dontrun{
instructions <- paste(
  "Context: You are seeing what the user was doing at the time the screenshot was taken.",
  "Objective: Describe what is being done by the user.",
  "",
  "Examples:",
  "",
  "Emails, appears to be something personal",
  "Reddit, content about artificial intelligence",
  "WhatsApp, chatting with 'Jonh Doe'",
  "LLM, discussing an R function",
  sep = "\n"
)

instructions <- read_text_file("overseer_prompt.txt")
overseer(
  instructions = instructions,
  min_interval = 5,
  max_interval = 30,
  change_threshold = 0.05,
  service = "openai",
  model = "gpt-4o-mini"
)
}
}

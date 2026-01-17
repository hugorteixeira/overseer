\name{read_text_file}
\alias{read_text_file}
\title{Read a text file into a single string}
\usage{
read_text_file(path)
}
\arguments{
  \item{path}{Character path to a text file.}
}
\value{
A single character string with newlines preserved.
}
\description{
Reads a text file and returns its contents as one string, preserving line breaks.
}
\examples{
tmp <- tempfile(fileext = ".txt")
writeLines(c("Line one", "Line two"), tmp)
read_text_file(tmp)
}

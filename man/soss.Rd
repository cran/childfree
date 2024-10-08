% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soss.R
\name{soss}
\alias{soss}
\title{Read and recode Michigan State of the State (SOSS) data}
\usage{
soss(waves, extra.vars = NULL, survey = FALSE, progress = TRUE)
}
\arguments{
\item{waves}{vector: a numeric vector containing the SOSS waves to include (currently available: 79, 82, 84, 85, 86)}

\item{extra.vars}{vector: a character vector containing the names of variables to be retained from the raw data}

\item{survey}{boolean: returns an unweighted data.frame if \code{FALSE}, or a weighted \link[survey]{svydesign} object if \code{TRUE}}

\item{progress}{boolean: display a progress bar}
}
\value{
A data frame or weighted \link[survey]{svydesign} object containing variables described in the codebook available using \code{vignette("codebooks")}.
If you are offline, or if the requested data are otherwise unavailable, NULL is returned.
}
\description{
Read and recode Michigan State of the State (SOSS) data
}
\details{
The \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss}{State of the State Survey} (SOSS) is
regularly collected by the Institute for Public Policy and Social Research (IPPSR) at Michigan State
University (MSU). Each wave is collected from a sample of 1000 adults in the US state of Michigan, and
includes sampling weights to obtain a sample that is representative of the state's population with respect
to age, gender, race, and education. The \code{soss()} function reads the raw data from IPPSR's website, extracts
and recodes selected variables useful for studying childfree adults and other family statuses, then returns
either an unweighted data frame, or a weighted design object that can be analyzed using the \code{survey}
package. Questions necessary for identifying childfree adults have been asked in five waves, which each
include unique questions that may be of interest:
\itemize{
\item \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-79b-spring-2020}{Wave 79} (May 2020) - Neighborhoods, Health care, COVID, Personality
\item \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-82-fall-2021}{Wave 82} (September 2021) - Trust in government, Critical Race Theory
\item \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-84-spring-2022}{Wave 84} (April 2022) - Trust in scientists, Autonomous vehicles, Morality
\item \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-85-fall-2022}{Wave 85} (September 2022) - Reproductive rights, Race equity
\item \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-86-winter-2022}{Wave 86} (December 2022) - Education, Infrastructure
}

\strong{Notes}
\itemize{
\item Wave 79 did not include a "do not know" option for selected questions. Therefore, it is not possible to identify
"undecided" or "ambivalent non-parent" respondents. This may lead other family status categories to be inflated.
\item Wave 82 originally included a 500 person oversample of parents, but they are excluded from \code{nsfg(wave==82)}.
\item The provided sampling weights are designed to be used in the analyses of individual waves. Combining data from multiple
waves may require using adjusted weights.
}
}
\examples{
\donttest{
unweighted <- soss(waves = 86)  #Request unweighted data
if (!is.null(unweighted)) {  #If data was available...
table(unweighted$famstat) / nrow(unweighted)  #Fraction of respondents with each family status
}

weighted <- soss(waves = 86, survey = TRUE)  #Request weighted data
if (!is.null(weighted)) {  #If data was available...
survey::svymean(~famstat, weighted, na.rm = TRUE)  #Estimated prevalence of each family status
}
}
}

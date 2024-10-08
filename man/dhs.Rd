% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dhs.R
\name{dhs}
\alias{dhs}
\title{Read and recode Demographic and Health Surveys (DHS) individual data}
\usage{
dhs(files, extra.vars = NULL, survey = FALSE, progress = TRUE)
}
\arguments{
\item{files}{vector: a character vector containing the paths for one or more Individual Recode DHS data files (see details)}

\item{extra.vars}{vector: a character vector containing the names of variables to be retained from the raw data}

\item{survey}{boolean: returns an unweighted data.frame if \code{FALSE}, or a weighted \link[survey]{svydesign} object if \code{TRUE}}

\item{progress}{boolean: display a progress bar}
}
\value{
A data frame or weighted \link[survey]{svydesign} object containing variables described in the codebook available using \code{vignette("codebooks")}
If you are offline, or if the requested data are otherwise unavailable, NULL is returned.
}
\description{
Read and recode Demographic and Health Surveys (DHS) individual data
}
\details{
The \href{https://www.dhsprogram.com/}{Demographic and Health Surveys} (DHS) program regularly collects
health data from population-representative samples in many countries using standardized surveys since 1984. The
"individual recode" data files contain women's responses, while the "men recode" files contain men's responses. These
files are available in SPSS, SAS, and Stata formats from \href{https://www.dhsprogram.com/}{https://www.dhsprogram.com/},
however access requires a \href{https://dhsprogram.com/data/Access-Instructions.cfm}{free application}. The \code{dhs()} function
reads one or more of these files, extracts and recodes selected variables useful for studying childfree adults and other
family statuses, then returns either an unweighted data frame, or a weighted \link[survey]{svydesign} object that can be analyzed using the
\code{survey} package.

Although access to DHS data requires an application, the DHS program provides a \href{https://dhsprogram.com/data/Download-Model-Datasets.cfm}{model dataset}
for practice. The example provided below uses the model data file "ZZIR62FL.SAV", which contains fictitious women's data,
but has the same structure as a real DHS data file. The example can be run without prior application for data access.

\strong{Known issues}
\itemize{
\item The SPSS-formatted files containing data from Gabon Recode 4 (GAIR41FL.SAV, GAMR41FL.SAV) and Turkey Recode 4 (TRIR41FL.SAV, TRMR41FL.SAV)
contain encoding errors. Use the SAS-formatted files (GAIR41FL.SAS7BDAT, GAMR41FL.SAS7BDAT, TRIR41FL.SAS7BDAT, TRMR41FL.SAS7BDAT) instead.
\item In some cases, DHS makes available individual recode data files for specific regions. For example, women's data from individual states
in India from 1999 are contained in files named XXIR42FL.SAV, where the "XX" is a two-letter state code. The \code{dhs()} function has only
been tested using whole-country files, and may not perform as expected for regional files.
\item Variables containing women's responses in the individual recode files begin with \code{v}, while variables containing men's responses in the
men recode files begin with \code{mv}. When applying \code{dhs()} to both female and male data, these are automatically harmonized. However, if
extra variables are requested using the \code{extra.vars} option, be sure to specify both names (e.g. \code{extra.vars = c("v201", "mv201")}).
\item If \code{survey = TRUE}, then \code{(m)v021} and \code{(m)v023} are used as the cluster and strata indicators, respectively. This is
appropriate for most surveys, however there are a few exceptions. Additional information about analyzing DHS data using weights is
available \href{https://dhsprogram.com/data/Guide-to-DHS-Statistics/Analyzing_DHS_Data.htm}{here} and in the documentation provided
with the downloaded data files.
}
}
\examples{
\donttest{
unweighted <- dhs(files = c("ZZIR62FL.SAV"), extra.vars = c("v201"))  #Request unweighted data
if (!is.null(unweighted)) {  #If data was available...
round(table(unweighted$famstat)/nrow(unweighted),3)  #Fraction of respondents w/ each family status
}

weighted <- dhs(files = c("ZZIR62FL.SAV"), survey = TRUE)  #Request weighted (example) data
if (!is.null(weighted)) {  #If dtaa was available...
survey::svymean(~famstat, weighted, na.rm = TRUE)  #Estimated prevalence of each family status
}
}
}

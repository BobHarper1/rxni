\name{import_rx}
\alias{import_rx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download and return a combined dataframe for specified months
}
\description{

}
\usage{
import_rx(month_range = 1:1)
}
\arguments{
  \item{month_range}{
The range of months to download and combine, with \code{1} being the most recent available month
}
}
\details{
The \code{import_rx} function has been written to make it easier to import monthly Northern Ireland GP practice-level prescribing data from OpenDataNI.gov.uk and create combined datasets of the .csv files. This requires internet access to work.

The data covers prescriptions that are prescribed by a GP or Nurse practitioner (within a GP practice) subsequently dispensed (e.g. by a Community Pharmacist) and submitted to HSCNI Business Services Organisation (BSO) for payment. Items prescribed but not redeemed by patients at pharamacies are therefore not included. Private prescriptions, hospice items and post-payment adjustments are also excluded.

For each GP practice in Northern Ireland, and for each prescribed medicine (by presentation), dressing and appliance, information on the number of prescribed items, quantity of individual tablets, capsules, liquids etc. dispensed (in units, millilitres or grammes), and the gross and actual cost are included.

Practices are referred to by their \code{practice_id}. Practice references are available from... @TODO

Individual presentations are referred to by the substance or product's name (\code{VTM_NM}), by the generic name (\code{VMP_NM}) and by their branded name (\code{AMP_NM}). In addition, British National Formulary (BNF) codes, and BNF chapter, section, paragraph and subparagraph reference numbers feature.

The default behaviour is to download the most recent single month (i.e. \code{\link{rx_combine_months(1:1)}}).
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
prescriptions <- import_rx(1:12) ## Download the last 12 months of prescribing data and return to object

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

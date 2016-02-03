\name{Package 'wcagepreds'}
\alias{wcagepreds-package}
\alias{wcagepreds}
\docType{package}
\title{
Estimating Age and other Characteristics of the Williams College Faculty
}
\description{
The wcagepreds package provides pathways to explore characteristics of the Williams College faculty between the academic years 2004-2005 and 2013-2014.
}
\details{
\tabular{ll}{
Package: \tab HHFindAge\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-02-03\cr
License: \tab GPL-2 \cr
}
}

\usage{
findage(academicyear, 
        type = "average", 
        department = "None", 
        gender = "None",
        yearterminal="None",
        TimebwBAandTerminalbyDept="None")
        
Users must input into findage 'academicyear' and only one other argument at a time.

Inputs into academicyear must be of the form of the last two numbers of the two consecutive academic years entered consecutively. Example: 0405 for academic year 2004-2005 and 1011 for academic year 2010-2011.

The only inputs into type may be "average", "min", and "max".



Department inputs must be of the abbreviations denoted by the Williams College Course Catalog. These abbreviations are:
AFR, AMST, ANTH, ARAB, ART, ARTH, ASST, ASTR, BIO, CHEM, CHIN, CLAS, COMP, CSCI, DANC, ECON, ENGL, ENVI, GEOS, GERM, HIST, HSCI, INST, JAPN, LATS, LEAD, MAST, MATH, MUS, NSCI, PE, PHIL, PHYS, PSCI, PSYC, REL, RL, RLFR, RLSP, RUSS, SOC, STAT, THEA, WGSS

Gender inputs must be denoted as "M" for male or "F " for female.

yearterminal must be entered as a full year, i.e. "1967"

TimebwBAandTerminalbyDept must be entered by department via the abbreviations above.
}
\arguments{

academicyear

A character vector determined by the user input of what academic year between 2004-2005 and 2013-2014 the user would like to learn more about.

type

A character vector which indicts what type of age the user would like to discover for a given academic year. Type can be set to its default "average", "min" for minimum age, or "max" for maximum age.

department

A character vector indicating for a given department what the average age is.

gender

A character vector indicating for a given gender what the average age is.

yearterminal

A character vector indicating for a given year of terminal degree what the average age is.

TimebwBAandTerminalbyDept

A character vector indicating for a given department what the average amount of years were in between the date of Bachelor's Degree and Terminal Degree. 
}


\author{
Matthew Morris <mrm10@williams.edu>

Maintainer: Matthew Morris <mrm10@williams.edu>
}

\references{
"Williams College Bulletin: Course Catalog, September 2013". <http://web.williams.edu/admin/registrar//catalog/bulletin2013_14.pdf>

"Williams College Bulletin: 2012-2013".
<http://web.williams.edu/admin/registrar//catalog/bulletin12_13.pdf>

"Williams College Bulletin: 2011-2012".
<http://web.williams.edu/admin/registrar//catalog/bulletin2011_12.pdf>

"Williams College Bulletin: 2010-2011".
<http://web.williams.edu/admin/registrar//catalog/bulletin2010-11.pdf>

"Williams College Bulletin: 2009-2010".
<http://web.williams.edu/admin/registrar//catalog/catalog0910.pdf>

"Williams College Bulletin: 2008-2009".
<http://web.williams.edu/admin/registrar//catalog/catalog0809.pdf>

"Williams College Bulletin: Course Catalog, September 2007". <http://web.williams.edu/admin/registrar//catalog/catalog0708.pdf>

"Williams College Bulletin: Course Catalog, September 2006". <http://web.williams.edu/admin/registrar//catalog/catalog0607.pdf>

"Williams College Catalog 2005-2006".
<http://web.williams.edu/admin/registrar//catalog/catalog0506.pdf>

"Williams College Bulletin: 2004-2005"
<http://web.williams.edu/admin/registrar//catalog/depts0405/catalog0405.pdf>
}


\examples{

ex = findage(1314,type="min")
ex
[1] 24

ex2 = findage(0607,department="JAPN")
ex2
[1] 42

ex3 = facultynumber(1213,facultynumber=TRUE)
ex3
[1] 401

ex4 = stddevage(1112,stddevage=TRUE)
ex4
[1] 11.60477

ex5 = sumstats(0405,sumstats=FALSE)
ex5
[1] 45 
}

\seealso{

facultynumber(academicyear,
              facultynumber=TRUE)

facultynumber allows the user to input an academic year in the same manner as findage and if facultynumber is set to "TRUE" the function tells the user what the number of faculty were at Williams for the given academic year. If facultynumber is set to "FALSE" the function tells the user the average age of the Williams College faculty for that given academic year.

stddevage(academicyear,
          stddevage=TRUE)

stddevage allows the user to input an academic year in the same manner as findage and if stddevage is set to "TRUE" the function tells the user what the standard deviation of the faculty ages were at Williams for the given academic year. If stddevage is set to "FALSE" the function tells the user the average age of the Williams College faculty for that given academic year.

sumstats(academicyear, sumstats=TRUE)

sumstats allows the user to input an academic year in the same manner as findage and if sumstats is set to "TRUE" the function tells the user what the summary statistics, i.e. minimum, first quartile, median, mean, third quartile, maximum, of the faculty ages were at Williams for the given academic year. If sumstats is set to "FALSE" the function tells the user the average age of the Williams College faculty for that given academic year.





}
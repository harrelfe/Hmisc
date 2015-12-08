require(Hmisc)
ht <- function(x) {
  base <- gsub('\\.tex', '', x$file)
  invisible(html(w, method='hevea', file=paste(base, 'html', sep='.')))
}

n <- 500; set.seed(88)
sex <- factor(sample(c("female","male"), n, TRUE))
age <- rnorm(n, 50, 10)
height <- rnorm(n, 1.7, 0.5)
type <- factor(sample(c('A', 'B'), n, TRUE))
dbase= data.frame(sex, age, height, type)

w <- latex(summaryM(age + height + type ~ sex , data=dbase, overall=TRUE,
                    test=TRUE),
      long=TRUE, prmsd = TRUE, npct='slash', middle.bold=TRUE,
      caption="Descriptive Statistics",
      msdsize='scriptsize', round=2, digits=2, prtest='P', pdig=2,
      file='/tmp/a.tex',
      label="table:summary")
ht(w)
## If this were in a knitr document you could have the following after the @
## that ends the chunk to also include the LaTeX typeset table (omit the ## )
## \input{/tmp/a}

# From Lauren Samuels
set.seed(1)
d <- expand.grid(x1=c('A', 'B'), x2=c('a', 'b', 'c'))
d$y <- runif(nrow(d))
d
w <- latex(
  summaryM(x2 + y ~ x1, data= d, test=TRUE, overall=TRUE, continuous=6 ),
  file="/tmp/b.tex",
  caption="Descriptive stats and tests of between-group differences for all primary and secondary neuroimaging outcomes", 
  label= "tbl:descrOutcomes",
  middle.bold=TRUE, msdsize='scriptsize',
  exclude1=FALSE,   digits=2, long=TRUE, prmsd=TRUE, 
  npct="slash")
ht(w)

## Example taken from help file for summaryM
options(digits=3)
set.seed(173)
sex <- factor(sample(c("m","f"), 500, rep=TRUE))
country <- factor(sample(c('US', 'Canada'), 500, rep=TRUE))
age <- rnorm(500, 50, 5)
sbp <- rnorm(500, 120, 12)
label(sbp) <- 'Systolic BP'
units(sbp) <- 'mmHg'
treatment <- factor(sample(c("Drug","Placebo"), 500, rep=TRUE))
treatment[1]
sbp[1] <- NA

# Generate a 3-choice variable; each of 3 variables has 5 possible levels
symp <- c('Headache','Stomach Ache','Hangnail',
          'Muscle Ache','Depressed')
symptom1 <- sample(symp, 500,TRUE)
symptom2 <- sample(symp, 500,TRUE)
symptom3 <- sample(symp, 500,TRUE)
Symptoms <- mChoice(symptom1, symptom2, symptom3, label='Primary Symptoms')
table(as.character(Symptoms))
# Produce separate tables by country
f <- summaryM(age + sex + sbp + Symptoms ~ treatment + country,
              groups='treatment', test=TRUE)
w <- latex(f, file='/tmp/c.tex', msdsize='scriptsize',
           npct='slash', middle.bold=TRUE, prmsd=TRUE)
ht(w)


getHdata(pbc)
s5 <- summaryM(bili + albumin + stage + protime + sex +
                age + spiders ~ drug, data=pbc)
w <- latex(s5, npct='both', insert.bottom = "More stuff to add \\ldots",
           middle.bold=TRUE,
           file='/tmp/d.tex')
ht(w)

pkgname <- "imcr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "imcr-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('imcr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("bin")
### * bin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bin
### Title: Compute binomial coefficient
### Aliases: bin

### ** Examples

n=5
k=2
bin(n,k)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bin", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cl.profil")
### * cl.profil

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cl.profil
### Title: Columns percentages of a frequency table from variables and
###   clusters
### Aliases: cl.profil

### ** Examples

var = c(rep("yes",7), rep("no",7))
clust = c(1,1,2,1,2,3,1,2,3,3,2,1,3,2)
cl.profil(var,clust)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cl.profil", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("corr_coef")
### * corr_coef

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: corr_coef
### Title: Compute correlation coefficient of quantitative variables and
###   clusters
### Aliases: corr_coef

### ** Examples

data(iris)
iris.cr <- scale(iris[,-5],center=TRUE,scale=TRUE)
d.iris <- dist(iris.cr)
cah.ward <- hclust(d.iris,method="ward.D2")
groupes.cah <- cutree(cah.ward,k=4)
corr_coef(iris[,-5], groupes.cah, show_graph = TRUE, digits=3)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("corr_coef", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("effect_size")
### * effect_size

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: effect_size
### Title: Compute effect size of quantitative variables and clusters
### Aliases: effect_size

### ** Examples

data(iris)
iris.cr <- scale(iris[,-5],center=TRUE,scale=TRUE)
d.iris <- dist(iris.cr)
cah.ward <- hclust(d.iris,method="ward.D2")
groupes.cah <- cutree(cah.ward,k=4)
effect_size(iris[,-5], groupes.cah, digits=3)
#when prompted for variable enter :Sepal.length
#when prompted for variable enter :2



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("effect_size", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("evaluation")
### * evaluation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: evaluation
### Title: Compute Rand index and V measure between real and predicted
###   clusters
### Aliases: evaluation

### ** Examples

true_label <-as.factor(c("1","2","2","1"))
pred_label <-as.factor(c("1","2","1","2"))
evaluation(true_label,pred_label)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("evaluation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("h.value.test")
### * h.value.test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: h.value.test
### Title: Effect size for proportion comparison
### Aliases: h.value.test

### ** Examples

var = c(rep("yes",7), rep("no",7))
clust = c(1,1,2,1,2,3,1,2,3,3,2,1,3,2)
h.value.test(var,clust)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("h.value.test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("l.profil")
### * l.profil

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: l.profil
### Title: Row percentages of a frequency table from variables and clusters
### Aliases: l.profil

### ** Examples

var = c(rep("yes",7), rep("no",7))
clust = c(1,1,2,1,2,3,1,2,3,3,2,1,3,2)
l.profil(var,clust)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("l.profil", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("phi.value.test")
### * phi.value.test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: phi.value.test
### Title: Effect size expressed by correlation
### Aliases: phi.value.test

### ** Examples

var = c(rep("yes",7), rep("no",7))
clust = c(1,1,2,1,2,3,1,2,3,3,2,1,3,2)
phi.value.test(var,clust)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("phi.value.test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rand")
### * rand

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rand
### Title: Compute Rand index between real and predicted clusters
### Aliases: rand

### ** Examples

true_label <-as.factor(c("1","2","2","1"))
pred_label <-as.factor(c("1","2","1","2"))
rand(true_label,pred_label)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rand", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_value")
### * test_value

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_value
### Title: Compute test value of quantitative variables and clusters
### Aliases: test_value

### ** Examples

data(iris)
iris.cr <- scale(iris[,-5],center=TRUE,scale=TRUE)
d.iris <- dist(iris.cr)
cah.ward <- hclust(d.iris,method="ward.D2")
groupes.cah <- cutree(cah.ward,k=4)
test_value(iris[,-5], groupes.cah, show_graph = TRUE, digits=3)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_value", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("uni.quali")
### * uni.quali

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: uni.quali
### Title: Constructor for qualitative univariate characterization
### Aliases: uni.quali

### ** Examples

var = c(rep("yes",7), rep("no",7))
clust = c(1,1,2,1,2,3,1,2,3,3,2,1,3,2)
uni.quali(var,clust)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("uni.quali", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("v.cramer")
### * v.cramer

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: v.cramer
### Title: Compute Cramer's v of qualitatives variables and clusters
### Aliases: v.cramer

### ** Examples

var = c('yes','no','yes')
clust = c(1,1,2)
v.cramer(var,clust)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("v.cramer", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("v.measure")
### * v.measure

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: v.measure
### Title: Compute V measure between real and predicted clusters
### Aliases: v.measure

### ** Examples

true_label <-as.factor(c("1","2","2","1"))
pred_label <-as.factor(c("1","2","1","2"))
v.measure(true_label,pred_label)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("v.measure", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

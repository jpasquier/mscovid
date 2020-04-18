library(mscovid)
library(flexsurv)

# ---------------------------------- Data ----------------------------------- #

# Data are not provided in the package

# Data
path <- "data-raw/20.04.14 - Données REDCap hôpitaux anonymisés.xlsx"
data <- import_data(path, censoring_date = "2020-04-14")
rm(path)

# --------------------------- Parametric analysis --------------------------- #

# Prepare data
msdata <- prep_data(data, format = "mstate")
tmat <- attr(msdata, "tmat")

# Parametric fit (joint)
fit <- flexsurvreg(Surv(Tstart, Tstop, status) ~ trans,
                   data = msdata, dist = "weibull")

# Parametric fit (transition-specific)
n_trans <- max(tmat, na.rm = TRUE)
fits <- lapply(1:n_trans, function(i) {
  flexsurvreg(Surv(time, status) ~ sex + age,
              data = subset(msdata, trans == i),
              dist = "weibull")
})

# ------------------------------- Simulations ------------------------------- #

pars <- as.data.frame(readxl::read_xlsx("data-raw/params.xlsx"))
pars$date <- conv_date(pars$date)

pred1 <- pred_state_occupancy(
  fit = fits,
  trans = tmat,
  nday = 60,
  nsim = 1000,
  pars = pars,
  data = data,
  seed = 666,
  ncpu = 2
)

# ------------------------------ Plot results ------------------------------- #

pdf("tmp/fig_mscovid.pdf")
ttl <- "multi-state model"
sttl <- "semi-Markov model fitted with a Weibull distribution"
pi <- FALSE
for (s in c("all", "hos", "icu", "rest", "death", "recovery")) {
  print(plot_occupancy(pred = pred1, state = s, pi = pi,
                       ttl = ttl, sttl = sttl))
  pi <- TRUE
}
dev.off()

# --------------------------------------------------------------------------- #

# final_prop
final_prop <- prop.table(sapply(pred1, function(u) median(u[, ncol(u)])))

# ----------------------------------- DEV ----------------------------------- #


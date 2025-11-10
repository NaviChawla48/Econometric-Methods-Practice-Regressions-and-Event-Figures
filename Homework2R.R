#packages
req <- c(
  "tidyverse","haven","labelled","fixest","sandwich","lmtest",
  "marginaleffects","modelsummary","broom","janitor","rnaturalearth",
  "sf","geosphere"
)
install.packages(c("terra","sf","s2","units","rnaturalearth","rnaturalearthdata"))
miss <- setdiff(req, rownames(installed.packages()))
if (length(miss)) install.packages(miss, quiet = TRUE)
invisible(lapply(req, library, character.only = TRUE))

#data path
DAT <- function(...) file.path("C:/Empirical Political Economy", ...)

#1a)
pwt <- read_dta(DAT("pwt100.dta")) %>%
  janitor::clean_names()

d <- pwt %>% mutate(
  y_pw = rgdpo/emp,
  mexico = as.integer(country == "Mexico"),
  france = as.integer(country == "France")
)

#1b)
mex_1980_mean <- d %>% filter(country == "Mexico", year == 1980) %>% summarise(m = mean(y_pw, na.rm = TRUE))
print(mex_1980_mean)

#1c)
d80 <- d %>% filter(year == 1980)
M1c <- feols(y_pw ~ mexico, data = d80)
print(summary(M1c))

#1d)
d80 <- d %>% filter(year == 1980)
M1d <- feols(y_pw ~ 0 + mexico, data = d80)
print(summary(M1d))

#1e)
M1e <- feols(y_pw ~ mexico + france, data = d80)
print(summary(M1e))

#1f)
base <- d %>% filter(year >= 1980, year <= 2019) %>%
  group_by(country) %>% mutate(nyrs = n()) %>% ungroup() %>%
  filter(nyrs == 40) %>%
  mutate(post84 = as.integer(year >= 1985))
M1f <- feols(y_pw ~ post84, data = base, cluster = ~ country)
print(summary(M1f))

#1g)
M1g_a <- feols(y_pw ~ post84 | country, data = base, cluster = ~ country)
M1g_b <- feols(y_pw ~ post84 | country + year, data = base, cluster = ~ country)
print(summary(M1g_a))
print(summary(M1g_b))

#1h)
base2 <- base %>% mutate(
  mexico = as.integer(country == "Mexico"),
  france = as.integer(country == "France")
)
M1h <- feols(y_pw ~ post84*mexico + post84*france | country + year, data = base2, cluster = ~ country)
print(summary(M1h))

#2a)
china <- d %>% mutate(
  ln_y = log(y_pw),
  china = as.integer(country == "China")
) %>% filter(year >= 1990, year <= 2019)

did2a <- feols(ln_y ~ i(year >= 2002, china) | country + year, data = china, cluster = ~ country)
print(summary(did2a))

#2c)
china_es <- china %>% mutate(rel_event = year - 2001) %>%
  filter(rel_event >= -10, rel_event <= 18)
ES <- feols(
  ln_y ~ i(rel_event, china, ref = -1) | country + year,
  data = china_es, cluster = ~ country
)
print(summary(ES))

coefs <- broom::tidy(ES, conf.int = TRUE) %>%
  filter(str_detect(term, "^rel_event::")) %>%
  mutate(rel = as.integer(str_extract(term, "-?\\d+")))

ggplot(coefs, aes(x = rel, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  labs(x = "Event time (years from 2001)", y = "Log output: China vs others") +
  theme_minimal()

#2d)
pretrend_test <- summary(ES)$coeftable %>%
  as.data.frame() %>%
  filter(grepl("rel_event::-[0-9]+", rownames(.)))
print(pretrend_test)

#2e)
china_long <- d %>%
  mutate(ln_y = log(y_pw),
         china = as.integer(country == "China"),
         rel_event = year - 2001) %>%
  filter(year >= 1950, year <= 2019)

ES_long <- feols(
  ln_y ~ i(rel_event, china, ref = -1) | country + year,
  data = china_long,
  cluster = ~ country
)
print(summary(ES_long))

coefs_long <- broom::tidy(ES_long, conf.int = TRUE) %>%
  filter(str_detect(term, "^rel_event::")) %>%
  mutate(rel = as.integer(str_extract(term, "-?\\d+")))

ggplot(coefs_long, aes(x = rel, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  labs(x = "Years from 2001", y = "Log output difference (China vs others)",
       title = "Event study 1950–2019 (China WTO timing)") +
  theme_minimal()

#3a)
nq <- read_dta("nq_update.dta") %>%
  arrange(country, year) %>%
  mutate(post = as.numeric(year >= 1500))

reg_3a <- feols(
  ln_population ~ post:ln_wpot | country + year + year[ln_oworld, ln_elevation, ln_tropical, ln_rugged],
  data = nq,
  cluster = ~country
)
summary(reg_3a)

#3b)
sd_wpot <- sd(nq$ln_wpot, na.rm = TRUE)
pop_coef <- coef(reg_3a)["post:ln_wpot"]

cat("POP per-SD (log points):", pop_coef * sd_wpot, "\n")
cat("POP per-SD (percent):", 100 * (exp(pop_coef * sd_wpot) - 1), "\n")

#Urbanization regression
reg_3b_urb <- feols(
  city_pop_share ~ post:ln_wpot | country + year + year[ln_oworld, ln_elevation, ln_tropical, ln_rugged],
  data = nq,
  cluster = ~country
)
summary(reg_3b_urb)

urb_coef <- coef(reg_3b_urb)["post:ln_wpot"]
cat("URB per-SD (units):", urb_coef * sd_wpot, "\n")

#3c)
reg_3c_pop <- feols(
  ln_population ~ post:ln_wpot + post:cont_europe + cont_europe | 
    country + year + year[ln_oworld, ln_elevation, ln_tropical, ln_rugged],
  data = nq,
  cluster = ~country
)
summary(reg_3c_pop)

reg_3c_urb <- feols(
  city_pop_share ~ post:ln_wpot + post:cont_europe + cont_europe | 
    country + year + year[ln_oworld, ln_elevation, ln_tropical, ln_rugged],
  data = nq,
  cluster = ~country
)
summary(reg_3c_urb)

#3d)
nq_event <- nq %>%
  mutate(
    rel = year - 1500,
    rel_index = rel - min(rel, na.rm = TRUE) + 1
  )

base_idx_3d <- nq_event %>% 
  filter(rel == 0) %>% 
  pull(rel_index) %>% 
  unique()

reg_3d <- feols(
  ln_population ~ i(rel_index, ln_wpot, ref = base_idx_3d) | 
    country + year + year[ln_oworld, ln_elevation, ln_tropical, ln_rugged],
  data = nq_event,
  cluster = ~country
)

coef_df_3d <- tidy(reg_3d, conf.int = TRUE) %>%
  filter(str_detect(term, "rel_index.*ln_wpot")) %>%
  mutate(
    rel = as.numeric(str_extract(term, "(?<=rel_index::)[0-9]+")) - base_idx_3d
  ) %>%
  select(rel, estimate, conf.low, conf.high)

coef_df_3d <- bind_rows(
  coef_df_3d,
  data.frame(rel = 0, estimate = 0, conf.low = 0, conf.high = 0)
) %>%
  arrange(rel)

ggplot(coef_df_3d, aes(x = rel, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Q3(d) Event study: ln_wpot × year (base = 1500)",
    x = "Years relative to 1500 (0 = 1500)",
    y = "Effect of ln_wpot on log population"
  ) +
  theme_minimal()

# 3e)
nq <- read_dta("nq_update.dta") %>%
  arrange(isocode, year) %>%
  mutate(post = as.numeric(year >= 1500))

# Check what the country identifier variable is called
names(nq)  # This will show you the variable names

milk <- read_dta("milk_lpf.dta")
nq <- nq %>%
  left_join(milk, by = "isocode") %>%
  mutate(lpf = ifelse(lpf > 1 & lpf <= 100, lpf / 100, lpf))

#3f)
reg_3f_pop <- feols(
  ln_population ~ post:ln_wpot:lpf + post:ln_wpot + post:lpf | 
    isocode + year + year[ln_oworld, ln_elevation, ln_tropical, ln_rugged],
  data = nq,
  cluster = ~isocode
)
summary(reg_3f_pop)

#lpf=0
cat("\nEffect when LPF = 0:\n")
lincom_0 <- coef(reg_3f_pop)["post:ln_wpot"]
cat("Coefficient:", lincom_0, "\n")

#When lpf = 1
cat("\nEffect when LPF = 1:\n")
lincom_1 <- coef(reg_3f_pop)["post:ln_wpot"] + coef(reg_3f_pop)["post:ln_wpot:lpf"]
cat("Coefficient:", lincom_1, "\n")

#Urbanization
reg_3f_urb <- feols(
  city_pop_share ~ post:ln_wpot:lpf + post:ln_wpot + post:lpf | 
    isocode + year + year[ln_oworld, ln_elevation, ln_tropical, ln_rugged],
  data = nq,
  cluster = ~isocode
)
summary(reg_3f_urb)

cat("\nUrbanization - Effect when LPF = 0:\n")
urb_lincom_0 <- coef(reg_3f_urb)["post:ln_wpot"]
cat("Coefficient:", urb_lincom_0, "\n")

cat("\nUrbanization - Effect when LPF = 1:\n")
urb_lincom_1 <- coef(reg_3f_urb)["post:ln_wpot"] + coef(reg_3f_urb)["post:ln_wpot:lpf"]
cat("Coefficient:", urb_lincom_1, "\n")

#3g)
reg_3g_pop <- feols(
  ln_population ~ post:ln_wpot:lpf + post:ln_wpot + post:lpf + post:cont_europe | 
    isocode + year + year[ln_oworld, ln_elevation, ln_tropical, ln_rugged],
  data = nq,
  cluster = ~isocode
)
summary(reg_3g_pop)

cat("\n3g Population - Effect when LPF = 0:\n")
g_lincom_0 <- coef(reg_3g_pop)["post:ln_wpot"]
cat("Coefficient:", g_lincom_0, "\n")

cat("\n3g Population - Effect when LPF = 1:\n")
g_lincom_1 <- coef(reg_3g_pop)["post:ln_wpot"] + coef(reg_3g_pop)["post:ln_wpot:lpf"]
cat("Coefficient:", g_lincom_1, "\n")

reg_3g_urb <- feols(
  city_pop_share ~ post:ln_wpot:lpf + post:ln_wpot + post:lpf + post:cont_europe | 
    isocode + year + year[ln_oworld, ln_elevation, ln_tropical, ln_rugged],
  data = nq,
  cluster = ~isocode
)
summary(reg_3g_urb)

cat("\n3g Urbanization - Effect when LPF = 0:\n")
g_urb_lincom_0 <- coef(reg_3g_urb)["post:ln_wpot"]
cat("Coefficient:", g_urb_lincom_0, "\n")

cat("\n3g Urbanization - Effect when LPF = 1:\n")
g_urb_lincom_1 <- coef(reg_3g_urb)["post:ln_wpot"] + coef(reg_3g_urb)["post:ln_wpot:lpf"]
cat("Coefficient:", g_urb_lincom_1, "\n")

#3h)
nq_trip <- nq %>%
  mutate(
    rel = year - 1500,
    rel_index = rel - min(rel, na.rm = TRUE) + 1,
    wpot_lpf = ln_wpot * lpf  # Create the product variable
  )

base_idx_3h <- nq_trip %>% 
  filter(rel == 0) %>% 
  pull(rel_index) %>% 
  unique()

reg_3h <- feols(
  ln_population ~ i(rel_index, wpot_lpf, ref = base_idx_3h) | 
    isocode + year + year[ln_oworld, ln_elevation, ln_tropical, ln_rugged],
  data = nq_trip,
  cluster = ~isocode
)

coef_df_3h <- tidy(reg_3h, conf.int = TRUE) %>%
  filter(str_detect(term, "rel_index")) %>%
  mutate(
    rel = as.numeric(str_extract(term, "(?<=rel_index::)[0-9]+")) - base_idx_3h
  ) %>%
  select(rel, estimate, conf.low, conf.high)

coef_df_3h <- bind_rows(
  coef_df_3h,
  data.frame(rel = 0, estimate = 0, conf.low = 0, conf.high = 0)
) %>%
  arrange(rel)

ggplot(coef_df_3h, aes(x = rel, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Q3(h) Event study: ln_wpot × LPF dynamics (base = 1500)",
    x = "Years relative to 1500 (0 = 1500)",
    y = "Effect of ln_wpot × LPF on log population"
  ) +
  theme_minimal()

#4a)
nq4 <- read_dta("nq_update.dta") %>%
  filter(between(year, 1000, 1900)) %>%
  mutate(post1600 = as.numeric(year >= 1600))

cat("Variables in nq_update.dta:\n")
print(names(nq4))

palos_lat <- 37.23
palos_lon <- -6.89

nq4 <- nq4 %>%
  mutate(
    lat_rad = lat * pi / 180,      
    lon_rad = lon * pi / 180,      
    pal_lat = palos_lat * pi / 180,
    pal_lon = palos_lon * pi / 180,
    dlat = lat_rad - pal_lat,
    dlon = lon_rad - pal_lon,
    a = sin(dlat/2)^2 + cos(lat_rad) * cos(pal_lat) * sin(dlon/2)^2,
    dist_palos_km = 2 * 6371 * asin(sqrt(a))
  )

#4b)
results <- data.frame()
best_r2 <- -1
best_speed <- NA

for (s in seq(5, 80, by = 5)) {
  nq4_temp <- nq4 %>%
    mutate(arrive_s = as.numeric(year >= (1600 + dist_palos_km / s)))
  
  reg_temp <- feols(
    ln_population ~ arrive_s | 
      country + year + year[ln_oworld, ln_elevation, ln_tropical, ln_rugged],
    data = nq4_temp,
    cluster = ~country
  )
  
  r2_adj <- r2(reg_temp, type = "ar2")
  results <- bind_rows(results, data.frame(speed = s, adj_r2 = r2_adj))
  
  if (r2_adj > best_r2) {
    best_r2 <- r2_adj
    best_speed <- s
  }
}

cat("Best diffusion speed (km/year):", best_speed, "| adj R^2 =", round(best_r2, 3), "\n")

nq4 <- nq4 %>%
  mutate(arrive = as.numeric(year >= (1600 + dist_palos_km / best_speed)))

#4c)
reg_4c_pop <- feols(
  ln_population ~ arrive:ln_wpot + arrive + ln_wpot | 
    country + year + year[ln_oworld, ln_elevation, ln_tropical, ln_rugged],
  data = nq4,
  cluster = ~country
)
summary(reg_4c_pop)

reg_4c_urb <- feols(
  city_pop_share ~ arrive:ln_wpot + arrive + ln_wpot | 
    country + year + year[ln_oworld, ln_elevation, ln_tropical, ln_rugged],
  data = nq4,
  cluster = ~country
)
summary(reg_4c_urb)

#4d)
mean_wpot <- mean(nq4$ln_wpot, na.rm = TRUE)
arrive_coef <- coef(reg_4c_urb)["arrive"]
interact_coef <- coef(reg_4c_urb)["arrive:ln_wpot"]
effect_at_mean <- arrive_coef + mean_wpot * interact_coef

cat("\nEffect at mean ln_wpot (", round(mean_wpot, 2), "):", 
    round(effect_at_mean, 6), "\n")

cat("\n=== Analysis Complete ===\n")
cat("All regressions have been run.\n")
cat("Event study plots have been generated.\n")
cat("Remember to adjust the working directory path at 
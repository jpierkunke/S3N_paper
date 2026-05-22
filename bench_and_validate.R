# Combine and summarize benchmarking results
# 2026 May 16

# S3N: stores just params and runtimes
# runtimes has the following five elements:
# "Build LSN"
# "Stream updist and AFV"
# "Add obs to LSN"
# "Obs-obs distances"
# "Estimation"

# SSN: stores just params and runtimes
# runtimes has the following six elements:
# "Build LSN"
# "Stream updist and AFV"
# "Add obs to LSN"
# "Assemble SSN"
# "Obs-obs distances"
# "Estimation"

library(sf) # for reading and possibly writing shapefiles
library(knitr) # for making tables
library(igraph) # for making the adjacency matrix
library(tictoc) # for benchmarking (timing to evaluate efficiency/runtime)
library(Matrix) # for efficient sparse adjacency matrix operations
library(tidyverse) # for efficient data manipulation
library(shapefiles) # for read.dbf
library(magrittr) # for the eager pipe
library(latex2exp) # for LaTeX formatting in plot labels
library(fishualize) # for fish-inspired color palettes
library(ggpattern) # for validation plots
library(lemon) # for repositioning legend

bench_res_dir = "bench_results/"

s3nfiles = list.files(path = bench_res_dir, pattern = "^S3N", full.names = TRUE)
ssnfiles = list.files(path = bench_res_dir, pattern = "^SSN", full.names = TRUE)

# read in all the S3N results and extract just the runtimes for benchmarking
s3nrun = s3nfiles |>
  # include filenames as the names of the files vector so we can extract metadata from them later
  set_names(basename(s3nfiles)) |>
  # load each file, make a dataframe out of the runtimes and filename, and stack them together
  imap_dfr(~ {
    load(.x)
    runtimes |> matrix(nrow=1) |> data.frame() |> mutate(fn = .y)
  }) |>
  # rename the five runtimes more informatively
  rename(
    build_lsn = X1,
    stream_updist = X2,
    add_obs = X3,
    obs_dist = X4,
    estimation = X5
  ) |>
  # extract metadata from the filenames
  mutate(
    network = map_int(fn, \(one_row) str_split_1(one_row, "[_.]")[3] |> parse_number()),
    nobs = map_int(fn, \(one_row) str_split_1(one_row, "[_.]")[4] |> parse_number()),
    rep = map_int(fn, \(one_row) str_split_1(one_row, "[_.]")[5] |> parse_number())
  )

# drop the fn column after verifying
s3nrun = select(s3nrun, -fn)
# add columns for total, model, and assemble ssn
s3nrun$total = apply(select(s3nrun, build_lsn:estimation), 1, sum)
s3nrun$model = "S3N"; s3nrun$assemble_ssn = NA

# check what combinations of network and nobs we have
s3nrun |> distinct(network, nobs) |> arrange(network, nobs)
s3nrun |> dplyr::count(network, nobs) |> arrange(network, nobs)


# read in all the SSN results and extract just the runtimes for benchmarking
ssnrun = ssnfiles |>
  # include filenames as the names of the files vector so we can extract metadata from them later
  set_names(basename(ssnfiles)) |>
  # load each file, make a dataframe out of the runtimes and filename, and stack them together
  imap_dfr(~ {
    load(.x)
    runtimes |> matrix(nrow=1) |> data.frame() |> mutate(fn = .y)
  }) |>
  # rename the six runtimes more informatively
  rename(
    build_lsn = X1,
    stream_updist = X2,
    add_obs = X3,
    assemble_ssn = X4,
    obs_dist = X5,
    estimation = X6
  ) |>
  # extract metadata from the filenames
  mutate(
    network = map_int(fn, \(one_row) str_split_1(one_row, "[_.]")[3] |> parse_number()),
    nobs = map_int(fn, \(one_row) str_split_1(one_row, "[_.]")[4] |> parse_number()),
    rep = map_int(fn, \(one_row) str_split_1(one_row, "[_.]")[5] |> parse_number())
  )

# drop the fn column after verifying
ssnrun = select(ssnrun, -fn)
# add columns for total and model
ssnrun$total = apply(select(ssnrun, build_lsn:estimation), 1, sum)
ssnrun$model = "SSN"

runtime_summary = rbind(s3nrun, ssnrun) |>
  select(-rep) |>
  relocate(model, .before = 1) |>
  relocate(network, .before = 2) |>
  relocate(nobs, .before = 3) |>
  relocate(assemble_ssn, .after = add_obs) |>
  mutate(
    # these numbers can be verified by loading an initial data file network*_nobs*_initial_data.rda for each network
    nreach = case_when(
      network == 1 ~ 284,
      network == 2 ~ 1273,
      network == 3 ~ 7146,
      network == 4 ~ 11540,
      network == 5 ~ 30748,
      network == 6 ~ 169060
    ),
    .after = network
  ) |>
  pivot_longer(
    cols = build_lsn:total,
    names_to = "task",
    values_to = "time"
  ) |>
  group_by(model, network, nreach, nobs, task) |>
  summarize(avg_time = mean(time), nreps = n()) |>
  ungroup()

runtime_summary_wide = runtime_summary |>
  # pivot wider to have all the times in a single row
  pivot_wider(names_from = task, values_from = avg_time) |>
  # also rearrange task columns to the order in which the tasks are performed:
  # "Build LSN"
  # "Stream updist and AFV"
  # "Add obs to LSN"
  # "Assemble SSN"
  # "Obs-obs distances"
  # "Estimation"
  relocate(nreps, .after = nobs) |>
  relocate(build_lsn, .after = nreps) |>
  relocate(stream_updist, .after = build_lsn) |>
  relocate(add_obs, .after = stream_updist) |>
  relocate(assemble_ssn, .after = add_obs) |>
  relocate(obs_dist, .after = assemble_ssn)

print(runtime_summary_wide, n=40)
#    # A tibble: 38 × 12
#    model network nreach  nobs nreps build_lsn stream_updist add_obs assemble_ssn obs_dist estimation    total
#    <chr>   <int>  <dbl> <int> <int>     <dbl>         <dbl>   <dbl>        <dbl>    <dbl>      <dbl>    <dbl>
#  1 S3N         1    284   200    50     0.228         0.975    3.64       NA        1.01     0.0547      5.91
#  2 S3N         2   1273   200    50     0.399         1.45     5.13       NA        1.73     0.0692      8.77
#  3 S3N         2   1273  1000    50     0.383         1.49     2.82       NA        5.35     0.296      10.3 
#  4 S3N         3   7146   200    50     1.55          4.91     3.47       NA        0.682    0.0548     10.7 
#  5 S3N         3   7146  1000    50     1.46          4.55     2.99       NA        6.81     0.290      16.1 
#  6 S3N         4  11540   100    50     2.26          8.15     3.27       NA        0.782    0.0271     14.5 
#  7 S3N         4  11540   200    50     3.16         11.0      4.36       NA        1.11     0.0670     19.7 
#  8 S3N         4  11540   500    50     2.37          8.64     3.64       NA        2.50     0.167      17.3 
#  9 S3N         4  11540  1000    50     2.22          8.10     3.35       NA        6.88     0.327      20.9 
# 10 S3N         4  11540  5000    50     2.23          7.81     4.47       NA      176.       1.37      192.  
# 11 S3N         4  11540 10000    50     2.39          8.44     4.06       NA      852.       3.47      870.  
# 12 S3N         5  30748   200    50     6.54         19.7      4.92       NA        0.897    0.0723     32.1 
# 13 S3N         5  30748  1000    50     5.84         17.0      4.49       NA        8.52     0.332      36.2 
# 14 S3N         6 169060   100    50    33.7          43.7     12.0        NA        1.37     0.00372    90.7 
# 15 S3N         6 169060   200    50    31.4          40.1     11.4        NA        1.51     0.0259     84.5 
# 16 S3N         6 169060   500    50    32.5          41.7     11.5        NA        3.77     0.122      89.6 
# 17 S3N         6 169060  1000    50    33.2          42.9     11.8        NA       11.9      0.330     100.  
# 18 S3N         6 169060  5000    50    32.7          42.4     12.7        NA      266.       1.94      356.  
# 19 S3N         6 169060 10000    50    36.6          42.9     12.4        NA     1180.       3.43     1275.  
# 20 SSN         1    284   200    50    24.1           0.748    6.82        0.714    6.32     0.508      39.2 
# 21 SSN         2   1273   200    50   101.            4.43    10.1         3.35    27.8      0.866     147.  
# 22 SSN         2   1273  1000    50     9.51          2.98    30.2         1.48     9.33     2.54       56.0 
# 23 SSN         3   7146   200    50    30.8          66.5      5.50        9.93     3.57     0.297     117.  
# 24 SSN         3   7146  1000    50    22.5          63.5     27.3         9.17     9.93     3.21      136.  
# 25 SSN         4  11540   100    50    47.8         160.       2.85       20.4      2.77     0.196     234.  
# 26 SSN         4  11540   200    50    54.6         178.       6.29       22.8      5.23     0.362     268.  
# 27 SSN         4  11540   500    50    54.4         166.      14.9        24.9      8.27     1.39      270.  
# 28 SSN         4  11540  1000    50    40.0         161.      26.0        18.7     12.5      3.46      261.  
# 29 SSN         4  11540  5000    50    67.2         161.     159.         21.0    211.      71.2       690.  
# 30 SSN         4  11540 10000    50    54.4         163.     301.         20.1    712.     105.       1356.  
# 31 SSN         5  30748   200    50   181.         1171.       5.69      116.       4.85     0.301    1479.  
# 32 SSN         5  30748  1000    50   179.         1127.      26.3       115.      16.6      2.26     1466.  
# 33 SSN         6 169060   100    50  5006.        34565.       3.63     3370.       8.07     0.200   42953.  
# 34 SSN         6 169060   200    50  3369.        33879.       6.35     3194.       9.92     0.234   40458.  
# 35 SSN         6 169060   500    50  5054.        34668.      16.2      3691.      13.8      0.599   43444.  
# 36 SSN         6 169060  1000    50  5123.        34003.      30.2      3464.      30.7      1.41    42652.  
# 37 SSN         6 169060  5000    50  3597.        34039.     158.       3491.     270.      47.4     41604.  
# 38 SSN         6 169060 10000    50  4115.        34952.     277.       3580.     919.      86.5     43929. 

# compute ratio ----------

runtime_summary_wide |>
  select(-assemble_ssn) |>
  pivot_wider(
    names_from = model,
    names_sep = "_",
    values_from = build_lsn:total
  ) |>
  mutate(
    build_lsn_ratio = build_lsn_SSN/build_lsn_S3N,
    stream_updist_ratio = stream_updist_SSN/stream_updist_S3N,
    add_obs_ratio = add_obs_SSN/add_obs_S3N,
    obs_dist_ratio = obs_dist_SSN/obs_dist_S3N,
    estimation_ratio = estimation_SSN/estimation_S3N,
    total_ratio = total_SSN/total_S3N
  ) |>
  select(
    network, nreach, nobs, nreps, build_lsn_ratio, stream_updist_ratio,
    add_obs_ratio, obs_dist_ratio, estimation_ratio, total_ratio
  )
# # A tibble: 19 × 10
#    network nreach  nobs nreps build_lsn_ratio stream_updist_ratio add_obs_ratio obs_dist_ratio estimation_ratio total_ratio
#      <int>  <dbl> <int> <int>           <dbl>               <dbl>         <dbl>          <dbl>            <dbl>       <dbl>
#  1       1    284   200    50           106.                0.767         1.87           6.24              9.29        6.63
#  2       2   1273   200    50           253.                3.06          1.96          16.1              12.5        16.8 
#  3       2   1273  1000    50            24.9               2.00         10.7            1.74              8.60        5.42
#  4       3   7146   200    50            19.8              13.5           1.58           5.24              5.42       10.9 
#  5       3   7146  1000    50            15.4              14.0           9.12           1.46             11.1         8.42
#  6       4  11540   100    50            21.1              19.6           0.873          3.54              7.24       16.1 
#  7       4  11540   200    50            17.3              16.2           1.44           4.70              5.40       13.6 
#  8       4  11540   500    50            23.0              19.2           4.10           3.30              8.32       15.6 
#  9       4  11540  1000    50            18.0              19.8           7.76           1.81             10.6        12.5 
# 10       4  11540  5000    50            30.1              20.6          35.7            1.20             51.9         3.60
# 11       4  11540 10000    50            22.8              19.3          74.3            0.836            30.2         1.56
# 12       5  30748   200    50            27.7              59.6           1.16           5.40              4.17       46.1 
# 13       5  30748  1000    50            30.7              66.2           5.86           1.95              6.79       40.5 
# 14       6 169060   100    50           149.              792.            0.303          5.88             53.6       474.  
# 15       6 169060   200    50           107.              844.            0.555          6.55              9.03      479.  
# 16       6 169060   500    50           156.              832.            1.41           3.67              4.93      485.  
# 17       6 169060  1000    50           154.              793.            2.56           2.59              4.26      426.  
# 18       6 169060  5000    50           110.              803.           12.5            1.02             24.4       117.  
# 19       6 169060 10000    50           113.              814.           22.3            0.779            25.2        34.5 

# making benchmarking figures ---------

runtime_summary = runtime_summary |>
  mutate(
    task = task |>
      recode(
        "add_obs" = "Add obs to LSN",
        "assemble_ssn" = "Assemble SSN",
        "build_lsn" = "Build LSN",
        "estimation" = "Estimation",
        "obs_dist" = "Obs-obs distances",
        "stream_updist" = "Stream updist and AFV",
        "total" = "Total"
      ) |>
      factor(
        levels = c(
          "Build LSN",
          "Stream updist and AFV",
          "Add obs to LSN",
          "Assemble SSN",
          "Obs-obs distances",
          "Estimation",
          "Total"
        )
      )
  )

plot_colors = fish(12, option = "Centropyge_loricula")[c(5,10)] # S3N, SSN

# for varying nreach, fixed nobs = 200
ggplot(runtime_summary |>
         filter(nobs == 200),
       aes(x = log(nreach, base = 10), 
           y = log(avg_time, base = 10), 
           color = model,
           group = model,
           shape = model)) +
  geom_point() +
  geom_line() +
  ylim(c(-3, 5)) +
  xlim(c(3, 5.25)) +
  scale_shape_manual(values = c("S3N" = 16, "SSN" = 1)) + # S3N filled circles, SSN open circles
  scale_colour_manual(values = plot_colors) +
  facet_wrap(~ task, scales = "free", nrow=2) +
  theme_minimal() +
  labs(
    x = TeX("log$_{10}$(Number of reaches)"),
    y = TeX("log$_{10}$(Runtime in seconds)"),
    color = "Model",
    shape = "Model"
  ) +
  theme(legend.position = c(0.92, 0.15),
        legend.justification = c(1, 0))
# theme(legend.position = "inside",
#       legend.position.inside = c(0.59, 0.21),
#       text = element_text(size = 11))

ggsave(
  paste0(bench_res_dir, "fixednobs200_varynreach_univscale.png"),
  width = 8, height = 4, units = "in"
)

# for varying nreach, fixed nobs = 1000
ggplot(runtime_summary |>
         filter(nobs == 1000),
       aes(x = log(nreach, base = 10), 
           y = log(avg_time, base = 10), 
           color = model,
           group = model,
           shape = model)) +
  geom_point() +
  geom_line() +
  ylim(c(-3, 5)) +
  xlim(c(3, 5.25)) +
  scale_shape_manual(values = c("S3N" = 16, "SSN" = 1)) + # S3N filled circles, SSN open circles
  scale_colour_manual(values = plot_colors) +
  facet_wrap(~ task, scales = "free", nrow=2) +
  theme_minimal() +
  labs(
    x = TeX("log$_{10}$(Number of reaches)"),
    y = TeX("log$_{10}$(Runtime in seconds)"),
    color = "Model",
    shape = "Model"
  ) +
  theme(legend.position = c(0.92, 0.15),
        legend.justification = c(1, 0))

ggsave(
  paste0(bench_res_dir, "fixednobs1000_varynreach_bynreach_univscale.png"),
  width = 8, height = 4, units = "in"
)

# varying nobs, fixed nreach = 11540 (NW4)
ggplot(runtime_summary |>
         filter(network == 4),
       aes(x = log(nobs, base = 10), 
           y = log(avg_time, base = 10), 
           color = model,
           group = model,
           shape = model)) +
  geom_point() +
  geom_line() +
  ylim(c(-3, 5)) +
  xlim(c(2, 4)) +
  scale_shape_manual(values = c("S3N" = 16, "SSN" = 1)) + # S3N filled circles, SSN open circles
  scale_colour_manual(values = plot_colors) +
  facet_wrap(~ task, scales = "free", nrow=2) +
  theme_minimal() +
  labs(
    x = TeX("log$_{10}$(Number of observation points)"),
    y = TeX("log$_{10}$(Runtime in seconds)"),
    color = "Model",
    shape = "Model"
  ) +
  theme(legend.position = c(0.92, 0.15),
        legend.justification = c(1, 0))

ggsave(
  paste0(bench_res_dir, "fixednreachNW4_varynobs_univscale.png"),
  width = 8, height = 4, units = "in"
)

# varying nobs, fixed nreach (NW6)
ggplot(runtime_summary |>
         filter(network == 6),
       aes(x = log(nobs, base = 10), 
           y = log(avg_time, base = 10), 
           color = model,
           group = model,
           shape = model)) +
  geom_point() +
  geom_line() +
  ylim(c(-3, 5)) +
  xlim(c(2, 4)) +
  scale_shape_manual(values = c("S3N" = 16, "SSN" = 1)) + # S3N filled circles, SSN open circles
  scale_colour_manual(values = plot_colors) +
  facet_wrap(~ task, scales = "free", nrow=2) +
  theme_minimal() +
  labs(
    x = TeX("log$_{10}$(Number of observation points)"),
    y = TeX("log$_{10}$(Runtime in seconds)"),
    color = "Model",
    shape = "Model"
  ) +
  theme(legend.position = c(0.92, 0.15),
        legend.justification = c(1, 0))

ggsave(
  paste0(bench_res_dir, "fixednreachNW6_varynobs_univscale.png"),
  width = 8, height = 4, units = "in"
)


## Combine and summarize validation results -------------------------------

# read in all the S3N results and extract just the runtimes for benchmarking
s3nparams = s3nfiles |>
  # include filenames as the names of the files vector so we can extract metadata from them later
  set_names(basename(s3nfiles)) |>
  # load each file, make a dataframe out of the runtimes and filename, and stack them together
  imap_dfr(~ {
    load(.x)
    params |> matrix(nrow=1) |> data.frame() |> mutate(fn = .y)
  }) |>
  # rename the five parameters more informatively
  rename(
    beta_1 = X1,
    beta_2 = X2,
    sigma_sq = X3,
    tau_sq = X4,
    lambda = X5
  ) |>
  # extract metadata from the filenames
  mutate(
    network = map_int(fn, \(one_row) str_split_1(one_row, "[_.]")[3] |> parse_number()),
    nobs = map_int(fn, \(one_row) str_split_1(one_row, "[_.]")[4] |> parse_number()),
    rep = map_int(fn, \(one_row) str_split_1(one_row, "[_.]")[5] |> parse_number())
  )

# drop the fn column after verifying
s3nparams = select(s3nparams, -fn)
s3nparams$model = "S3N"

# read in all the SSN results and extract just the runtimes for benchmarking
ssnparams = ssnfiles |>
  # include filenames as the names of the files vector so we can extract metadata from them later
  set_names(basename(ssnfiles)) |>
  # load each file, make a dataframe out of the runtimes and filename, and stack them together
  imap_dfr(~ {
    load(.x)
    params |> matrix(nrow=1) |> data.frame() |> mutate(fn = .y)
  }) |>
  # rename the five parameters more informatively
  rename(
    beta_1 = X1,
    beta_2 = X2,
    sigma_sq = X3,
    tau_sq = X4,
    lambda = X5
  ) |>
  # extract metadata from the filenames
  mutate(
    network = map_int(fn, \(one_row) str_split_1(one_row, "[_.]")[3] |> parse_number()),
    nobs = map_int(fn, \(one_row) str_split_1(one_row, "[_.]")[4] |> parse_number()),
    rep = map_int(fn, \(one_row) str_split_1(one_row, "[_.]")[5] |> parse_number())
  )

# drop the fn column after verifying
ssnparams = select(ssnparams, -fn)
ssnparams$model = "SSN"

param_combined = rbind(s3nparams, ssnparams) |>
  select(-rep) |>
  relocate(model, .before = 1) |>
  relocate(network, .before = 2) |>
  relocate(nobs, .before = 3) |>
  pivot_longer(
    cols = beta_1:lambda,
    names_to = "parameter",
    values_to = "value"
  )

param_summary = param_combined |>
  group_by(model, network, nobs, parameter) |>
  summarize(
    avg = mean(value),
    sd = sd(value)
  ) |>
  ungroup() |>
  mutate(
    truth = case_when(
      parameter == "beta_1" ~ -44,
      parameter == "beta_2" ~ 0.5,
      parameter == "sigma_sq" ~ 5,
      parameter == "tau_sq" ~ 0.1,
      parameter == "lambda" ~ 5
    ),
    
    parameter_plot = case_when(
      parameter == "beta_1" ~ "$\\beta_0$",
      parameter == "beta_2" ~ "$\\beta_1$",
      parameter == "sigma_sq" ~ "$\\sigma^2$",
      parameter == "tau_sq" ~ "$\\tau^2$",
      parameter == "lambda" ~ "$\\lambda$"
    ) |>
      factor(
        levels = c(
          "$\\beta_0$",
          "$\\beta_1$",
          "$\\sigma^2$",
          "$\\tau^2$",
          "$\\lambda$"
        )
      ),
    
    bias = avg - truth
  ) |>
  pivot_wider(
    names_from = model,
    values_from = c(avg, bias, sd)
  )

param_combined = left_join(param_combined, param_summary)

make_validation_plot = function(nw, no, param_combined){
  latex_labels <- c(
    "$\\beta_0$"   = TeX("$\\beta_0$"),
    "$\\beta_1$"   = TeX("$\\beta_1$"),
    "$\\sigma^2$"  = TeX("$\\sigma^2$"),
    "$\\tau^2$"    = TeX("$\\tau^2$"),
    "$\\lambda$"   = TeX("$\\lambda$")
  )
  
  hist_plot = ggplot(
    dplyr::filter(param_combined, network == nw, nobs == no), 
    aes(x = value, fill = model)
  ) +
    facet_wrap(
      ~ parameter_plot, 
      scales = "free", 
      # ncol = 1,
      labeller = as_labeller(function(x) latex_labels[x], default = label_parsed)
    ) +
    geom_histogram_pattern(
      aes(pattern = model), 
      alpha = 0.6,
      pattern_density = 0.05
    ) +
    geom_vline(
      aes(xintercept = truth, 
          color = "Truth", linetype = "Truth", linewidth = "Truth"),
      data = distinct(dplyr::filter(param_combined, network == nw, nobs == no), parameter_plot, truth)
    ) +
    geom_vline(
      aes(xintercept = avg_S3N,
          color = "S3N", linetype = "S3N", linewidth = "S3N"),
      data = distinct(dplyr::filter(param_combined, network == nw, nobs == no), parameter_plot, avg_S3N)
    ) +
    geom_vline(
      aes(xintercept = avg_SSN,
          color = "SSN", linetype = "SSN", linewidth = "SSN"),
      data = distinct(dplyr::filter(param_combined, network == nw, nobs == no), parameter_plot, avg_SSN)
    ) +
    labs(
      title = paste0("Network ", nw, ", ", no, " observation points"),
      y = "Count",
      x = "Parameter value"
    ) +
    theme_minimal(base_size = 8) +
    scale_fill_manual(values = plot_colors) +
    scale_pattern_manual(values = c("S3N" = "none", "SSN" = "circle")) +
    scale_linetype_manual(values = c("Truth" = "solid", 
                                     "S3N" = "dashed", 
                                     "SSN" = "dotted"), name = "Legend") +
    scale_linewidth_manual(values = c("Truth" = 0.5, 
                                      "S3N" = 0.4, 
                                      "SSN" = 0.4), name = "Legend") +
    scale_color_manual(values = c("Truth" = 'black', 
                                  "S3N" = plot_colors[1], 
                                  "SSN" = plot_colors[2]), name = "Legend") +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 6),
      legend.direction = "horizontal")
  
  # # for a 2x3 plot
  hist_plot = reposition_legend(hist_plot, "center", panel = "panel-3-2")
  
  ggsave(hist_plot, 
         filename = paste0(
           bench_res_dir, "validation_network", nw, "_nobs", no, ".png"
         ),
         width = 6, height = 3, units = "in")
  
}

combos = s3nrun |> distinct(network, nobs) |> arrange(network, nobs)

for(i in seq_along(combos$network)){
  make_validation_plot(combos[i, 1], combos[i, 2], param_combined)
}

param_summary |> 
  group_by(parameter) |> 
  summarize(
    biasS3Nless = sum(abs(bias_S3N) < abs(bias_SSN)), 
    biasS3Ngreater = sum(abs(bias_S3N) > abs(bias_SSN)), 
    sdS3Nless = sum(abs(sd_S3N) < abs(sd_SSN)), 
    sdS3Ngreater = sum(abs(sd_S3N) > abs(sd_SSN))
  )

param_summary2 = param_summary |>
  mutate(
    RMSE_S3N = sqrt(bias_S3N^2 + sd_S3N^2),
    RMSE_SSN = sqrt(bias_SSN^2 + sd_SSN^2)
  ) |>
  mutate(
    RMSE_ratio = RMSE_SSN/RMSE_S3N,
    RMSE_S3N_smaller = (RMSE_S3N < RMSE_SSN)
  )

param_summ_stats = param_summary2 |>
  group_by(parameter, RMSE_S3N_smaller) |>
  summarize(
    n = n(),
    median_ratio = median(RMSE_ratio),
    mean_ratio = mean(RMSE_ratio)
  )



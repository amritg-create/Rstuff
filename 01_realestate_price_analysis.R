# BUSINESS SCIENCE UNIVERSITY ----
# LEARNING LAB 73: Time Series Features ----
# Copyright: Business Science, LLC
# **** ----

# LIBRARIES ----

library(tidymodels) # MACHINE LEARNING IN R
library(tidyverse)  # CORE DATA ANALYSIS TOOLKIT
library(lubridate)  # Might need to load this if not loaded automatically with tidyverse
library(timetk)     # MY TIME SERIES TOOLKIT

# DATA -----

# * Housing Data ----
kc_house_data_raw_tbl <- read_csv("data/kc_house_data.csv")

kc_house_data_raw_tbl %>% glimpse()

# * 30-Year Mortgage Rates ----
mortgage_rate_tbl <- read_csv("data/mortgage_rate_tbl.csv")

mortgage_rate_tbl 

# * Employment Rate ----

payems_tbl <- read_csv("data/payems_tbl.csv")

payems_tbl

# * CPI ----

cpi_tbl <- read_csv("data/cpi_tbl.csv")

cpi_tbl

# EXPLORATORY TIME SERIES ANALYSIS ----

# * Home Sales ----

kc_summarized_tbl <- kc_house_data_raw_tbl %>%
    select(date, price) %>%
    summarise_by_time(
        date, 
        .by   = "month", 
        
        price = median(price),
        n     = n()
    ) %>%
    pivot_longer(-date) 

kc_summarized_tbl %>%
    group_by(name) %>%
    plot_time_series(date, value)

# * 30-Year Mortgage Rate ----
mortgage_rate_tbl %>%
    plot_time_series(date, price)

# * Employment Numbers (month-to-month change) ----
payems_tbl %>%
    plot_time_series(date, price)

payems_tbl %>%
    plot_time_series(date, diff_vec(price, silent = T), .smooth = TRUE)

# * CPI ----

cpi_tbl %>%
    plot_time_series(date, price)

# QUICK HACK: VIEW EVERYTHING TOGETHER WITH TRELLISCOPE ----

all_summarized_tbl <- kc_summarized_tbl %>%
    bind_rows(
        mortgage_rate_tbl %>% 
            rename(name = symbol, value = price)
    ) %>%
    bind_rows(
        payems_tbl %>%
            mutate(price = diff_vec(price, silent = T)) %>% 
            rename(name = symbol, value = price)
    ) %>%
    bind_rows(
        cpi_tbl %>%
            rename(name = symbol, value = price)
    )

all_summarized_tbl %>%
    group_by(name) %>%
    plot_time_series(
        .date_var    = date, 
        .value       = value,
        .trelliscope = T
    )

# INTRO TO TIME SERIES FEATURES ----

# * Example 30-Year Mortgage Rate ----

# ?tk_tsfeatures
# RESOURCE. List of all standard features: ----
#   http://pkg.robjhyndman.com/tsfeatures/reference/index.html

mortgage_rate_tbl %>%
    timetk::tk_tsfeatures(
        .date_var = date, 
        .value    = price, 
        .features = c("median", "frequency", "stl_features", "entropy", "acf_features")
    ) %>%
    glimpse()

mortgage_features_q_tbl <- mortgage_rate_tbl %>%
    
    # HACK #1 - Group by Quarter and treat each as it's own time series ----
    mutate(date_rounded = round_date(date, "quarter")) %>%
    group_by(date_rounded) %>%
    
    timetk::tk_tsfeatures(
        .date_var = date, 
        .value    = price, 
        .features = c("median", "frequency", "stl_features", "entropy", "acf_features"),
        .prefix   = "mort_"
    ) %>%
    ungroup()

mortgage_features_q_tbl %>% glimpse()

# Correlation

kc_house_data_raw_tbl %>%
    select(date, price) %>%
    mutate(date_rounded = round_date(date, "quarter")) %>%
    
    left_join(mortgage_features_q_tbl) %>%
    
    select(-date, - date_rounded) %>%
    cor() %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    as_tibble() %>%
    select(rowname, price) %>%
    arrange(desc(abs(price)))

# * Employment Numbers (month-to-month change) ----

payems_tbl %>%
    
    # HACK #2: DIFFERENCE THE PRICE ----
    mutate(price = diff_vec(price, silent = T)) %>%
    drop_na() %>%
    timetk::tk_tsfeatures(
        .date_var = date, 
        .value    = price, 
        .features = c("median", "frequency", "stl_features", "entropy", "acf_features")
    )

payems_features_q_tbl <- payems_tbl %>%
    
    mutate(price = diff_vec(price, silent = T)) %>%
    fill(price, .direction = "up") %>%
    
    mutate(date_rounded = floor_date(date, "quarter")) %>%
    group_by(date_rounded) %>%
    
    timetk::tk_tsfeatures(
        .date_var = date, 
        .value    = price, 
        .features = c("median", "frequency", "stl_features", "entropy", "acf_features"),
        .prefix   = "payems_"
    ) %>%
    ungroup()

payems_features_q_tbl

# Correlation

kc_house_data_raw_tbl %>%
    select(date, price) %>%
    mutate(date_rounded = round_date(date, "quarter")) %>%
    
    left_join(payems_features_q_tbl) %>%
    
    select(-date, - date_rounded) %>%
    cor() %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    as_tibble() %>%
    select(rowname, price) %>%
    arrange(desc(abs(price)))


# * Example CPI ----

cpi_tbl %>%
    timetk::tk_tsfeatures(
        .date_var = date, 
        .value    = price, 
        .features = c("median", "frequency", "stl_features", "entropy", "acf_features")
    )

cpi_features_q_tbl <- cpi_tbl %>%
    mutate(date_rounded = floor_date(date, "quarter")) %>%
    group_by(date_rounded) %>%
    timetk::tk_tsfeatures(
        .date_var = date, 
        .value    = price, 
        .features = c("median", "frequency", "stl_features", "entropy", "acf_features"), 
        .prefix   = "cpi_"
    ) %>%
    ungroup()

cpi_features_q_tbl

# Correlation

kc_house_data_raw_tbl %>%
    select(date, price) %>%
    mutate(date_rounded = round_date(date, "quarter")) %>%
    left_join(cpi_features_q_tbl) %>%
    select(-date, - date_rounded) %>%
    cor() %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    as_tibble() %>%
    select(rowname, price) %>%
    arrange(desc(abs(price)))



# MACHINE LEARNING ----

# * Data Preparation: Join Time Series Features ----

data_tbl <- kc_house_data_raw_tbl %>%
    
    select(-id) %>%
    mutate(year = year(date)) %>%
    mutate(month = month(date, label = TRUE)) %>%
    rename_all(~ str_glue("home_{.}")) %>%
    
    # HACK #3 - ROUND THE DATE SO I CAN JOIN!!!
    mutate(date_rounded = round_date(home_date, "quarter")) %>%
    select(-home_date) %>%
    
    select(date_rounded, everything()) %>%
    left_join(mortgage_features_q_tbl) %>%
    left_join(payems_features_q_tbl) %>%
    left_join(cpi_features_q_tbl) %>%
    
    select(-date_rounded) %>%
    rowid_to_column()

data_tbl


# * Resample ----

set.seed(123)
splits <- initial_split(data_tbl, prop = 0.80)

splits

training(splits) %>% glimpse()


# * Machine Learning ----

# ** 1. No time series features ----
recipe_spec_1 <- recipe(home_price ~ ., 
                        data = training(splits) %>% 
                            select(-rowid, -starts_with("cpi_"), -starts_with("payems_"), -starts_with("mort_"))
                        ) %>%
    step_dummy(home_month, one_hot = TRUE)

recipe_spec_1 %>% prep() %>% juice() %>% glimpse()

wflw_fit_1 <- workflow() %>%
    add_model(
        boost_tree(mode = "regression", learn_rate = 0.35) %>% set_engine("xgboost")
    ) %>%
    add_recipe(recipe_spec_1) %>%
    fit(training(splits))

# ** 2. With time series features ----
recipe_spec_2 <- recipe(home_price ~ ., data = training(splits) %>% select(-rowid)) %>%
    step_dummy(home_month, one_hot = TRUE)

recipe_spec_2 %>% prep() %>% juice() %>% glimpse()

wflw_fit_2 <- workflow() %>%
    add_model(
        boost_tree(mode = "regression", learn_rate = 0.35) %>% set_engine("xgboost")
    ) %>%
    add_recipe(recipe_spec_2) %>%
    fit(training(splits))


# ACCURACY ----

# * 1. No TS Features ----
wflw_fit_1 %>%
    predict(testing(splits)) %>%
    bind_cols(testing(splits) %>% select(home_price)) %>% 
    metric_set(rmse, rsq)(home_price, .pred)

# * 2. With TS Features ----
wflw_fit_2 %>%
    predict(testing(splits)) %>%
    bind_cols(testing(splits) %>% select(home_price)) %>% 
    metric_set(rmse, rsq)(home_price, .pred)

# FEATURE IMPORTANCE ----

wflw_fit_2$fit$fit$fit %>%
    xgboost::xgb.importance(model = .) %>%
    mutate(Feature = fct_reorder(Feature, Gain)) %>%
    ggplot(aes(x = Feature, y = Gain)) +
    geom_point() +
    coord_flip() +
    labs(title = "Feature Importance")

# CONCLUSIONS ----

# WE GOT ABOUT A 1% IMPROVEMENT. IS THIS GOOD?
# FOR A COMPANY LIKE WALMART, THAT'S $100,000,000 SAVINGS... HUGE. 
# WHO'S READY FOR A PYTHON DEMO?


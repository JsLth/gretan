library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(sf)
library(factoextra)
library(kableExtra)

pov <- readRDS("data-ext/survey_resampled.rds") %>%
  select(starts_with(c(
    "c6",  # Energy saving #1
    "c11", # Energy complexity
    "c13", # Energy saving #2
    "c16", # Cooling system
    "c18", # Heating system
    "c19", # Heating system configuration
    "c22", # Heating costs
    "c24", # Hot water costs
    "c26", # Energy costs
    "c28", # Consensual energy poverty
    "c29", # Housing type
    "c30", # Housing area
    "c31", # Housing age
    "c32", # Major renovations
    "c33", # Performance certificate
    "c34", # Energy rating
    "c48", # Household size
    "c51", # Special conditions
    "c55"  # Income
  )), "id", "nuts0", "nuts1", "nuts2")

cb <- readRDS("data-ext/codebook.rds") %>%
  filter(variable %in% names(pov))

all_cats <- unique(tail(cb$category, -1))
all_vars <- unique(tail(cb$og_var, -1))

for (v in all_vars) {
  cb_v <- cb[cb$og_var %in% v, ]
  opts <- cb_v$option
  ext_var <- cb_v$variable
  if (!all(is.na(opts)) && length(opts) > 1) {
    for (o in seq_along(opts)) {
      col <- which(grepl(v, ext_var))[[o]]
      pov[ext_var[col]] <- if_else(
        pov[ext_var[col]][[1]] == 1, opts[o], NA
      )
    }
    if (is.character(pov[[ext_var[1]]])) {
      pov[[ext_var[1]]] <- factor(do.call(
        coalesce,
        as.list(st_drop_geometry(pov[ext_var]))
      ), levels = cb_v$labels[[1]])
    }
    pov[tail(ext_var, -1)] <- NULL
    names(pov)[names(pov) %in% ext_var[1]] <- v
  }
}


vars_lookup <- list(
  # demographic variables
  household = "c48_1_open",
  cond_air = "c51_1",
  cond_heat = "c51_2",
  cond_trans = "c51_3",
  cond_smoke = "c51_4",
  cond_support = "c51_5",
  income = "c55",
  
  # behavioral variables
  behav_unplug = "c6_1",
  behav_products = "c6_2",
  behav_lights = "c6_3",
  behav_share = "c6_4",
  behav_carpool = "c6_5",
  behav_car = "c6_6",
  behav_temp = "c13_1",
  behav_shower = "c13_2",
  behav_drive = "c13_3",
  behav_dishwash = "c13_4",
  
  # knowledge variables
  know_energy = "c11_1",
  know_heating = "c11_2",
  know_costs = "c11_3",
  know_share = "c11_4",
  know_solutions = "c11_5",
  
  # energy access variables
  has_cooling = "c16", # include as yes/no or divide by option?
  has_heating = "c18_1", # include as yes/no or divide by option?
  heat_config = "c19",
  
  # energy affordability variables
  heating_cost = "c22",
  water_cost = "c24",
  energy_cost = "c26",
  ability_to_pay = "c28_1",
  supplier_threat = "c28_2",
  safety_winter = "c28_3",
  safety_summer = "c28_4",
  
  # housing variables
  house_type = "c29",
  house_area = "c30",
  house_age = "c31",
  house_renov = "c32_1",
  house_cert = "c33",
  house_rate = "c34"
)


pov <- do.call(select, c(vars_lookup, id, nuts0, nuts1, nuts2))


pov <- pov %>%
  # Convert non-responses to NA
  mutate(across(
    where(is.factor),
    .fns = function(x) {
      lev <- levels(x)
      as.character(x) %>%
        na_if("I do not know") %>%
        na_if("Prefer not to say") %>%
        factor(levels = lev)
    }
  )) %>%
  # Unify binary codings
  mutate(across(where(is.logical), .fns = as.numeric)) %>%
  # Convert Yes/No to binary
  mutate(across(
    where(~identical(unique(na.omit(as.character(.x))), c("Yes", "No"))),
    .fns = ~case_match(.x, "Yes" ~ 1, "No" ~ 0, .default = NA)
  )) %>%
  mutate(
    heat_config = case_when(
      heat_config %in% "Central heating (heating the whole building where I live)" ~ 1,
      !heat_config %in% "Central heating (heating the whole building where I live)" ~ 0,
      .default = NA
    ),
    has_cooling = case_when(
      !has_cooling %in% "No" ~ 1,
      has_cooling %in% "No" ~ 0,
      .default = NA
    ),
    house_type = case_when(
      house_type %in% c("Detached house", "Semi-detached house") ~ 1,
      !house_type %in% c("Detached house", "Semi-detached house") ~ 0,
      .default = NA
    ),
    house_rate = case_when(
      house_rate %in% c("A or higher (e.g. A+)", "B or higher (e.g. B+)") ~ 1,
      !house_rate %in% c("A or higher (e.g. A+)", "B or higher (e.g. B+)") ~ 0,
      .default = NA
    )
  ) %>%
  rename(heat_central = heat_config, house_detach = house_type)

pov <- pov %>%
  mutate(across(
    where(is.factor) & !any_of(c("id", "nuts0", "nuts1", "nuts2")),
    .fns = as.numeric
  )) %>%
  select(where(~!all(.x == 0, na.rm = TRUE)))

summ <- tbl_summary(
  st_drop_geometry(select(pov, !any_of(c("id", "nuts0", "nuts1", "nuts2")))),
  statistic = list(
    all_continuous2() ~ c("{mean}", "{sd}", "{median} ({min}, {max})"),
    income ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    behav_unplug ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    behav_products ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    behav_lights ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    behav_share ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    behav_carpool ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    behav_car ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    know_energy ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    know_heating ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    know_costs ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    know_share ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    know_solutions ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    heating_cost ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    water_cost ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    energy_cost ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    ability_to_pay ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    supplier_threat ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    safety_winter ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    safety_summer ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    house_area ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    house_age ~ c("{median} ({p25}, {p75})", "{min}, {max}")
  ),
  type = list(
    household ~ "continuous2",
    income ~ "continuous2",
    behav_unplug ~ "continuous2",
    behav_products ~ "continuous2",
    behav_lights ~ "continuous2",
    behav_share ~ "continuous2",
    behav_carpool ~ "continuous2",
    behav_car ~ "continuous2",
    know_energy ~ "continuous2",
    know_heating ~ "continuous2",
    know_costs ~ "continuous2",
    know_share ~ "continuous2",
    know_solutions ~ "continuous2",
    heating_cost ~ "continuous2",
    water_cost ~ "continuous2",
    energy_cost ~ "continuous2",
    ability_to_pay ~ "continuous2",
    supplier_threat ~ "continuous2",
    safety_winter ~ "continuous2",
    safety_summer ~ "continuous2",
    house_area ~ "continuous2",
    house_age ~ "continuous2"
  ),
  label = list(
    "household" ~ "Household size",
    "cond_air" ~ "Condition requiring additional air",
    "cond_heat" ~ "Condition requiring additional heating",
    "cond_trans" ~ "Condition requiring additional transport",
    "cond_smoke" ~ "Smoker",
    "cond_support" ~ "Receiving public support",
    "income" ~ "Income",
    "behav_unplug" ~ "Unplug electronic devices",
    "behav_products" ~ "Search for products that are more energy-efficient",
    "behav_lights" ~ "Turn off lights before leaving a room",
    "behav_share" ~ "Encourage friends or family to be more energy efficient",
    "behav_carpool" ~ "Consciously participate in carpooling",
    "behav_car" ~ "Consciously choose to travel without a car",
    "behav_temp" ~ "Lower the temperature set point of your water heater",
    "behav_shower" ~ "Take shorter showers",
    "behav_drive" ~ "Drive slower on the highway",
    "behav_dishwash" ~ "Run full loads in the dishwasher",
    "know_energy" ~ "Understanding my monthly electricity consumption",
    "know_heating" ~ "Understanding my monthly heating and/or cooling consumption",
    "know_costs" ~ "Knowing how much I spend in energy",
    "know_share" ~ "Knowing the share of renewable energy that I consume",
    "know_solutions" ~ "Implementing green energy solutions at home",
    "has_cooling" ~ "Has a cooling system",
    "has_heating" ~ "Has a heating system",
    "heat_central" ~ "Has central heating",
    "heating_cost" ~ "Heating costs",
    "water_cost" ~ "Hot water costs",
    "energy_cost" ~ "Electricity costs",
    "ability_to_pay" ~ "How often did you worry that you wouldnt be able to pay your home energy bill?",
    "supplier_threat" ~ "How often did you have a supplier threaten you to disconnect your electricity or home heating fuel service, or discontinue making fuel deliveries?",
    "safety_winter" ~ "During the winter months, how often did you keep your home at a temperature that you felt was unsafe or unhealthy?",
    "safety_summer" ~ "During the summer months, how often did you keep your home at a temperature that you felt was unsafe or unhealthy?",
    "house_detach" ~ "Living in a detached house",
    "house_area" ~ "Housing area",
    "house_age" ~ "Housing age",
    "house_renov" ~ "Major renovations since construction",
    "house_cert" ~ "Has an Energy Performance Certificate",
    "house_rate" ~ "Energy rating of B or higher"
  )
) %>%
  as_kable_extra(format = "latex")


clean_pov <- pov %>%
  select(where(is.numeric) & !id) %>%
  st_drop_geometry()

item_desc <- c(
  household = "Household size",
  cond_air = "Chronical disease or disability that requires more air",
  cond_heat = "Chronical disease or disability that requires more heating",
  cond_trans = "Chronical disease or disability that requires special care for transport",
  cond_smoke = "Smoking",
  cond_support = "Receiving any form of public support such as social welfare payments or housing allowances",
  income = "Household's annual mean net income [€] in the last year",
  behav_unplug = "Performs activity: Unplug electronic devices that are not being used",
  behav_products = "Performs activity: Actively search for products that are more energy efficient",
  behav_lights = "Performs activity: Turn off all lights before leaving a room",
  behav_share = "Performs activity: Encourage friends or family to be more energy efficient",
  behav_carpool = "Performs activity: Consciously participate in carpooling",
  behav_car = "Performs activity: Consciously choose to travel without a car (e.g., walk, bike, public transport, etc.)",
  behav_temp = "Performs activity: Lower the temperature set point of your water heater",
  behav_shower = "Performs activity: Take shorter showers",
  behav_drive = "Performs activity: Drive slower on the highway",
  behav_dishwash = "Performs activity: Run full loads in the dishwasher",
  know_energy = "Finds complex: Understanding monthly electricity consumption",
  know_heating = "Finds complex: Understanding my monthly heating and/or cooling consumption",
  know_costs = "Finds complex: Understanding my monthly heating and/or cooling consumption",
  know_share = "Finds complex: Knowing the share of renewable energy and fossil fuel-based energy that I consume",
  know_solutions = "Finds complex: Implementing green energy solutions at home",
  has_cooling = "Has a cooling system",
  has_heating = "Has a heating system",
  heat_central = "Has a central heating system",
  heating_cost = "Average monthly expenditure [€] for heating in the last year",
  water_cost = "Average monthly expenditure [€] for hot water in the last year",
  energy_cost = "Average monthly expenditure [€] for electricity in the last year",
  ability_to_pay = "How often did you worry that you wouldnt be able to pay your home energy bill?",
  supplier_threat = "How often did you have a supplier threaten you to disconnect your electricity or home heating fuel service, or discontinue making fuel deliveries?",
  safety_winter = "During the winter months, how often did you keep your home at a temperature that you felt was unsafe or unhealthy?",
  safety_summer = "During the summer months, how often did you keep your home at a temperature that you felt was unsafe or unhealthy?",
  house_detach = "Lives in a (semi) detached house",
  house_area = "Area [m²] of home",
  house_age = "Construction year of house",
  house_renov = "Did the house have at least one major renovation since its construction?",
  house_cert = "Does the house have an energy performance certificate?",
  house_rate = "What is the energy rating of the house?"
)

tibble(
  Variable = paste0("\\texttt{", names(clean_pov), "}"),
  #Item = item_desc,
  Missing = map_dbl(clean_pov, ~sum(is.na(.x))),
  Mean = round(map_dbl(clean_pov, mean, na.rm = TRUE), 2),
  SD = round(map_dbl(clean_pov, sd, na.rm = TRUE), 2),
  Min = round(map_dbl(clean_pov, min, na.rm = TRUE), 2),
  P25 = round(map_dbl(clean_pov, quantile, probs = 0.25, na.rm = TRUE), 2),
  Median = round(map_dbl(clean_pov, median, na.rm = TRUE), 2),
  P75 = round(map_dbl(clean_pov, quantile, probs = 0.75, na.rm = TRUE), 2),
  Max = round(map_dbl(clean_pov, max, na.rm = TRUE), 2),
) %>%
  kbl(format = "latex", booktabs = TRUE, longtable = TRUE) %>%
  str_replace_all(fixed("\\{"), "{") %>%
  str_replace_all(fixed("\\}"), "}") %>%
  str_replace_all(fixed("\\textbackslash{}"), "\\") %>%
  cat()



t <- pov %>%
  select(!any_of(c("id", "nuts0", "nuts1", "nuts2", "geometry"))) %>%
  st_drop_geometry()
t2 <- prcomp(as.formula(paste("~", paste(names(t), collapse = " + "))), data = t)

t <- prcomp(st_drop_geometry(select(pov, !any_of(c("id", "nuts0", "nuts1", "nuts2", "geometry")))), na.action = na.omit)

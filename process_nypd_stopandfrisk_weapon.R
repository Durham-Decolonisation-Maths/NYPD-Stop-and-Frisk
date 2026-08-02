########## NYPD Stop-and-Frisk: clean -> label -> bridge, in one file.
########## ---- settings ----
prefix         = "stopfrisk"
labels         = c("heuristic","found.weapon")
labelnames     = c("score","outcome")
truefactors    = "precinct"
impute         = "mean"        
SAMPLE_SIZE    = 5000          # target row count for nypd_cpw_stops.csv
MIN_PER_GROUP  = 25            # keep at least this many rows per race group, if available
set.seed(42)


to01 = function(x)
{
  if (is.numeric(x)) return(as.numeric(x))
  if (is.logical(x)) return(as.numeric(x))
  as.numeric(as.character(x) %in% c("Y","y","TRUE","T","1"))
}


is_numeric_col = function(x)
{
  vals = as.character(x)
  vals = vals[!is.na(vals)]
  if (length(vals) == 0) return(FALSE)
  !any(is.na(suppressWarnings(as.numeric(vals))))
}

drop_columns = function(data, drop)
{
  drop = intersect(drop, names(data))
  if (length(drop) > 0) data[drop] = NULL
  return(data)
}

get_dup_columns = function(data)
{
  fp = sapply(data, function(col) paste(as.character(col), collapse = ""))
  names(data)[duplicated(fp)]
}

get_oneval_columns = function(data)
{
  names(data)[sapply(data, function(col) length(unique(col)) == 1)]
}

get_NA_columns = function(data, dropthreshold)
{
  thresh = max(nrow(data) * dropthreshold, 1)
  miss = sapply(data, function(col) sum(is.na(col)))
  names(data)[miss >= thresh]
}

remove_useless_vars = function(data)
{
  drop = unique(c(get_dup_columns(data), get_oneval_columns(data), get_NA_columns(data, 1)))
  drop_columns(data, drop)
}

remove_unused_levels = function(data)
{
  for (i in seq_along(data)) if (is.factor(data[[i]])) data[[i]] = factor(data[[i]])
  return(data)
}

check_type = function(data, truefactors)
{
  for (nm in names(data))
  {
    col = data[[nm]]
    if (is.factor(col) && is_numeric_col(col)) data[[nm]] = as.numeric(as.character(col))
    else if (!is.factor(col) && !is.numeric(col)) data[[nm]] = factor(col)
    if (!is.null(truefactors) && nm %in% truefactors) data[[nm]] = factor(data[[nm]])
  }
  return(data)
}


fix_NAs = function(data, impute)
{
  if (impute == "drop")
  {
    keep = complete.cases(data)
    cat(sum(!keep), "obs dropped because of NAs\n")
    return(data[keep, ])
  }
  if (impute == "mean")
  {
    for (nm in names(data))
    {
      col = data[[nm]]
      if (!any(is.na(col))) next
      if (is.numeric(col)) { col[is.na(col)] = median(col, na.rm = TRUE) }
      else
      {
        tab = table(col)
        mode_val = names(tab)[which.max(tab)]
        col = as.character(col); col[is.na(col)] = mode_val; col = factor(col)
      }
      data[[nm]] = col
    }
    return(data)
  }
  if (impute == "none") return(data)
  stop("Imputation options supported here: drop, mean, none")
}

scale_down_label = function(data, labels, const = 1000)
{
  for (nm in labels)
  {
    if (!is.factor(data[[nm]]) && median(data[[nm]]) > const) data[[nm]] = data[[nm]] / const
  }
  return(data)
}


change_label = function(data, labels, k)
{
  data_out = data[, setdiff(names(data), labels), drop = FALSE]
  labelk = data[[labels[k]]]
  if (length(unique(labelk)) == 2)
  {
    counts = sort(table(labelk))              # smallest count first
    recoded = ifelse(labelk == names(counts)[1], 1, 0)   # minority -> 1
    labelk = factor(recoded)
  }
  data_out[[labels[k]]] = labelk
  return(data_out)
}

print_summary = function(data)
{
  cat(nrow(data), "observations,", ncol(data), "variables\n")
  cat(sum(!complete.cases(data)), "observations have at least one missing value\n")
}

process_data = function(data, prefix, labels, labelnames, impute, truefactors = NULL)
{
  stopifnot(length(labelnames) == length(labels))
  keep = complete.cases(data[, labels])
  data = data[keep, ]
  data = check_type(data, truefactors)
  data = remove_useless_vars(data)
  print_summary(data)
  data = fix_NAs(data, impute)
  data = remove_useless_vars(data)
  data = remove_unused_levels(data)
  data = scale_down_label(data, labels)

  for (i in seq_along(labels))
  {
    data_out = change_label(data, labels, i)
    filename = paste0(prefix, "_", labelnames[i], ".csv")
    write.csv(data_out, filename, quote = FALSE, row.names = FALSE)
    cat("wrote", filename, ":", nrow(data_out), "rows x", ncol(data_out), "cols\n")
  }
  return(data)
}

########## ---- 1. filter to CPW crime, 2009-2010 (following Goel et al.) ----
data = stops[stops$suspected.crime == "cpw" & (stops$year == 2009 | stops$year == 2010) &
             !is.na(stops$suspected.crime) & !is.na(stops$year), ]

########## ---- 2. heuristic risk score ----
data$heuristic = 3*to01(data$stopped.bc.object) + 1*to01(data$stopped.bc.bulge) + 1*to01(data$additional.sights)

########## ---- 3. date/time features ----
month_num = as.numeric(format(as.Date(data$date), "%m"))
data$monthofyear = factor(month_num, levels = 1:12,
  labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
data$dayofweek = weekdays(as.Date(data$date))
time_parts = do.call(rbind, strsplit(as.character(data$time), ":"))
timeofday_secs = as.numeric(time_parts[,1]) * 3600 + as.numeric(time_parts[,2]) * 60
breaks = seq(0, 24, 4) * 60 * 60
temp = findInterval(timeofday_secs, breaks)
data$timeofday = factor(temp, levels = sort(unique(temp)),
  labels = c("0-4","4-8","8-12","12-16","16-20","20-24")[sort(unique(temp))])

########## ---- 4. drop administrative / leaky / high-missingness columns ----
drop = c("year","serial","suspect.dob","lat","lon","xcoord","ycoord","id","date","time",
         "arrested","arrested.reason","summons.issued","frisked","searched",
         names(data)[grepl("force", names(data))],
         names(data)[grepl("frisked", names(data))],
         names(data)[grepl("searched", names(data))],
         names(data)[grepl("found", names(data))],
         "officer.verbal","officer.shield","suspect.hair","stop.length")
drop = setdiff(drop, labels)
data = drop_columns(data, drop)

########## ---- 5. drop rows missing key variables, relevel ordinals ----
keep = complete.cases(data.frame(data$suspect.sex, data$suspect.race, data$suspect.weight))
data = data[keep, ]
data$dayofweek = factor(data$dayofweek,
  levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
data$suspect.eye = factor(data$suspect.eye,
  levels = c("black","blue","brown","gray","green","hazel","maroon","pink","violet","other","two different","unknown"))

########## ---- 6. impute, dedupe, output stopfrisk_score.csv / stopfrisk_outcome.csv ----
data = process_data(data, prefix, labels, labelnames, impute, truefactors)

########## ---- 7. bridge stopfrisk_outcome.csv -> nypd_cpw_stops.csv ----
########## subsamples -- stratified by race -- to a small, downloadable file)
df = read.csv("stopfrisk_outcome.csv", stringsAsFactors = FALSE)
rename_map = c(found.weapon = "found_weapon", stopped.bc.object = "stopped_bc_object",
               stopped.bc.bulge = "stopped_bc_bulge", timeofday = "time_of_day",
               suspect.race = "race", suspect.sex = "sex",
               suspect.weight = "weight", suspect.eye = "eye")
present = intersect(names(rename_map), names(df))
names(df)[match(present, names(df))] = rename_map[present]

cat("bridge input:", nrow(df), "rows x", ncol(df), "cols\n")

if (!is.null(SAMPLE_SIZE) && nrow(df) > SAMPLE_SIZE)
{
  idx_by_race = split(seq_len(nrow(df)), df$race)
  sampled_idx = unlist(lapply(idx_by_race, function(idx) {
    n_i = round(SAMPLE_SIZE * length(idx) / nrow(df))
    n_i = max(n_i, min(length(idx), MIN_PER_GROUP))
    sample(idx, min(n_i, length(idx)))
  }))
  df = df[sort(sampled_idx), ]
  cat("subsampled (stratified by race) to:", nrow(df), "rows\n")
}

write.csv(df, "nypd_cpw_stops.csv", quote = FALSE, row.names = FALSE)
cat("wrote nypd_cpw_stops.csv:", nrow(df), "rows x", ncol(df), "cols,",
    round(file.size("nypd_cpw_stops.csv")/1024, 1), "KB\n")

cat("\nDone. Run your own \"FPR Disparity by Race.R\" next -- it reads nypd_cpw_stops.csv.\n")

# =========================================
# Setup
# =========================================
pkgs <- c("tidyverse","janitor","stringr","stringi","cli","purrr","tidyr","readr","irr","psych")
invisible(lapply(setdiff(pkgs, rownames(installed.packages())), install.packages))
invisible(lapply(pkgs, library, character.only = TRUE))

# path <- "/mnt/data/spreads.csv"

raw <- readr::read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTmmggYZHSB2lADFcnXXr8aGS6SxxOV_mCwVEU6loRWYodx6ggHmLXu2ZTood7VBLoND1CsBEOkHTsP/pub?gid=368164262&single=true&output=csv", show_col_types = FALSE) |>
  janitor::remove_empty("cols")
stopifnot(nrow(raw) > 0)

# Fuzzy column-finders
cn <- names(raw)
find_col <- function(patterns) {
  ix <- unlist(lapply(patterns, function(p) grep(p, cn, ignore.case = TRUE, perl = TRUE)))
  if (length(ix)) cn[ix[1]] else stop("Column not found: ", paste(patterns, collapse=", "))
}
col_url   <- find_col(c("^Input URL","\\burl\\b"))
col_coder <- find_col(c("^Coder$","Coder"))
col_id    <- if (any(grepl("^Input ID", cn))) find_col(c("^Input ID")) else NULL

dat <- raw |>
  mutate(across(everything(), ~ ifelse(is.na(.x), NA, as.character(.x)))) |>
  rename(url = !!col_url, coder = !!col_coder) |>
  relocate(coder, url, .before = 1) |>
  # Remove duplicates - keep the newest (last) occurrence of each coder-URL combination
  group_by(coder, url) |>
  slice(n()) |>
  ungroup()

# Debug: Let's see what the data looks like
cat("=== DEBUGGING DATA STRUCTURE ===\n")
cat("Total rows in dataset:", nrow(dat), "\n")
cat("Unique coders:", paste(unique(dat$coder), collapse = ", "), "\n")
cat("Number of unique URLs:", length(unique(dat$url)), "\n")
cat("Sample URLs:\n")
print(head(unique(dat$url), 3))
cat("================================\n\n")

# Normalization helper (lowercase, remove accents, collapse spaces)
norm_txt <- function(x) {
  x |>
    stringi::stri_trans_general("NFKD; Latin-ASCII; Lower") |>
    str_replace_all("[^[:alnum:][:space:]:/()&+-]", " ") |>
    str_squish()
}

# Build tolerant regex from a canonical label, ignoring punctuation/spacing differences
to_pattern <- function(label) {
  z <- norm_txt(label)
  toks <- str_split(z, "[^a-z0-9]+", simplify = TRUE)
  toks <- toks[toks != ""]
  if (!length(toks)) return("(?!)")
  paste0("(?<![a-z0-9])(", paste(toks, collapse = "[^a-z0-9]*"), ")(?![a-z0-9])")
}

# =========================================
# Variables (names as they appear in your form)
# =========================================

# SINGLE-LABEL variables (exactly one option chosen)
vars_single <- c(
  "Post availability",
  "Image(s)/Video(s) included",
  "AI Image/Video",
  "AI Labeling in Image/Video",
  "AI Labeling in Post Text",
  "AI Labeling in Post/Ad",
  "Nature of the Image/Video",
  "Number of People in the Image/Video",
  "Negative Tendency/Tonality",
  "Positive Tendency/Tonality",
  "Topics"  # main topic is single choice
)

# MULTI-LABEL variables that we should parse, but:
# - "People-like" vars are comma-splittable (no commas inside labels typically)
# - others must use regex (labels can contain commas in the text)
vars_people_like <- c("People in the Image/Video", "Names of Actors")
vars_multi_regex <- c(
  "Theme of the Image/Video",
  "Actors",
  "Type of Attack",
  "Target of Attack",
  "Type of Acclaim",
  "Target of Acclaim"
)

# For multi-select variables, we need to handle them as sets for reliability analysis
# This means we need to create a different approach for calculating reliability

# Function to calculate set-based reliability for multi-select variables
calculate_set_reliability <- function(df, var_name) {
  # Get the original multi-select column
  original_col <- find_var_col(var_name, names(dat))
  
  # Create sets for each coder
  sets_data <- dat |>
    select(coder, url, !!original_col) |>
    # Don't filter out NAs - treat them as empty sets
    mutate(
      # Split comma-separated values and create sets
      value_set = map(!!sym(original_col), function(x) {
        if (is.na(x) || x == "") return(character(0))
        values <- str_split(x, ",") |> 
          unlist() |> 
          str_trim()
        values[values != ""]
      })
    ) |>
    select(coder, url, value_set)
  
  # Calculate Jaccard similarity for each URL
  jaccard_similarity <- function(set1, set2) {
    if (length(set1) == 0 && length(set2) == 0) return(1)
    if (length(set1) == 0 || length(set2) == 0) return(0)
    intersection <- length(intersect(set1, set2))
    union <- length(union(set1, set2))
    intersection / union
  }
  
  # Calculate pairwise similarities
  urls <- unique(sets_data$url)
  similarities <- map_dbl(urls, function(url_val) {
    url_data <- sets_data |> filter(url == url_val)
    coders <- unique(url_data$coder)
    
    if (length(coders) < 2) return(NA_real_)
    
    # Calculate average pairwise Jaccard similarity
    pairs <- combn(coders, 2, simplify = FALSE)
    pair_similarities <- map_dbl(pairs, function(pair) {
      set1_data <- url_data |> filter(coder == pair[1]) |> pull(value_set)
      set2_data <- url_data |> filter(coder == pair[2]) |> pull(value_set)
      set1 <- set1_data[[1]]
      set2 <- set2_data[[1]]
      jaccard_similarity(set1, set2)
    })
    
    mean(pair_similarities, na.rm = TRUE)
  })
  
  # Return summary statistics
  tibble(
    variable = var_name,
    n_units = length(urls),
    mean_jaccard_similarity = mean(similarities, na.rm = TRUE),
    median_jaccard_similarity = median(similarities, na.rm = TRUE),
    min_jaccard_similarity = min(similarities, na.rm = TRUE),
    max_jaccard_similarity = max(similarities, na.rm = TRUE)
  )
}

# Parties (for token discovery like "PVV: Geert Wilders"); we won't hard-code people.
parties <- c("CDA","GL-PvdA","PVV","VVD","D66","BBB","SP","FvD","CU","JA21","Volt","SGP","Denk","NSC","PvdD")

# Canonical label dictionaries (only for the regex-parsed multi-labels).
# Keep these high-level items; we are NOT enumerating person names.
dict <- list(
  `Theme of the Image/Video` = c(
    "Event announcement/Election call/Candidate presentation",
    "Political positioning",
    "Negative imagery/Negative campaigning: The image/video portrays the political opponent in a negative light. This can happen through sharp criticism or with humor.",
    "Positive imagery/Acclaims: The image/video portrays past and future achievements of the party/government in a positive light. This includes drawing a positive balance, saying “thank you” to campaign helpers, or positively discussing political plans/goals.",
    "Symbolic image of person(s)/politician(s)/citizen(s) looking directly into the camera (classic campaign poster image).",
    "Events/Landscape & city scenes: Unlike a symbolic image, people/politicians are doing something, moving, or working (e.g., groups such as politicians, refugees, farmers, protesters, police officers, etc.). Alternatively, a city or (natural) landscape is shown where something is happening (e.g., forest dieback, fire, or flooding, nuclear power plants or wind turbines).",
    "Image from a campaign event",
    "Individual voter contact",
    "Media work (incl. TV debate)",
    "Private background story",
    "Everyday politics",
    "Nostalgic / historical Dutch imagery (references to glorified past or national symbols)",
    "Humor / Satire (depictions used to mock, exaggerate, or parody)",
    "Fantasy / futuristic scenarios or depictions of imagined or speculative realities (utopian or dystopian future Netherlands)",
    "Other imagery"
  ),
  `Actors` = c(
    "Ordinary people / average citizens / ordinary supporters",
    "(Members of) minorities",
    "Illegal actors, criminals, offenders, terrorists, extremists / extremist groups",
    "Dutch government, ministries, parliament, and courts (ALSO KABINET SCHOOF)",
    "Governments, ministries, parliaments and courts of the provinces",
    parties,                      # party-level buckets (e.g., "PVV")
    "Partijkartel","Gevestigde partijen",
    "Other parties from the Netherlands",
    "Other politicians from Dutch parties",
    "Foreign politicians or parties and/or foreign governments, foreign parliaments",
    "Europe/EU/NATO, EU/NATO institutions and bodies & their representatives",
    "(Representatives of) non-political institutions",
    "(Representatives of) interest groups, NGOs",
    "(Representatives of) social movements",
    "Companies / business leaders",
    "Celebrities and/or opinion leaders",
    "News media and their journalists / representatives",
    "Other actors"
  ),
  `Type of Attack` = c(
    "Attack on political issues/achievements/ideologies",
    "Attack on a person/party/organization/institution",
    "Other form of attack/negative tonality"
  ),
  `Target of Attack` = c(
    "No actor attacked",
    "Ordinary people / average citizens / ordinary supporters",
    "(Members of) minorities",
    "Illegal actors, criminals, offenders, terrorists, extremists / extremist groups",
    "Dutch government, ministries, parliament, and courts (ALSO KABINET SCHOOF)",
    "Governments, ministries, parliaments and courts of the provinces",
    parties,
    "Other parties from the Netherlands","Other politicians from Dutch parties",
    "Foreign politicians or parties and/or foreign governments, foreign parliaments",
    "Europe/EU/NATO, EU/NATO institutions and bodies & their representatives",
    "(Representatives of) non-political institutions",
    "(Representatives of) interest groups, NGOs",
    "(Representatives of) social movements",
    "Companies / business leaders",
    "Celebrities and/or opinion leaders",
    "News media and their journalists / representatives",
    "Other actors"
  ),
  `Type of Acclaim` = c(
    "Acclaim/Praise/Recognition of political issues/achievements/ideologies",
    "Acclaim/Praise/Recognition of a person/party/organization/institution",
    "Other form of recognition/praise"
  ),
  `Target of Acclaim` = c(
    "No actor praised",
    "Ordinary people / average citizens / ordinary supporters",
    "(Members of) minorities",
    "Illegal actors, criminals, offenders, terrorists, extremists / extremist groups",
    "Dutch government, ministries, parliament, and courts (ALSO KABINET SCHOOF)",
    "Governments, ministries, parliaments and courts of the provinces",
    parties,
    "Other parties from the Netherlands","Other politicians from Dutch parties",
    "Foreign politicians or parties and/or foreign governments, foreign parliaments",
    "Europe/EU/NATO, EU/NATO institutions and bodies & their representatives",
    "(Representatives of) non-political institutions",
    "(Representatives of) interest groups, NGOs",
    "(Representatives of) social movements",
    "Companies / business leaders",
    "Celebrities and/or opinion leaders",
    "News media and their journalists / representatives",
    "Other actors"
  )
)

# ---------- Robust column matcher (replace previous find_var_col and mapping) ----------
norm_key <- function(x) {
  x <- stringi::stri_trans_general(x, "NFKD; Latin-ASCII; Lower")
  x <- gsub("[^a-z0-9]+", " ", x, perl = TRUE)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  return(x)
}

find_var_col <- function(var_label, candidate_names) {
  # 1) exact fixed
  hit <- which(candidate_names == var_label)
  if (length(hit) >= 1) return(candidate_names[hit[1]])
  
  # 2) contains fixed
  hit <- which(grepl(var_label, candidate_names, fixed = TRUE))
  if (length(hit) >= 1) return(candidate_names[hit[1]])
  
  # 3) normalized equality
  nl <- norm_key(var_label)
  nn <- norm_key(candidate_names)
  hit <- which(nn == nl)
  if (length(hit) >= 1) return(candidate_names[hit[1]])
  
  # 4) normalized contains
  hit <- which(grepl(nl, nn, fixed = TRUE))
  if (length(hit) >= 1) return(candidate_names[hit[1]])
  
  stop("Cannot find column for '", var_label, "'")
}


map_vars_to_cols <- function(var_labels, df_names) {
  setNames(vapply(var_labels, function(v) find_var_col(v, df_names), character(1)), var_labels)
}


# ---------- Use it on your current 'dat' ----------
df_names <- names(dat)

vars_single <- c(
  "Post availability",
  "Image(s)/Video(s) included",
  "AI Image/Video",
  "AI Labeling in Image/Video",
  "AI Labeling in Post Text",
  "AI Labeling in Post/Ad",
  "Nature of the Image/Video",
  "Number of People in the Image/Video",
  "Negative Tendency/Tonality",
  "Positive Tendency/Tonality",
  "Topics"
)

vars_people_like <- c("People in the Image/Video", "Names of Actors")

vars_multi_regex <- c(
  "Theme of the Image/Video",
  "Actors",
  "Type of Attack",
  "Target of Attack",
  # NOTE: in your CSV this one appears as a long description line;
  # the robust matcher will still find it:
  "Type of Acclaim",
  "Target of Acclaim"
)

cols_single <- map_vars_to_cols(vars_single, df_names)
cols_people <- map_vars_to_cols(vars_people_like, df_names)
cols_multi  <- map_vars_to_cols(vars_multi_regex, df_names)


# =========================================
# Expand PEOPLE-LIKE (comma-splittable) to binaries
# =========================================

# Define the actual categories from the form for each multi-select variable
# This includes all politician names from the categories.txt file
people_categories <- c(
  "Ordinary people / average citizens / ordinary supporters (e.g., individuals as well as groups of citizens such as teachers, pensioners, students, caregivers, etc.)",
  "(Members of) minorities, e.g., refugees, LGBTQ+, people with disabilities, Jewish people, women (when women are explicitly labeled, depicted, or referred to as a minority)",
  "Illegal actors, criminals, offenders, terrorists, extremists / extremist groups. Actors who commit generally criminally relevant actions without being associated with a specific party, business, (non-terrorist) organization, or (non-extremist) social movement (please code them accordingly); actors who plan and carry out criminal activities in a structured group; actors who use violence",
  
  # CDA politicians
  "CDA: Henri Bontenbal", "CDA: Hanneke Steen", "Other CDA politicians",
  
  # GL-PvdA politicians  
  "GL-PvdA: Frans Timmermans", "GL-PvdA: Esmah Lahlah", "GL-PvdA: Jesse Klaver", 
  "GL-PvdA: Marjolein Moorman", "GL-PvdA: Mei Li Vos", "GL-PvdA: Femke Halsema", "Other GL-PvdA politicians",
  
  # PVV politicians
  "PVV: Geert Wilders", "PVV: Fleur Agema", "PVV: Dirk Beljaarts", "PVV: Barry Madlener", 
  "PVV: Marjolein Faber", "PVV: Reinette Klever", "PVV: Zsolt Szabó", "PVV: Vicky Maeijer", 
  "PVV: Chris Jansen", "PVV: Martin Bosma", "Other PVV politicians",
  
  # VVD politicians
  "VVD: Dilan Yeşilgöz", "VVD: Sophie Hermans", "VVD: Eelco Heinen", "VVD: Ruben Brekelmans", 
  "VVD: David van Weel", "VVD: Mariëlle Paul", "VVD: Vincent Karremans", "VVD: Jurgen Nobel", "Other VVD politicians",
  
  # D66 politicians
  "D66: Rob Jetten", "D66: Jan Paternotte", "D66: Hans Vijlbrief", "D66: Paul van Meenen", "Other D66 politicians",
  
  # BBB politicians
  "BBB: Caroline van der Plas", "BBB: Mona Keijzer", "BBB: Eddie van Marum", "BBB: Jean Rummenie", 
  "BBB: Gijs Tuinman", "BBB: Femke Wiersma", "Other BBB politicians",
  
  # SP politicians
  "SP: Jimmy Dijk", "Other SP politicians",
  
  # FvD politicians
  "FvD: Thierry Baudet", "FvD: Gideon van Meijeren", "Other FVD politicians",
  
  # CU politicians
  "CU: Mirjam Bikker", "CU: Pieter Grinwis", "Other CU politicians",
  
  # JA21 politicians
  "JA21: Joost Eerdmans", "JA21: Annabel Nanninga", "JA21: Ingrid Coenradie", "Other JA21 politicians",
  
  # Volt politicians
  "Volt: Laurens Dassen", "Volt: Marieke Koekkoek", "Other Volt politicians",
  
  # SGP politicians
  "SGP: Chris Stoffer", "Other SGP politicians",
  
  # Denk politicians
  "Denk: Stephan van Baarle", "Denk: Tunahan Kuzu", "Other Denk politicians",
  
  # NSC politicians
  "NSC: Eddy van Hijum", "NSC: Nicolien van Vroonhoven", "NSC: Eppo Bruins", "NSC: Tjebbe van Oostenbruggen", 
  "NSC: Teun Struycken", "NSC: Judith Uitermark", "NSC: Caspar Veldkamp", "NSC: Daniëlle Jansen", "Other NSC politicians",
  
  # PvdD politicians
  "PvdD: Esther Ouwehand", "PvdD: Ines Kostić", "Other PvdD politicians",
  
  # Other categories
  "Other politicians from Dutch parties / independent politicians",
  "Foreign politicians (ALL EXCEPT from EU, UN, NATO)",
  "Foreign politicians from EU, UN, NATO",
  "Representatives of non-political institutions (e.g., police, churches, military, other public institutions)",
  "Representatives of interest groups / NGOs (e.g., Greenpeace, Amnesty International, Doctors Without Borders, Reporters Without Borders, trade unions, employers' associations, lobbyists)",
  "Representatives of social movements (e.g., Fridays for Future, Identitarian Movement, QAnon, #MeToo movement)",
  "Entrepreneurs / business leaders (e.g., from VW, Porsche, Daimler, Amazon, Google, Facebook)",
  "Celebrities and/or opinion leaders (e.g., actors, sports figures/athletes, musicians, economists, social media influencers)",
  "Journalists, representatives of news media (general, media types, individual media outlets)",
  "Other persons"
)

cat("=== PEOPLE CATEGORIES DEBUG ===\n")
cat("Total people categories:", length(people_categories), "\n")
cat("Sample categories:\n")
print(head(people_categories, 5))
cat("================================\n\n")

# Function to check if a category is present in the text
check_category_presence <- function(text, category) {
  if (is.na(text) || text == "") return(0)
  # Use exact matching with fixed = TRUE to avoid regex issues
  as.integer(grepl(category, text, fixed = TRUE))
}

# Build binaries for people-like variables using predefined categories
expand_people_like <- function(d, var_label, colname) {
  # Use predefined categories instead of splitting on commas
  categories <- if (var_label == "People in the Image/Video") {
    people_categories
  } else if (var_label == "Names of Actors") {
    # For Names of Actors, we'll use a different approach - just split on commas
    # since these are actual names, not predefined categories
    all_names <- d[[colname]] |> 
      str_split(",") |> 
      unlist() |> 
      str_trim()
    all_names <- all_names[all_names != ""] |> unique()
    all_names
  } else {
    stop("Categories not defined for variable: ", var_label)
  }
  
  out <- map_dfc(categories, function(category) {
    key <- paste0(var_label, "__", gsub("[^a-zA-Z0-9]", "_", category), "_bin")
    binary_values <- map_int(d[[colname]], ~ check_category_presence(.x, category))
    tibble(!!key := binary_values)
  })
  bind_cols(out)
}

people_bins <- imap(cols_people, ~ expand_people_like(dat, .y, .x))

# =========================================
# Expand REGEX multi-labels to binaries
#  - for generic labels: tolerant regex via to_pattern()
#  - for party:person tokens: discover dynamically
# =========================================
# Pre-build patterns for generic labels per variable
patterns <- lapply(dict, function(lbls) setNames(vapply(lbls, to_pattern, character(1)), lbls))

detect_label <- function(text_vec, pat) {
  txt <- norm_txt(text_vec)
  str_detect(txt, regex(pat))
}

expand_regex_multi <- function(d, var_label, colname) {
  txt <- d[[colname]]
  bins_generic <- list()
  # generic codes
  for (lbl in names(patterns[[var_label]])) {
    pat <- patterns[[var_label]][[lbl]]
    key <- paste0(var_label, "__", lbl, "_bin")
    bins_generic[[key]] <- as.integer(detect_label(txt, pat))
  }
  # party:person discovery (e.g., "PVV: Geert Wilders"). We do NOT hard-code names.
  # capture tokens like "<PARTY> : <Anything up to delimiter/end>"
  party_regex <- paste0("(?i)\\b(", paste(parties, collapse="|"), ")\\s*:\\s*([^,;|]+)")
  matches <- str_match_all(txt, party_regex)
  discovered <- unique(unlist(lapply(matches, function(m) if (is.null(m)) character(0) else paste0(m[,2], ": ", str_squish(m[,3])) )))
  # Create binaries for each discovered "Party: Name"
  bins_people <- list()
  for (tok in discovered) {
    if (is.na(tok) || tok == "") next
    # tolerant version of exactly this token (not splitting it)
    pat_tok <- to_pattern(tok)
    key <- paste0(var_label, "__", tok, "_bin")
    bins_people[[key]] <- as.integer(detect_label(txt, pat_tok))
  }
  as_tibble(bins_generic) |>
    bind_cols(as_tibble(bins_people))
}

regex_bins <- imap(cols_multi, ~ expand_regex_multi(dat, .y, .x))

# =========================================
# Assemble clean binary dataset (for multi-select variables only)
# =========================================
bin_df <- bind_cols(
  dat[, c("coder","url")],
  !!!people_bins,
  !!!regex_bins
)

# =========================================
# Create separate dataset for single-label variables (keep original labels)
# =========================================
single_label_df <- bind_cols(
  dat[, c("coder","url")],
  dat[, cols_single]
)

# =========================================
# Create AI Image/Video (AI or not) binary variable
# =========================================
# Debug: Let's see what values we're actually getting
cat("=== AI Image/Video Debug ===\n")
debug_data <- single_label_df |>
  select(coder, url, `AI Image/Video`) |>
  filter(!is.na(`AI Image/Video`))
cat("Sample AI Image/Video values:\n")
print(head(debug_data, 10))
cat("All unique AI Image/Video values:\n")
unique_values <- unique(debug_data$`AI Image/Video`)
for(i in seq_along(unique_values)) {
  cat(sprintf("%d: '%s'\n", i, unique_values[i]))
}
cat("================================\n")

ai_binary <- single_label_df |>
  mutate(
    `AI Image/Video (AI or not)` = case_when(
      is.na(`AI Image/Video`) ~ 0,
      grepl("COMPLETELY AI-generated", `AI Image/Video`, fixed = TRUE) ~ 1,
      grepl("PARTLY AI-generated", `AI Image/Video`, fixed = TRUE) ~ 1,
      grepl("unsure whether.*AI-generated", `AI Image/Video`, fixed = TRUE) ~ 1,  # Treat unsure as AI present
      TRUE ~ 0
    )
  ) |>
  select(`AI Image/Video (AI or not)`)

# Debug: Let's see the binary values
cat("=== AI Binary Values Debug ===\n")
debug_binary <- single_label_df |>
  select(coder, url, `AI Image/Video`) |>
  mutate(
    `AI Image/Video (AI or not)` = case_when(
      is.na(`AI Image/Video`) ~ 0,
      grepl("COMPLETELY AI-generated", `AI Image/Video`, fixed = TRUE) ~ 1,
      grepl("PARTLY AI-generated", `AI Image/Video`, fixed = TRUE) ~ 1,
      grepl("unsure whether.*AI-generated", `AI Image/Video`, fixed = TRUE) ~ 1,
      TRUE ~ 0
    )
  ) |>
  filter(!is.na(`AI Image/Video`))
cat("Sample binary conversions:\n")
print(head(debug_binary, 10))

# Debug: Let's see specific examples with PARTLY AI-generated
cat("\n=== PARTLY AI-generated Examples ===\n")
partly_examples <- debug_binary |>
  filter(`AI Image/Video` == "The image/video is PARTLY AI-generated, meaning it contains both AI-generated and real or \"photoshopped\" content.")
cat("Examples with PARTLY AI-generated:\n")
print(partly_examples)

# Debug: Let's see all responses for a specific URL to understand disagreements
cat("\n=== URL-by-URL Analysis ===\n")
url_analysis <- debug_binary |>
  group_by(url) |>
  summarise(
    coders = paste(coder, collapse = ", "),
    responses = paste(`AI Image/Video`, collapse = " | "),
    binary_values = paste(`AI Image/Video (AI or not)`, collapse = ", "),
    n_unique_binary = length(unique(`AI Image/Video (AI or not)`)),
    .groups = "drop"
  ) |>
  filter(n_unique_binary > 1)  # Only show URLs with disagreements
cat("URLs with disagreements in AI binary values:\n")
print(head(url_analysis, 5))

# Debug: Let's check the specific URL you mentioned
target_url <- "https://www.facebook.com/GeenAangifte/posts/pfbid026Xwjp4kP41c6VfB53j3LXaAwqf1hz25ngbkEsqzPkmJ5BRnZvjL2qMRyeLVbZ91Pl"
cat("\n=== Specific URL Debug ===\n")
specific_url_debug <- debug_binary |>
  filter(url == target_url)
cat("Responses for the specific URL:\n")
print(specific_url_debug)

# Debug: Let's check if PARTLY responses are being found
cat("\n=== PARTLY Response Check ===\n")
partly_check <- debug_binary |>
  filter(grepl("PARTLY", `AI Image/Video`, fixed = TRUE))
cat("All PARTLY responses found:\n")
print(partly_check)

cat("================================\n")

# Add AI binary variable to the binary dataset
bin_df <- bind_cols(
  bin_df,
  ai_binary
)

# =========================================
# Split Topics variable into individual binary variables
# =========================================
topics_categories <- c(
  "Election in general (electoral system, campaign events, campaign dates, campaign appearances, election polls)",
  "Political parties and politicians (candidate presentation; politicians as the main subject)",
  "Call to vote / get-out-the-vote call (WITHOUT AND WITH NAMING OF PARTY(S)",
  "Economy and finance",
  "Labor and social issues",
  "Domestic and migration policy (public safety, crime (incl. femicide), political radicalism, religious fanaticism, terrorism)",
  "Environmental and energy policy",
  "Foreign, security, and defense policy",
  "Housing (Crisis)",
  "Digital and Infrastructure",
  "Europe",
  "Education and culture",
  "Constitution of the Political System (e.g. independence of the judiciary, rule of law, parliamentary representation of the will of the people)",
  "Civil rights (including gender politics, LGBTQ+ policies)",
  "Other political topic"
)

# Create binary variables for each topic
topics_bins <- map(topics_categories, ~ {
  topic_name <- .x
  bin_name <- paste0("Topics__", janitor::make_clean_names(topic_name), "_bin")
  
  # Check if the topic appears in the Topics column (comma-separated)
  bins <- single_label_df |>
    mutate(
      !!bin_name := ifelse(
        is.na(Topics), 0,
        ifelse(grepl(topic_name, Topics, fixed = TRUE), 1, 0)
      )
    ) |>
    select(!!bin_name)
  
  return(bins)
}) |>
  bind_cols()

# Add topics binary variables to the binary dataset
bin_df <- bind_cols(
  bin_df,
  topics_bins
)

# Remove Topics from single_label_df since it's now split into binary variables
single_label_df <- single_label_df |> select(-Topics)

# =========================================
# Disagreement summary (same logic you asked for)
#  - For each variable/code: build wide (rows=url, cols=coder)
#  - modal value vs. coder values -> n_disagree; if 1, record coder
# =========================================
# list of all binary columns
bin_cols <- setdiff(names(bin_df), c("coder","url"))

# function: per binary code, compute disagreements per URL
disagree_per_code <- function(df, code_col) {
  wide <- df |>
    select(url, coder, !!code_col) |>
    # Handle any remaining duplicates by taking the first value
    group_by(url, coder) |>
    summarise(!!code_col := first(!!sym(code_col)), .groups = "drop") |>
    pivot_wider(names_from = coder, values_from = all_of(code_col)) |>
    arrange(url)
  coder_cols <- setdiff(names(wide), "url")
  
  # Replace NA values with 0 (not present) for disagreement analysis
  wide[coder_cols] <- lapply(wide[coder_cols], function(x) ifelse(is.na(x), 0, x))
  
  # Process each row
  results <- list()
  for (i in 1:nrow(wide)) {
    row <- wide[i, ]
    url_val <- row[["url"]]
    vals <- unlist(row[coder_cols], use.names = FALSE)
    keep <- !is.na(vals)
    vals <- vals[keep]
    n_coders <- length(vals)
    
    if (n_coders < 2) {
      results[[i]] <- tibble(url = url_val, code = code_col, n_coders = n_coders,
                             modal_value = NA_integer_, n_disagree = NA_integer_,
                             disagree_coder = NA_character_, disagree_value = NA_integer_)
    } else {
      tab <- table(vals)
      modal <- as.integer(names(tab)[which.max(tab)])
      n_dis <- n_coders - max(tab)
      disagree_idx <- which(unlist(row[coder_cols])[keep] != modal)
      who <- if (n_dis == 1 && length(disagree_idx) == 1) coder_cols[keep][disagree_idx] else NA_character_
      what <- if (n_dis == 1 && length(disagree_idx) == 1) vals[disagree_idx] else NA_integer_
      results[[i]] <- tibble(url = url_val, code = code_col, n_coders = n_coders,
                             modal_value = modal, n_disagree = n_dis,
                             disagree_coder = who, disagree_value = what)
    }
  }
  
  bind_rows(results)
}

disagree_long <- map_dfr(bin_cols, ~ disagree_per_code(bin_df, .x))

# Summaries per code
code_summary <- disagree_long |>
  group_by(code) |>
  summarise(
    n_units = sum(!is.na(n_disagree)),
    units_with_any_disagreement = sum(n_disagree > 0, na.rm = TRUE),
    units_with_exactly_one_disagreement = sum(n_disagree == 1, na.rm = TRUE),
    prop_with_any_disagreement = mean(n_disagree > 0, na.rm = TRUE),
    prop_with_exactly_one_disagreement = mean(n_disagree == 1, na.rm = TRUE),
    mean_n_disagree = mean(n_disagree, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(prop_with_any_disagreement), desc(mean_n_disagree))

# =========================================
# Write outputs
# =========================================
dir.create("reliability_out", showWarnings = FALSE)
readr::write_csv(bin_df,        "reliability_out/binary_codes_clean.csv")
readr::write_csv(disagree_long, "reliability_out/per_url_disagreements_binary.csv")
readr::write_csv(code_summary,  "reliability_out/code_disagreement_summary.csv")

cli::cli_alert_success("Wrote: reliability_out/binary_codes_clean.csv")
cli::cli_alert_success("Wrote: reliability_out/per_url_disagreements_binary.csv")
cli::cli_alert_success("Wrote: reliability_out/code_disagreement_summary.csv")

# Quick peek: top 20 most disagreed codes
print(head(code_summary, 20))

# =========================================
# Comprehensive Intercoder Reliability Analysis
# =========================================

# Function to calculate reliability statistics for a single variable
calculate_reliability_stats <- function(df, code_col) {
  # Create wide format for reliability analysis
  wide <- df |>
    select(url, coder, !!code_col) |>
    # Handle any remaining duplicates by taking the first value
    group_by(url, coder) |>
    summarise(!!code_col := first(!!sym(code_col)), .groups = "drop") |>
    pivot_wider(names_from = coder, values_from = all_of(code_col)) |>
    arrange(url)
  
  coder_cols <- setdiff(names(wide), "url")
  
  # Replace NA values with 0 (not present) instead of removing rows
  wide_clean <- wide
  wide_clean[coder_cols] <- lapply(wide_clean[coder_cols], function(x) ifelse(is.na(x), 0, x))
  
  if (nrow(wide_clean) == 0) {
    return(tibble(
      code = code_col,
      n_units = 0,
      n_coders = length(coder_cols),
      percent_agreement = NA_real_,
      krippendorff_alpha = NA_real_,
      cohen_kappa = NA_real_,
      fleiss_kappa = NA_real_,
      scott_pi = NA_real_,
      status = "No valid data"
    ))
  }
  
  # Extract data matrix for reliability calculations
  data_matrix <- as.matrix(wide_clean[coder_cols])
  
  # Calculate basic statistics
  n_units <- nrow(wide_clean)
  n_coders <- ncol(data_matrix)
  
  # Percent Agreement
  percent_agreement <- mean(apply(data_matrix, 1, function(x) {
    non_na <- x[!is.na(x)]
    if (length(unique(non_na)) <= 1) return(1) else return(0)
  }), na.rm = TRUE)
  
  # Try to calculate various reliability statistics
  reliability_stats <- list()
  
  # Krippendorff's Alpha
  tryCatch({
    if (ncol(data_matrix) >= 2 && nrow(data_matrix) > 0) {
      # Convert to numeric first
      data_matrix <- apply(data_matrix, 2, as.numeric)
      
      # Remove rows with all NAs
      clean_matrix <- data_matrix[!apply(data_matrix, 1, function(x) all(is.na(x))), , drop = FALSE]
      
      if (nrow(clean_matrix) > 0 && ncol(clean_matrix) >= 2) {
        # Check if there's sufficient variation
        all_values <- as.vector(clean_matrix[!is.na(clean_matrix)])
        if (length(unique(all_values)) > 1) {
          # Try different methods
          tryCatch({
            reliability_stats$krippendorff_alpha <- irr::kripp.alpha(t(clean_matrix), method = "nominal")$value
          }, error = function(e1) {
            tryCatch({
              reliability_stats$krippendorff_alpha <<- irr::kripp.alpha(t(clean_matrix), method = "ordinal")$value
            }, error = function(e2) {
              reliability_stats$krippendorff_alpha <<- NA_real_
            })
          })
        } else {
          reliability_stats$krippendorff_alpha <- NA_real_
        }
      } else {
        reliability_stats$krippendorff_alpha <- NA_real_
      }
    } else {
      reliability_stats$krippendorff_alpha <- NA_real_
    }
  }, error = function(e) {
    reliability_stats$krippendorff_alpha <<- NA_real_
  })
  
  # Cohen's Kappa (for 2 coders)
  tryCatch({
    if (ncol(data_matrix) == 2) {
      # Remove rows where either coder has NA
      clean_data <- data_matrix[complete.cases(data_matrix), , drop = FALSE]
      if (nrow(clean_data) > 0) {
        reliability_stats$cohen_kappa <- irr::kappa2(clean_data)$value
      } else {
        reliability_stats$cohen_kappa <- NA_real_
      }
    } else {
      reliability_stats$cohen_kappa <- NA_real_
    }
  }, error = function(e) {
    reliability_stats$cohen_kappa <<- NA_real_
  })
  
  # Fleiss' Kappa (for multiple coders)
  tryCatch({
    if (ncol(data_matrix) > 2) {
      # Convert to factor with all possible levels
      all_values <- unique(as.vector(data_matrix[!is.na(data_matrix)]))
      data_factors <- apply(data_matrix, 2, function(x) factor(x, levels = all_values))
      reliability_stats$fleiss_kappa <- irr::kappam.fleiss(data_factors)$value
    } else {
      reliability_stats$fleiss_kappa <- NA_real_
    }
  }, error = function(e) {
    reliability_stats$fleiss_kappa <<- NA_real_
  })
  
  # Scott's Pi
  tryCatch({
    if (ncol(data_matrix) == 2) {
      clean_data <- data_matrix[complete.cases(data_matrix), , drop = FALSE]
      if (nrow(clean_data) > 0) {
        reliability_stats$scott_pi <- irr::kappam.fleiss(clean_data)$value
      } else {
        reliability_stats$scott_pi <- NA_real_
      }
    } else {
      reliability_stats$scott_pi <- NA_real_
    }
  }, error = function(e) {
    reliability_stats$scott_pi <<- NA_real_
  })
  
  # Determine status
  status <- "Calculated"
  if (is.na(reliability_stats$krippendorff_alpha) && is.na(reliability_stats$cohen_kappa) && 
      is.na(reliability_stats$fleiss_kappa)) {
    status <- "Unable to calculate"
  }
  
  return(tibble(
    code = code_col,
    n_units = n_units,
    n_coders = n_coders,
    percent_agreement = percent_agreement,
    krippendorff_alpha = reliability_stats$krippendorff_alpha,
    cohen_kappa = reliability_stats$cohen_kappa,
    fleiss_kappa = reliability_stats$fleiss_kappa,
    scott_pi = reliability_stats$scott_pi,
    status = status
  ))
}

# Calculate reliability statistics for multi-select binary codes
cli::cli_alert_info("Calculating reliability statistics for multi-select binary codes...")
reliability_stats_binary <- map_dfr(bin_cols, ~ calculate_reliability_stats(bin_df, .x))

# Calculate reliability statistics for single-label variables (using agreement only)
cli::cli_alert_info("Calculating reliability statistics for single-label variables...")
single_cols <- setdiff(names(single_label_df), c("coder","url"))
reliability_stats_single <- map_dfr(single_cols, ~ {
  # For single-label variables, calculate percent agreement
  wide <- single_label_df |>
    select(url, coder, !!sym(.x)) |>
    group_by(url, coder) |>
    summarise(!!sym(.x) := first(!!sym(.x)), .groups = "drop") |>
    pivot_wider(names_from = coder, values_from = all_of(.x)) |>
    arrange(url)
  
  coder_cols <- setdiff(names(wide), "url")
  n_units <- nrow(wide)
  
  if (n_units == 0) {
    return(tibble(code = .x, n_units = 0, percent_agreement = NA, krippendorff_alpha = NA, 
                  krippendorff_interpretation = "Unable to calculate", status = "No data"))
  }
  
  # Calculate percent agreement
  agreements <- 0
  for (i in 1:n_units) {
    values <- wide[i, coder_cols] |> unlist() |> na.omit()
    if (length(unique(values)) <= 1) agreements <- agreements + 1
  }
  percent_agreement <- agreements / n_units
  
  # Krippendorff's alpha not applicable for single-label categorical variables
  krippendorff_alpha <- NA
  krippendorff_interpretation <- "Not applicable for categorical"
  
  # Status based on percent agreement
  status <- case_when(
    percent_agreement >= 0.8 ~ "Excellent",
    percent_agreement >= 0.6 ~ "Good", 
    percent_agreement >= 0.4 ~ "Moderate",
    TRUE ~ "Poor"
  )
  
  tibble(
    code = .x,
    n_units = n_units,
    percent_agreement = percent_agreement,
    krippendorff_alpha = krippendorff_alpha,
    krippendorff_interpretation = krippendorff_interpretation,
    status = status
  )
})

# Combine both reliability statistics
reliability_stats <- bind_rows(
  reliability_stats_binary,
  reliability_stats_single
)

# Add interpretation of reliability levels
reliability_stats <- reliability_stats |>
  mutate(
    krippendorff_interpretation = case_when(
      is.na(krippendorff_alpha) ~ "Unable to calculate",
      krippendorff_alpha >= 0.8 ~ "Excellent",
      krippendorff_alpha >= 0.67 ~ "Good", 
      krippendorff_alpha >= 0.4 ~ "Moderate",
      krippendorff_alpha >= 0.2 ~ "Poor",
      TRUE ~ "Very Poor"
    ),
    kappa_interpretation = case_when(
      is.na(cohen_kappa) & is.na(fleiss_kappa) ~ "Unable to calculate",
      coalesce(cohen_kappa, fleiss_kappa) >= 0.8 ~ "Excellent",
      coalesce(cohen_kappa, fleiss_kappa) >= 0.6 ~ "Good",
      coalesce(cohen_kappa, fleiss_kappa) >= 0.4 ~ "Moderate", 
      coalesce(cohen_kappa, fleiss_kappa) >= 0.2 ~ "Poor",
      TRUE ~ "Very Poor"
    )
  )

# Create summary statistics
overall_summary <- reliability_stats |>
  summarise(
    total_codes = n(),
    codes_with_data = sum(n_units > 0),
    codes_excellent_kripp = sum(krippendorff_interpretation == "Excellent", na.rm = TRUE),
    codes_good_kripp = sum(krippendorff_interpretation == "Good", na.rm = TRUE),
    codes_moderate_kripp = sum(krippendorff_interpretation == "Moderate", na.rm = TRUE),
    codes_poor_kripp = sum(krippendorff_interpretation %in% c("Poor", "Very Poor"), na.rm = TRUE),
    mean_percent_agreement = mean(percent_agreement, na.rm = TRUE),
    mean_krippendorff_alpha = mean(krippendorff_alpha, na.rm = TRUE),
    median_krippendorff_alpha = median(krippendorff_alpha, na.rm = TRUE)
  )

# Identify most problematic codes
problematic_codes <- reliability_stats |>
  filter(n_units > 0) |>
  arrange(percent_agreement, krippendorff_alpha) |>
  head(20)

# =========================================
# Write additional outputs
# =========================================
readr::write_csv(reliability_stats, "reliability_out/reliability_statistics.csv")
readr::write_csv(overall_summary, "reliability_out/overall_reliability_summary.csv")
readr::write_csv(problematic_codes, "reliability_out/most_problematic_codes.csv")

cli::cli_alert_success("Wrote: reliability_out/reliability_statistics.csv")
cli::cli_alert_success("Wrote: reliability_out/overall_reliability_summary.csv") 
cli::cli_alert_success("Wrote: reliability_out/most_problematic_codes.csv")

# =========================================
# Display Results
# =========================================
cli::cli_h1("Intercoder Reliability Analysis Results")

cli::cli_h2("Overall Summary")
print(overall_summary)

cli::cli_h2("Top 20 Most Problematic Codes (Lowest Reliability)")
print(problematic_codes)

cli::cli_h2("Reliability Statistics Summary")
print(reliability_stats |> 
  select(code, n_units, percent_agreement, krippendorff_alpha, krippendorff_interpretation) |>
  arrange(percent_agreement) |>
  head(20))

cli::cli_h2("Codes with Excellent Reliability (Krippendorff's Alpha >= 0.8)")
excellent_codes <- reliability_stats |>
  filter(krippendorff_interpretation == "Excellent") |>
  select(code, n_units, percent_agreement, krippendorff_alpha)
if (nrow(excellent_codes) > 0) {
  print(excellent_codes)
} else {
  cli::cli_alert_warning("No codes achieved excellent reliability")
}

# =========================================
# Multi-Select Variable Reliability Analysis
# =========================================
cli::cli_h1("Multi-Select Variable Reliability Analysis")

# Calculate set-based reliability for multi-select variables
multi_select_vars <- c(vars_people_like, vars_multi_regex)
set_reliability <- map_dfr(multi_select_vars, ~ calculate_set_reliability(dat, .x))

cli::cli_h2("Set-Based Reliability (Jaccard Similarity)")
print(set_reliability)

# Write set-based reliability results
readr::write_csv(set_reliability, "reliability_out/set_based_reliability.csv")
cli::cli_alert_success("Wrote: reliability_out/set_based_reliability.csv")
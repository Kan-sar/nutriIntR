normalize_unit <- function(unit) {
  unit <- tolower(trimws(unit))
  unit[unit %in% c("mcg", "\u00b5g", "\u03bcg")] <- "ug"
  unit[!nzchar(unit)] <- "unitless"
  unit
}

nutrient_catalog <- function() {
  catalog <- utils::read.csv(engine_file("nutrient.csv"), stringsAsFactors = FALSE,
    colClasses = "character")
  catalog$id <- as.integer(catalog$id)
  catalog$rank <- as.numeric(catalog$rank)
  catalog$key <- paste0("usda_", catalog$id)
  aliases <- c(`1089` = "iron", `1087` = "calcium", `1162` = "vitamin_c",
               `1095` = "zinc", `1098` = "copper")
  matched <- as.character(catalog$id) %in% names(aliases)
  catalog$key[matched] <- unname(aliases[as.character(catalog$id[matched])])
  catalog$canonical_unit <- normalize_unit(catalog$unit_name)
  catalog$unit_family <- ifelse(catalog$canonical_unit %in% c("g", "mg", "ug"), "mass",
    ifelse(catalog$canonical_unit %in% c("kcal", "kj"), "energy", "native_measure"))
  catalog$evidence_key <- catalog$key

  catalog$evidence_key[catalog$id %in% c(1141L, 1142L)] <- "iron"
  catalog$evidence_key[catalog$id == 1163L] <- "vitamin_c"
  catalog$default_form <- "unspecified"
  catalog$default_form[catalog$id == 1141L] <- "heme"
  catalog$default_form[catalog$id == 1142L] <- "non_heme_food"
  catalog$default_form[catalog$id %in% c(1162L, 1163L)] <- "ascorbic_acid"
  catalog$default_form[catalog$id == 1042L] <- "phytic_acid"
  catalog$origin <- "USDA"
  catalog$definition_status <- ifelse(grepl("Archived|DO NOT USE", catalog$name, ignore.case = TRUE),
    "archived_in_source", "listed")
  rownames(catalog) <- NULL
  catalog
}

resolve_nutrient <- function(value, catalog) {
  key <- tolower(trimws(value))
  aliases <- c(ascorbic_acid = "vitamin_c", fe = "iron", ca = "calcium",
               zn = "zinc", cu = "copper")
  compact <- gsub("[ -]", "_", key)
  if (compact %in% names(aliases)) compact <- unname(aliases[compact])
  id <- sub("^usda[_:]", "", key)
  found <- which(catalog$key == compact | as.character(catalog$id) == id)
  if (!length(found)) found <- which(tolower(catalog$name) == key)
  if (length(found) > 1L) stop("Ambiguous USDA nutrient name: ", value, ". Select its USDA ID.")
  if (length(found)) return(catalog[found, , drop = FALSE])

  data.frame(id = NA_integer_, name = if (compact == "phytate") "Phytate phosphorus (study extension)" else value,
    unit_name = "MG", nutrient_nbr = NA_character_, rank = NA_real_, key = compact,
    canonical_unit = "mg", unit_family = "mass", evidence_key = compact,
    default_form = "unspecified", origin = "extension", definition_status = "extension", stringsAsFactors = FALSE)
}

nutrient_units <- function(unit, family) {
  if (family == "mass") return(c("mg", "g", "ug", "mcg"))
  if (family == "energy") return(c(unit, setdiff(c("kcal", "kj"), unit)))
  unit
}

nutrient_forms <- function(key, default = "unspecified") {
  known <- list(iron = c("unspecified", "non_heme_food", "ferrous_sulfate", "supplement", "heme"),
    usda_1141 = "heme", usda_1142 = c("non_heme_food", "unspecified"),
    vitamin_c = c("ascorbic_acid", "food", "unspecified"),
    usda_1163 = c("ascorbic_acid", "food", "unspecified"),
    calcium = c("unspecified", "food", "calcium_carbonate", "calcium_citrate", "calcium_phosphate"),
    phytate = c("sodium_phytate_phosphorus", "phytic_acid"), usda_1042 = "phytic_acid")
  if (key %in% names(known)) return(known[[key]])
  unique(c(default, "unspecified", "food", "supplement"))
}

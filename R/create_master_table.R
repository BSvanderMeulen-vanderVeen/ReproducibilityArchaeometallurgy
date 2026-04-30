#' Create master table from Crossref metadata records
#'
#' Searches crossref for a specified period and downloads metadata for articles
#' published in the specified journals up to the defined maximum number of
#' records. It then applies a series of filters to narrow the dataset down to
#' articles relevant to the scope of this study, anonymises them and adds
#' columns for the stages of the reproducibility index that were then filled
#' with our observations.
#'
#' Before running the function, please add the mail address to be shared with
#' Crossref by adding `crossref_email = your@email.com` to your `.Renviron`
#' file. See chapter "Be nice and share your email with Crossref" in
#' [rcrossref::rcrossref] for details.
#'
#' The filter terms were compiled iteratively against the downloaded metadata
#' records. You can see them in the source code of the function, which can be
#' asses with `View(create_master_table)`.
#'
#' @param start_date,end_date The period for with publications should be
#'   searched. The default values indicate the period used for the dataset in
#'   this compendium.
#' @param journal_issn Vector with the ISSNs of the journals included in the
#'   search.
#'
#' @returns A csv with `;`as separator, with the anonymised article ID and the columns
#'   for the reproducibility index to be populated.
#' @export
#'

create_master_table <- function(start_date = "2024-11-25", end_date = "2026-01-31",
                                journal_issn = source("./analysis/supplementary-materials/journals.R")
){

  results <- rcrossref::cr_journals(issn = journal_issn,
                                    works = TRUE,
                                    filter = c(from_pub_date = start_date,
                                               until_pub_date = end_date),
                                    limit = 1000,
                                    async = TRUE
  )

  # Process raw data:
  #   * Put data into data frame
  #   * Remove rows where title is missing
  #   * Only select columns: date of deposition, journal title, article title, ISSN, DOI, abstract
  # Date of "deposited" is chosen instead of "created", as the latter sometimes falls outside of chosen time frame
  results_data <- results$data[!is.na(results$data$title),
                               c("deposited", "container.title", "title", "issn",
                                 "doi", "abstract")]

  # This should result in 1494 observations

  # Filter papers related to metallurgy based on search terms in article title and abstract.
  # Filter includes paper if any column yields any of the search terms to get around lack of abstracts.
  pattern_metal <- paste(

    # general search terms
    "charact", "provenance", "metal", "trace", "element", "isotop", "composition",

    # specific materials or alloys
    "alloy", "bronze", "brass", "gunmetal", "copper", "\\btin\\b",
    "zinc", "\\blead\\b", "\\biron\\b", "silver", "gold",
    sep = "|")

  # Create a vector that selects rows passing the filter and trim dataset accordingly
 results_filtered1 <- subset(results_data, 
                              grepl(pattern_metal, 
                                    title, 
                                    ignore.case = TRUE) | 
    grepl(pattern_metal, 
          abstract, 
          ignore.case = TRUE)
  )

  # This should result in 575 observations

  # Explicitly filter out non-metal materials.
  # The list of terms was compiled iteratively against the dataset
  pattern_non_metal <- paste(

    # search terms related to environmental archaeology
    "soil", "shell", "wood", "charcoal", "bone", "leather", "\\boil\\b", "fibre", "fiber", "cloth", "silk", "textile", "fabric\\b", "amber", "crop", "agropastoral",

    # search terms related to mineralogy
    "cinnabar", "malachite",

    # search terms related to stable isotopes
    "strontium", "\\bSr", "86Sr", "carbon", "oxygen",

    # search terms related to ceramic artefacts
    "petrological", "seal", "amphora", "clay", "pots", "wheel-made", "ceramic", "potter", "porcelain", "cooking", "brick", "paste",

    # search terms related to glass artefacts
    "faience", "glaze", "vitr", "beads",

    # search terms related to stone artefacts
    "volcanic", "graphite", "obsidian", "amazonite", "radiolarite", "flint", "chert", "dolerite", "stone", "turquoise", "lime", "steatite", "\\blithic", "-lithic", "rock", "ochre", "marble", "quern", "carnelian",

    # search terms related to plant and animal remains and diet
    "cranial", "subsistence", "skelet", "weaning", "species", "camel", "horse", "sheep", "pig", "mice", "fish", "animal", "bovi", "\\bcani", "bio", "human", "neanderthal", "dna", "plant", "food", "diet", "tooth", "dentin", "enamel",

    # search terms related to architecture and building materials
    "architect", "plaster", "paint", "mortar", "inscription", "structure",
    sep = "|")

  # Create a vector that selects rows not passing the filter and trim dataset accordingly
  results_filtered2 <- subset(results_filtered1, 
                                !grepl(pattern_non_metal, 
                           title, 
                           ignore.case = TRUE) & 
    !grepl(pattern_non_metal, 
          abstract, 
          ignore.case = TRUE)
  )

  # This should results in 182  results

  # Apply filtering mask to ensure multi-material approaches are kept in
  # The list of terms was compiled iteratively against the dataset
  pattern_non_metal2 <- paste("water", "glass", sept = "|")

  metals <- paste("metal", "alloy", "bronze", "brass", "gunmetal", "copper", "\\btin\\b",
                  "zinc", "\\blead\\b", "\\biron\\b", "silver", "gold",
                  sep = "|") # set filter mask terms

  # Create a vector that selects rows passing the filter and trim dataset accordingly
  filtering_non_metal2 <- apply(results_filtered2[, c("title", "abstract")], 1, function(row) {
    has_neg <- any(grepl(pattern_neg_mat2, row, ignore.case = TRUE))
    has_metal <- any(grepl(metals, row, ignore.case = TRUE))
    return(!has_neg | has_metal)
  })

  results_filtered3 <- results_filtered3[filtering_non_metal2, ]

  # This should result in 154 observations

  # Explicitly filter out non-metallurgical methodologies
  # The list of terms was compiled iteratively against the dataset
  pattern_method <- paste(

    # general terms
    "typological", "experiment", "geometric",

    # methodologies related to environmental and landscape studies
    "settlement pattern", "magnetic", "climat", "micromorpho", "geospatial", "lidar",

    # methodologies related to human and animal mobility, health, diet and death
    "demograph", "organic", "botan", "patholog", "genome", "funerary",
    sep = "|")

  # Create a vector that selects rows not passing the filter and trim dataset accordingly
   results_filtered4 <- subset(results_filtered3, 
                             !grepl(pattern_method, 
                                title, 
                                ignore.case = TRUE) & 
    !grepl(pattern_method, 
           abstract, 
           ignore.case = TRUE)
  )

  # This should result in 139 observations

  # Filter out publications other than research papers (reviews, responses, comments, editorials) in article title
  results_filtered5 <- subset(results_filtered4, !grepl(
    "\\(ed\\.\\)\\.|hardback|reply|£|€|\\$|corrigendum|comment|editorial|response|review\\b",
    results_filtered$title, ignore.case = TRUE)
  )

  # This should result in 133 observations

  ## Manual identification and exclusion of additional papers as these concern metallographic analysis only, soil or botanical data or pertain to regional overviews/re-evaluations of legacy data (with no original metallurgical data produced) or metallurgical experiments with little focus on compositional analysis (or only analysis of replicas)
  results_final  <- subset(
    results_filtered5, !grepl(
      "10.1111/arcm.70040|10.54841/hm.669|10.1016/j.jas.2025.106364|10.1016/j.jas.2025.106212|10.1016/j.jas.2024.106135|10.1016/j.jas.2024.106093|10.1007/s12520-025-02378-1|10.1007/s10816-025-09694-9|10.1016/j.jasrep.2024.104811|10.1016/j.jasrep.2025.105390|10.1007/s12520-025-02327-y|10.1016/j.jasrep.2025.105455|10.1016/j.jas.2025.106154|10.1016/j.jas.2025.106420|10.1016/j.jas.2025.106382|10.1016/j.jas.2025.106272|
      10.54841/hm.670|10.54841/hm.674|10.1016/j.jas.2025.106355|10.1007/s12520-025-02318-z|10.1016/j.jasrep.2025.105379|10.54841/hm.670|10.1016/j.jasrep.2025.105333|10.1016/j.jas.2025.106168|10.1016/j.jas.2025.106177|10.1016/j.jas.2025.106418|10.1016/j.jas.2025.106243|
      10.1016/j.jasrep.2025.105333|10.1016/j.jasrep.2024.104850|10.1111/arcm.13056|10.1111/arcm.70006|10.1016/j.jasrep.2025.105068|10.1016/j.jasrep.2024.104801|10.1111/arcm.13088|10.1007/s10963-025-09199-z|10.1016/j.jasrep.2025.105268|10.1016/j.jasrep.2025.105114",
      results_filtered$doi)
  )

  # This should result in 99 observations

  # Anonymise publications, assigning randomly generated ID number and adding columns for data entry

  # Set seed for reproducibility and add column 'ID' with random numbers to dataset
  set.seed(42)

  results_final$ID <- sample(nrow(results_final), replace = FALSE)

  # Define columns with identifying information and remove them from dataset
  columns_remove <- c("doi", "deposited", "container.title", "title", "issn", "abstract")
  results_anon <- results_final[, !names(results_final) %in% columns_remove]

  # Add columns for data entry (12 stages of reproducibility index and additional comments)
  columns_add <- c("Stage_0", "Comments_0",
                   "Stage_1", "Comments_1",
                   "Stage_2", "Comments_2",
                   "Stage_3", "Comments_3",
                   "Stage_4", "Comments_4",
                   "Stage_5", "Comments_5",
                   "Stage_6", "Comments_6",
                   "Stage_7", "Comments_7",
                   "Stage_8", "Comments_8",
                   "Stage_9", "Comments_9",
                   "Stage_10", "Comments_10",
                   "Stage_11", "Comments_11",
                   "Stage_12", "Comments_12"
                  )

  results_anon[, columns_add] <- NA

  # Export anonymised data frame as .csv (; as separator)
  utils::write.csv2(results_anon, "reprod_astr_anonymised.csv", na = "", row.names = FALSE)
}

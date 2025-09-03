# Load necessary library
library(dplyr)

# Define the function classify_gene
classify_gene <- function(logFC, padj) {
  if (logFC > 1 && padj < 0.05) {
    return("Upregulated")
  } else if (logFC < -1 && padj < 0.05) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}

# Create a directory for results if it doesn't exist
if (!dir.exists("Results")) {
  dir.create("Results")
}

# List of files with GitHub raw links
files <- c(
  "https://raw.githubusercontent.com/AI-Biotechnology-Bioinformatics/AI_and_Omics_Research_Internship_2025/main/DEGs_Data_1.csv",
  "https://raw.githubusercontent.com/AI-Biotechnology-Bioinformatics/AI_and_Omics_Research_Internship_2025/main/DEGs_Data_2.csv"
)

# Process each file
for (file in files) {
  # Load the dataset with error handling
  data <- tryCatch({
    read.csv(file, check.names = FALSE)
  }, error = function(e) {
    cat("Error reading", file, ":", e$message, "\n")
    next
  })
  
  # Ensure the necessary columns exist
  if (!all(c("logFC", "padj") %in% names(data))) {
    cat("Missing columns in", file, "\n")
    next
  }
  
  # Replace missing padj values with 1
  data$padj[is.na(data$padj)] <- 1
  
  # Apply classify_gene function to create a new column 'status'
  data <- data %>%
    mutate(status = mapply(classify_gene, logFC, padj))
  
  # Extract filename for saving
  file_name <- basename(file)
  output_file <- file.path("Results", paste0("processed_", file_name))
  
  # Save the processed data to the Results folder
  write.csv(data, output_file, row.names = FALSE)
  
  # Print summary counts
  summary_counts <- table(data$status)
  cat("Summary for", file_name, ":\n")
  print(summary_counts)
  cat("\n")
}
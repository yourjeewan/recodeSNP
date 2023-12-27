recode <- function(df, ploidy) {
  if (ploidy != 2 && ploidy != 4) {
    stop("Invalid ploidy. Please provide ploidy as 2 or 4.")
  }

  replacement_values <- if (ploidy == 2) {
    c("AA", "AB", "BB")
  } else {
    c("AAAA", "AAAB", "AABB", "ABBB", "BBBB")
  }

  recode_column <- function(x) {
    ifelse(x == 0, replacement_values[1],
           ifelse(x == 1, replacement_values[2],
                  ifelse(x == 2, replacement_values[3],
                         ifelse(x == 3, replacement_values[4],
                                ifelse(x == 4, replacement_values[5], as.character(x))))))
  }

  if ("id" %in% names(df)) {
    df$id <- as.character(df$id)
  } else {
    df$id <- seq_len(nrow(df))  # Add id column
  }

  df <- as.data.frame(lapply(df, recode_column))
  return(df)
}

# Example usage with existing "id" column
df_with_id <- data.frame(
  id = c("TX-1", "TX-2", "TX-3", "TX-4"),
  M_1 = c(1, 3, 4, 2),
  M_2 = c(1, 3, 3, 2),
  M_3 = c(1, 3, 3, 1)
)

df_numeric_with_id <- recode(df_with_id, ploidy = 4)
print(df_numeric_with_id)



# Sample data with "chr" and "pos" columns
df_with_chr_pos <- data.frame(
  id = c("TX-1", "TX-2", "TX-3", "TX-4"),
  chr = c("chr1", "chr2", "chr1", "chr2"),
  pos = c(100, 150, 200, 250),
  m_1 = c(1, 3, 4, 2),
  m_2 = c(1, 3, 3, 2),
  m_3 = c(1, 3, 3, 1),
  m_4 = c(1, 3, 3, 2)
)

# Recode the SNP data
df_numeric_with_chr_pos <- recode(df_with_chr_pos, ploidy = 4)

# Print the result
print(df_numeric_with_chr_pos)

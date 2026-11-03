# code/functions/fix_address.R (formerly texas/FUN_fix_address.R)
fix_address <- function(x) {
  # Directions
  x <- gsub("SOUTH", "S", x)
  x <- gsub("WEST", "W", x)
  x <- gsub("EAST", "E", x)
  x <- gsub("NORTH", "N", x)
  
  #address components
  x <- gsub("BOULEVARD", "BLVD", x)
  x <- gsub("ROAD", "RD", x)
  x <- gsub("HIGHWAY", "HWY", x)
  x <- gsub("DRIVE", "DR", x)
  x <- gsub("STREET", "ST", x)
  x <- gsub("AVENUE", "AVE", x)
  x <- gsub("LANE", "LN", x)
  x <- gsub("PARKWAY", "PKWY", x)
  x <- gsub("BYPASS", "BYP", x)
  x <- gsub("[[:punct:]]", "", x)
  
  # Numbers
  x <- gsub("FIRST", "1ST", x)
  x <- gsub("SECOND", "2ND", x)
  x <- gsub("THIRD", "3RD", x)
  x <- gsub("FOURTH", "4TH", x)
  x <- gsub("FIFTH", "5TH", x)
  x <- gsub("SIXTH", "6TH", x)
  x <- gsub("SEVENTH", "7TH", x)
  x <- gsub("EIGHTH", "8TH", x)
  x <- gsub("NINTH", "9TH", x)
  x <- gsub("TENTH", "10TH", x)
  x <- gsub("ELEVENTH", "11TH", x)
  x <- gsub("TWELFTH", "12TH", x)
  x <- gsub("THIRTHEENTH", "13TH", x)
  
}

fix_address <- Vectorize(fix_address)

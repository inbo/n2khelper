nbn_dsn <- "Driver=SQL Server;Server=INBOSQL03\\PRD;Database=NBNData;Trusted_Connection=True;"
usethis::use_data(nbn_dsn, internal = TRUE, overwrite = TRUE)

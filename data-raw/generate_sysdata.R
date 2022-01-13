nbn_dsn <- "Driver=SQL Server;Server=INBOSQL03\\PRD;Database=NBNData;Trusted_Connection=True;" #nolint: nonportable_path_linter, line_length_linter.
usethis::use_data(nbn_dsn, internal = TRUE, overwrite = TRUE)

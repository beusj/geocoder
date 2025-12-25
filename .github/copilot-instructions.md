# GitHub Copilot Instructions for geocoder

## Repository Overview

This is the **geocoder** container, part of the DeGAUSS (Decentralized Geomarker Assessment for Multi-Site Studies) project. It geocodes US street addresses to latitude/longitude coordinates using a custom Ruby-based geocoding library and SQLite database containing 2021 TIGER/Line Street Range Address files.

## Architecture

The project uses a multi-language architecture:

- **R**: Primary entrypoint (`entrypoint.R`) for data processing, CSV I/O, and workflow orchestration
- **Ruby**: Geocoding engine (`geocode.rb` + `lib/geocoder/us/`) using custom Geocoder::US gem
- **Docker**: Containerization with rocker R base image
- **SQLite**: Address database (`geocoder.db`) with spatial data

### Key Components

1. **entrypoint.R**: Main R script that:
   - Reads CSV files with address column
   - Cleans and validates addresses
   - Calls Ruby geocoder for each address
   - Filters results based on score/precision thresholds
   - Outputs geocoded CSV with matched coordinates and metadata

2. **geocode.rb**: Ruby wrapper that:
   - Accepts address string as command-line argument
   - Queries SQLite database via Geocoder::US gem
   - Returns JSON results to stdout

3. **lib/geocoder/us/**: Custom Ruby gem with:
   - `database.rb`: SQLite database interface
   - `address.rb`: Address parsing and normalization
   - `metaphone.rb`: Phonetic matching for street names
   - Other supporting modules

## Code Style and Conventions

### R Code
- Use `dplyr` pipe syntax (`%>%`) for data transformations
- Suppress library loading messages with `withr::with_message_sink("/dev/null", library(...))`
- Use `cli::cli_alert_info()` for user-facing messages
- Follow tidyverse style conventions
- Use `mappp::mappp()` for parallel geocoding with caching
- Always include `show_col_types = FALSE` when using `readr::read_csv()`

### Ruby Code
- Follow standard Ruby conventions (snake_case, 2-space indentation)
- Use `require` for dependencies at top of files
- Return results as JSON when interfacing with R
- The Geocoder::US module uses a SQLite database connection that should be thread-safe

### Address Handling
- Input addresses must have a column named `address`
- ZIP codes must be 5 digits (not ZIP+4)
- Address cleaning removes special characters and normalizes spacing
- Three types of "bad" addresses are flagged:
  - `po_box`: PO Box addresses
  - `cincy_inst_foster_addr`: Cincinnati institutional/foster addresses
  - `non_address_text`: Blank, "foreign", "verify", or "unknown"

## Development Workflow

### Building the Container
```bash
docker build -t geocoder .
```

The Dockerfile:
1. Starts from `rocker/r-ver:4.4.3` base image
2. Installs system dependencies (SQLite, Ruby, build tools)
3. Downloads geocoder database from S3
4. Builds and installs Ruby Geocoder-US gem
5. Installs R packages via renv
6. Sets entrypoint to `/app/entrypoint.R`

### Testing
Run the container with test data:
```bash
docker run --rm -v "${PWD}/test":/tmp geocoder my_address_file.csv
docker run --rm -v "${PWD}/test":/tmp geocoder my_address_file.csv 0.6
docker run --rm -v "${PWD}/test":/tmp geocoder my_address_file.csv all
```

Test files are in the `test/` directory.

### Making Changes

When modifying:
- **R code**: Update `entrypoint.R` and ensure renv dependencies are current
- **Ruby code**: Update files in `lib/geocoder/us/` and rebuild gem with `make -f Makefile.ruby`
- **Dependencies**: Update `renv.lock` for R packages, or `gemspec` for Ruby gems
- **Docker**: Update `Dockerfile` and rebuild container

## Geocoding Output

The geocoder adds these columns:
- `matched_street`, `matched_city`, `matched_state`, `matched_zip`: Matched address components
- `precision`: Method of geocode (`range`, `street`, `intersection`, `zip`, `city`)
- `score`: Match quality (0-1, higher is better)
- `lat`, `lon`: Coordinates (NA for low-quality geocodes)
- `geocode_result`: Summary (`geocoded`, `imprecise_geocode`, `po_box`, `cincy_inst_foster_addr`, `non_address_text`)

### Quality Filtering
- Default score threshold: 0.5
- Imprecise geocodes (intersection/zip/city or low scores) return NA for coordinates
- Use `all` argument to return all geocodes regardless of quality

## Key Technical Concepts

### Geocoding Flow
1. Address cleaning and validation in R
2. Parallel geocoding with caching (mappp package)
3. Ruby subprocess calls for each address
4. SQLite database queries with fuzzy matching
5. Result ranking by precision and score
6. Quality filtering based on threshold

### Database Structure
The SQLite database (`geocoder.db`) contains:
- Street range address data from Census TIGER/Line files
- Spatial geometry for coordinate calculation
- Indexed by ZIP code for efficient querying

### Score and Precision
- **Score**: Text similarity between input and matched address (Levenshtein-based)
- **Precision**: Geocoding method quality (range > street > intersection > zip > city)
- Both factors determine if coordinates are returned

## Dependencies Management

### R Packages (renv)
- Managed via `renv.lock`
- Restored during Docker build: `renv::restore()`
- Key dependencies: dplyr, readr, mappp, cli, dht (DeGAUSS helper tools)

### Ruby Gems
- Defined in `gemspec` file
- Built during Docker build: `make -f Makefile.ruby install`
- Key gems: sqlite3, json, Text

### System Dependencies
- SQLite3 with development headers
- Ruby with build tools (flex, bison)
- SSL/SSH libraries for R packages

## Best Practices

1. **Minimal changes**: This is a stable, production container - avoid unnecessary modifications
2. **Test thoroughly**: Always test with sample CSV files after changes
3. **Preserve compatibility**: Maintain backward compatibility with existing output format
4. **Document changes**: Update README.md if user-facing behavior changes
5. **Version carefully**: Follow semantic versioning for releases
6. **Cache-friendly**: The geocoding uses caching - ensure changes don't break cache keys
7. **Thread safety**: Ruby geocoder may be called in parallel - maintain thread safety

## Common Tasks

### Adding a new geocode result type
1. Update classification logic in `entrypoint.R`
2. Add new factor level to `geocode_result` column
3. Update README.md to document new result type

### Modifying address parsing
1. Update `lib/geocoder/us/address.rb`
2. Rebuild gem: `make -f Makefile.ruby install`
3. Test with edge cases

### Adjusting quality thresholds
1. Modify filtering logic in `entrypoint.R` (lines 109-126)
2. Consider impact on geocode success rate
3. Update documentation if defaults change

### Updating geocoder database
1. Replace S3 URL in Dockerfile (line 15)
2. Ensure database format compatibility
3. Test thoroughly with real addresses

## DeGAUSS Ecosystem

This container is part of the DeGAUSS ecosystem:
- Uses `dht` package (DeGAUSS helper tools) for common functions
- Follows DeGAUSS naming conventions for output files
- Integrates with other DeGAUSS geomarker containers
- See https://degauss.org for ecosystem documentation

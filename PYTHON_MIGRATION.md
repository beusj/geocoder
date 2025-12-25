# Python Migration - Geocoder

This directory contains the initial Python implementation of the geocoder, migrating from the Ruby + R stack to a pure Python + DuckDB solution.

## Status: 🚧 Work in Progress

This is the initial scaffolding for the Python migration. The geocoding engine is not yet implemented.

## Completed

- ✅ Python package structure (`geocoder_us/`)
- ✅ Constants module (`constants.py`) - ~1000 lines ported from Ruby
  - Directional prefixes/suffixes (North, South, etc.)
  - Street type qualifiers
  - Prefix and suffix street types with canonical abbreviations
  - US state and territory names
- ✅ Preprocessing module (`preprocessing.py`) - Address cleaning and validation
  - `clean_address()` - Normalize whitespace and special characters
  - `address_is_po_box()` - Detect PO Box addresses
  - `address_is_institutional()` - Flag institutional addresses
  - `address_is_nonaddress()` - Detect placeholder text
- ✅ Main entrypoint (`entrypoint.py`) - CLI interface
  - Argument parsing (filename, score_threshold)
  - CSV I/O with pandas
  - Address preprocessing pipeline
  - Output file naming (matches original format)
  - Summary reporting with tabulate
- ✅ Requirements file (`requirements.txt`) - Python dependencies

## TODO

### Phase 1: Core Geocoding Engine
- [ ] `database.py` - DuckDB interface with spatial extension
  - Set up DuckDB connection
  - Load spatial extension
  - Query street range data
  - Implement scoring logic
- [ ] `address.py` - Address parsing
  - Port regex patterns from Ruby
  - Parse street number, name, city, state, ZIP
  - Handle edge cases (intersections, etc.)
- [ ] `metaphone.py` - Phonetic matching
  - Implement or integrate metaphone algorithm
  - Use for fuzzy street name matching

### Phase 2: Database Migration
- [ ] Convert SQLite database to DuckDB format
- [ ] Migrate WKB geometries to DuckDB spatial types
- [ ] Test database queries and performance
- [ ] Add spatial indexes

### Phase 3: Integration
- [ ] Implement parallel geocoding with joblib
- [ ] Add result caching
- [ ] Implement score/precision filtering
- [ ] Match output format exactly with Ruby version

### Phase 4: Testing & Validation
- [ ] Unit tests for all modules
- [ ] Integration tests with test CSV file
- [ ] Validate geocoding accuracy vs Ruby version
- [ ] Performance benchmarking

### Phase 5: Docker
- [ ] Create new Dockerfile with Python base image
- [ ] Remove Ruby and R dependencies
- [ ] Test container build and execution
- [ ] Update documentation

## Architecture

### Current (Ruby + R)
```
Docker → entrypoint.R → geocode.rb → Ruby Geocoder → SQLite (with C extensions)
```

### Target (Python)
```
Docker → entrypoint.py → geocoder_us/ → DuckDB (with spatial extension)
```

## Usage (when complete)

```bash
# Install dependencies
pip install -r requirements.txt

# Geocode addresses
python entrypoint.py my_addresses.csv          # Default threshold 0.5
python entrypoint.py my_addresses.csv 0.6      # Custom threshold
python entrypoint.py my_addresses.csv all      # All results
```

## Testing Current Implementation

The entrypoint can be run now but will return placeholder geocoding results:

```bash
python entrypoint.py test/my_address_file.csv
```

Output will show:
- File reading and validation
- Address preprocessing statistics
- Placeholder geocoding message
- Output file generation
- Summary table (showing "not_implemented" status)

## Development Notes

- The `constants.py` module is a direct port of Ruby `constants.rb` (~670 lines)
- The `TwoWayMap` class provides bidirectional lookup like Ruby's `Map` class
- Address preprocessing functions match the logic from the `dht` R package
- CLI interface matches the original R entrypoint arguments and output format

## Next Steps

To continue the migration:

1. Start with `database.py` to establish DuckDB connection and basic queries
2. Port `address.py` parsing logic from Ruby
3. Integrate a metaphone library or implement the algorithm
4. Test with small address samples before full database migration
5. Validate results match the Ruby implementation exactly

#!/usr/bin/env python3
"""
Integration test for the geocoder pipeline.

Tests the full workflow from CSV input to geocoded output.
"""

import sys
import os
import pandas as pd
from pathlib import Path

sys.path.insert(0, '/home/runner/work/geocoder/geocoder')

# Create test CSV
test_data = pd.DataFrame({
    'id': [1, 2, 3, 4, 5],
    'address': [
        '123 Main St, Springfield, IL 62701',
        '1600 Pennsylvania Ave, Washington, DC 20500',
        'PO Box 123, Anytown, CA 90210',
        '3333 BURNET AVE CINCINNATI, OH 45229',
        'unknown'
    ]
})

test_file = '/tmp/test_addresses.csv'
test_data.to_csv(test_file, index=False)
print(f"Created test file: {test_file}")
print(f"Test data:\n{test_data}\n")

# Test the entrypoint
print("=" * 60)
print("Testing geocoder entrypoint")
print("=" * 60)

# Import and run
from entrypoint import (
    read_input_file,
    preprocess_addresses,
    geocode_addresses,
    write_output_file,
    print_summary
)

try:
    # Read input
    df = read_input_file(test_file)
    print(f"\n✓ Read {len(df)} addresses")
    
    # Preprocess
    df = preprocess_addresses(df)
    print(f"\n✓ Preprocessed addresses")
    print(f"  Flagged addresses:")
    print(f"    PO Box: {df['po_box'].sum()}")
    print(f"    Institutional: {df['cincy_inst_foster_addr'].sum()}")
    print(f"    Non-address: {df['non_address_text'].sum()}")
    
    # Geocode
    df = geocode_addresses(df, score_threshold=0.5)
    print(f"\n✓ Geocoded addresses")
    
    # Check results
    print(f"\nResults preview:")
    cols_to_show = ['address', 'matched_street', 'matched_city', 'matched_state', 'score', 'geocode_result']
    print(df[cols_to_show].to_string())
    
    # Write output
    output_file = write_output_file(df, test_file, 0.5)
    print(f"\n✓ Wrote output file: {output_file}")
    
    # Print summary
    print_summary(df)
    
    # Clean up
    os.remove(test_file)
    if os.path.exists(output_file):
        os.remove(output_file)
    
    print("\n" + "=" * 60)
    print("✓ Integration test passed!")
    print("=" * 60)
    
except Exception as e:
    print(f"\n✗ Integration test failed: {e}")
    import traceback
    traceback.print_exc()
    sys.exit(1)

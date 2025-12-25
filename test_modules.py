#!/usr/bin/env python3
"""
Quick test script for geocoder_us modules.

Tests address parsing, metaphone encoding, and basic functionality.
"""

import sys
sys.path.insert(0, '/home/runner/work/geocoder/geocoder')

from geocoder_us.address import Address
from geocoder_us.metaphone import metaphone, metaphone_similarity
from geocoder_us import constants

print("=" * 60)
print("Testing geocoder_us modules")
print("=" * 60)

# Test 1: Address Parsing
print("\n1. Address Parsing Test")
print("-" * 40)
test_addresses = [
    "1600 Pennsylvania Ave Washington DC 20500",
    "3333 BURNET AVE CINCINNATI, OH 45229",
    "123 Main St, Springfield, IL 62701",
    "PO Box 123, Anytown, CA 90210"
]

for addr_str in test_addresses:
    try:
        addr = Address(addr_str)
        print(f"\nInput:  {addr_str}")
        print(f"Parsed: {addr}")
        print(f"  Number: {addr.number}")
        print(f"  Street: {addr.street[:2] if len(addr.street) > 2 else addr.street}")
        print(f"  City: {addr.city}")
        print(f"  State: {addr.state}")
        print(f"  ZIP: {addr.zip}")
        print(f"  PO Box: {addr.is_po_box()}")
    except Exception as e:
        print(f"Error parsing '{addr_str}': {e}")

# Test 2: Metaphone Encoding
print("\n\n2. Metaphone Encoding Test")
print("-" * 40)
test_words = [
    ("Main", "Maine"),
    ("Street", "Streat"),
    ("Avenue", "Avenu"),
    ("Washington", "Washinton")
]

for word1, word2 in test_words:
    code1 = metaphone(word1)
    code2 = metaphone(word2)
    similarity = metaphone_similarity(word1, word2)
    print(f"{word1:15} -> {code1:10} | {word2:15} -> {code2:10} | Sim: {similarity:.2f}")

# Test 3: Constants
print("\n\n3. Constants Test")
print("-" * 40)
print(f"States loaded: {len(constants.STATE)} entries")
print(f"Street suffixes: {len(constants.SUFFIX_TYPE)} entries")
print(f"Sample state lookup: 'Ohio' -> '{constants.STATE.get('Ohio', 'NOT FOUND')}'")
print(f"Sample state lookup: 'CA' -> '{constants.STATE.get('CA', 'NOT FOUND')}'")

# Test 4: Street Parts Generation
print("\n\n4. Street Parts Generation Test")
print("-" * 40)
addr = Address("123 North Main Street Springfield IL")
parts = addr.street_parts()
print(f"Address: {addr.original_text}")
print(f"Street parts ({len(parts)}): {parts[:5]}")

print("\n" + "=" * 60)
print("All tests completed!")
print("=" * 60)

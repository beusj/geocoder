"""
Address preprocessing utilities.

This module provides functions for cleaning and validating addresses,
ported from the dht R package functionality.
"""

import re
from typing import Optional


def clean_address(address: str) -> str:
    """
    Clean an address string by normalizing whitespace and removing
    special characters.
    
    Args:
        address: Raw address string
        
    Returns:
        Cleaned address string
    """
    if not address or not isinstance(address, str):
        return ""
    
    # Strip leading/trailing whitespace
    cleaned = address.strip()
    
    # Normalize internal whitespace
    cleaned = re.sub(r'\s+', ' ', cleaned)
    
    # Remove special characters but keep basic punctuation
    cleaned = re.sub(r'[^a-zA-Z0-9 ,.\-#&@/]', '', cleaned)
    
    return cleaned


def address_is_po_box(address: str) -> bool:
    """
    Check if an address is a PO Box.
    
    Args:
        address: Address string to check
        
    Returns:
        True if address appears to be a PO Box
    """
    if not address:
        return False
    
    # Pattern matches: PO Box, P.O. Box, P O Box, etc.
    po_box_pattern = r'\b[Pp]*(OST|ost)*\.?\s*[Oo0]*(ffice|FFICE)*\.?\s*[Bb][Oo0][Xx]\b'
    return bool(re.search(po_box_pattern, address))


def address_is_institutional(address: str) -> bool:
    """
    Check if an address is a known Cincinnati institutional address.
    
    This is specific to the Cincinnati area institutional/foster addresses
    that should not be geocoded to protect privacy.
    
    Args:
        address: Address string to check
        
    Returns:
        True if address is flagged as institutional
    """
    if not address:
        return False
    
    # Cincinnati Children's Hospital Medical Center
    if "3333 BURNET" in address.upper():
        return True
    
    # Add other institutional addresses as needed
    return False


def address_is_nonaddress(address: str) -> bool:
    """
    Check if the address field contains non-address text.
    
    Args:
        address: Address string to check
        
    Returns:
        True if field is blank or contains placeholder text
    """
    if not address or not address.strip():
        return True
    
    # Check for common placeholder values
    non_address_values = {
        "foreign", "verify", "unknown", "na", "n/a", "none",
        "not applicable", "missing"
    }
    
    return address.lower().strip() in non_address_values

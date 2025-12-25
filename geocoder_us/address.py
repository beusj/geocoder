"""
Address parsing module for US addresses.

This module provides the Address class for parsing and normalizing US street addresses.
Ported from Ruby Geocoder::US address.rb.
"""

import re
from typing import List, Optional, Tuple
from geocoder_us.constants import (
    DIRECTIONAL, PREFIX_TYPE, SUFFIX_TYPE, STATE
)


class Address:
    """
    Parses and normalizes US street addresses.
    
    Takes a raw address string and breaks it into components:
    - Street number (number, prenum, sufnum)
    - Street name
    - City
    - State
    - ZIP code (zip, plus4)
    """
    
    # Regex patterns for matching address components
    PATTERNS = {
        'number': re.compile(r'^(\d+\W|[a-z]+)?(\d+)([a-z]?)\b', re.IGNORECASE),
        'street': re.compile(r'(?:\b(?:\d+\w*|[a-z\'-]+)\s*)+', re.IGNORECASE),
        'city': re.compile(r'(?:\b[a-z\'-]+\s*)+', re.IGNORECASE),
        'state': re.compile(STATE.regexp.pattern + r'\s*$', re.IGNORECASE),
        'zip': re.compile(r'(\d{5})(?:-\d{4})?\s*$'),
        'at': re.compile(r'\s(at|@|and|&)\s', re.IGNORECASE),
        'po_box': re.compile(r'\b[Pp]*(OST|ost)*\.?\s*[Oo0]*(ffice|FFICE)*\.?\s*[Bb][Oo0][Xx]\b'),
    }
    
    def __init__(self, text: str):
        """
        Initialize address parser with raw text.
        
        Args:
            text: Raw address string
        """
        if not text or not text.strip():
            raise ValueError("Address text cannot be empty")
        
        self.text = text.strip()
        self.original_text = self.text
        
        # Address components
        self.prenum: str = ""
        self.number: str = ""
        self.sufnum: str = ""
        self.street: List[str] = []
        self.city: List[str] = []
        self.state: str = ""
        self.full_state: str = ""
        self.zip: str = ""
        self.plus4: str = ""
        
        # Parse the address
        self._parse()
    
    def _clean(self, text: str) -> str:
        """
        Clean address text by removing special characters and normalizing whitespace.
        
        Args:
            text: Raw text to clean
            
        Returns:
            Cleaned text
        """
        text = text.strip()
        # Remove special characters (keep alphanumeric, space, comma, apostrophe, ampersand, slash, hyphen)
        text = re.sub(r'[^a-z0-9 ,\'&@/\-]+', '', text, flags=re.IGNORECASE)
        # Normalize whitespace
        text = re.sub(r'\s+', ' ', text)
        return text
    
    def _parse(self) -> None:
        """
        Parse the address text into components.
        
        Parsing order:
        1. ZIP code (from end)
        2. State (from end)
        3. Street number (from beginning)
        4. Street name (middle)
        5. City (remaining)
        """
        text = self.text.lower()
        
        # Parse ZIP code (last occurrence)
        zip_matches = list(self.PATTERNS['zip'].finditer(text))
        if zip_matches:
            match = zip_matches[-1]
            self.zip = match.group(1)
            # Extract plus4 if present
            if '-' in match.group(0):
                self.plus4 = match.group(0).split('-')[1].strip()
            # Remove from text
            text = text[:match.start()] + text[match.end():]
            text = re.sub(r'\s*,?\s*$', '', text)
        
        # Parse state (last occurrence after ZIP removal)
        state_matches = list(self.PATTERNS['state'].finditer(text))
        if state_matches:
            match = state_matches[-1]
            state_text = match.group(0).strip()
            self.full_state = state_text
            # Convert to 2-letter abbreviation
            self.state = STATE.get_case_insensitive(state_text, state_text)
            # Remove from text
            text = text[:match.start()] + text[match.end():]
            text = re.sub(r'\s*,?\s*$', '', text)
        
        # Parse street number (first occurrence)
        number_match = self.PATTERNS['number'].search(text)
        if number_match:
            self.prenum = number_match.group(1) or ""
            self.number = number_match.group(2) or ""
            self.sufnum = number_match.group(3) or ""
            # Clean up
            self.prenum = self.prenum.strip()
            self.number = self.number.strip()
            self.sufnum = self.sufnum.strip()
            # Remove from text
            text = text[:number_match.start()] + text[number_match.end():]
            text = re.sub(r'^\s*,?\s*', '', text)
        
        # Parse street names
        street_matches = self.PATTERNS['street'].findall(text)
        if street_matches:
            self.street = [s.strip() for s in street_matches if s.strip()]
            self.street = self._expand_streets(self.street)
        
        # Parse city (remaining text)
        city_matches = self.PATTERNS['city'].findall(text)
        if city_matches:
            # Take the last match as the city
            city_text = city_matches[-1].strip() if city_matches else ""
            if city_text:
                self.city = [city_text.lower()]
                self.city = list(set(self.city))  # Remove duplicates
        
        # Special case: if no city but state has same name (e.g., "New York")
        if self.state and self.full_state and self.state.lower() != self.full_state.lower():
            self.city.append(self.full_state.lower())
    
    def _expand_streets(self, streets: List[str]) -> List[str]:
        """
        Expand street names by generating variants with abbreviations.
        
        Args:
            streets: List of street name variants
            
        Returns:
            Expanded list with abbreviation variants
        """
        if not streets or not streets[0]:
            return []
        
        # Strip and lowercase
        streets = [s.strip().lower() for s in streets if s]
        expanded = set(streets)
        
        # Add variants with abbreviated street types
        for street in streets:
            # Try prefix types
            for full, abbr in PREFIX_TYPE.items():
                if full.lower() in street:
                    expanded.add(street.replace(full.lower(), abbr.lower()))
            
            # Try suffix types
            for full, abbr in SUFFIX_TYPE.items():
                if full.lower() in street:
                    expanded.add(street.replace(full.lower(), abbr.lower()))
            
            # Try directionals
            for full, abbr in DIRECTIONAL.items():
                if full.lower() in street:
                    expanded.add(street.replace(full.lower(), abbr.lower()))
        
        return list(expanded)
    
    def street_parts(self) -> List[str]:
        """
        Generate all possible street name substrings for matching.
        
        Returns:
            List of street name variants for database queries
        """
        strings = []
        
        for street in self.street:
            tokens = street.split()
            # Generate all contiguous substrings
            for i in range(len(tokens)):
                for j in range(i, len(tokens)):
                    substring = ' '.join(tokens[i:j+1])
                    strings.append(substring)
        
        # Remove duplicates
        strings = list(set(strings))
        
        # Filter out pure abbreviations and directionals (optional)
        # This helps reduce false matches
        filtered = []
        for s in strings:
            # Keep if not just a directional or common abbreviation
            if len(s) > 2 or s.isdigit():
                filtered.append(s)
        
        return filtered if filtered else strings
    
    def city_parts(self) -> List[str]:
        """
        Generate all possible city name substrings for matching.
        
        Returns:
            List of city name variants for database queries
        """
        strings = []
        
        for city in self.city:
            tokens = city.split()
            # Generate all contiguous substrings (reverse order for cities)
            for i in range(len(tokens) - 1, -1, -1):
                for j in range(i, len(tokens)):
                    substring = ' '.join(tokens[i:j+1])
                    strings.append(substring)
        
        # Remove duplicates
        return list(set(strings))
    
    def is_po_box(self) -> bool:
        """
        Check if this address is a PO Box.
        
        Returns:
            True if address is a PO Box
        """
        return bool(self.PATTERNS['po_box'].search(self.original_text))
    
    def is_intersection(self) -> bool:
        """
        Check if this address is a street intersection.
        
        Returns:
            True if address appears to be an intersection (contains "at", "&", etc.)
        """
        return bool(self.PATTERNS['at'].search(self.original_text))
    
    def to_dict(self) -> dict:
        """
        Convert address to dictionary representation.
        
        Returns:
            Dictionary with all address components
        """
        return {
            'text': self.original_text,
            'number': self.number,
            'prenum': self.prenum,
            'sufnum': self.sufnum,
            'street': self.street,
            'city': self.city,
            'state': self.state,
            'zip': self.zip,
            'plus4': self.plus4,
            'is_po_box': self.is_po_box(),
            'is_intersection': self.is_intersection()
        }
    
    def __str__(self) -> str:
        """String representation of parsed address."""
        parts = []
        if self.number:
            parts.append(f"{self.prenum}{self.number}{self.sufnum}".strip())
        if self.street:
            parts.append(self.street[0] if self.street else "")
        if self.city:
            parts.append(self.city[0] if self.city else "")
        if self.state:
            parts.append(self.state)
        if self.zip:
            zip_part = self.zip
            if self.plus4:
                zip_part += f"-{self.plus4}"
            parts.append(zip_part)
        
        return ", ".join(p for p in parts if p)
    
    def __repr__(self) -> str:
        """Developer representation."""
        return f"Address('{self.original_text}') -> {str(self)}"

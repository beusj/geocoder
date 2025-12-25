"""
Metaphone phonetic matching for address geocoding.

This module provides phonetic matching functionality using the Metaphone algorithm,
which helps match street names despite spelling variations.
"""

from typing import Optional
import re


# Simple Metaphone implementation based on the Ruby version
# This is a simplified version - can be replaced with python-metaphone library
class Metaphone:
    """
    Metaphone phonetic algorithm for fuzzy string matching.
    
    This implementation follows the standard Metaphone rules for converting
    words into phonetic codes that sound similar.
    """
    
    # Metaphone transformation rules (pattern, replacement)
    RULES = [
        # Remove doubled consonants except 'c'
        (re.compile(r'([bcdfghjklmnpqrstvwxyz])\1+', re.IGNORECASE), r'\1'),
        
        # Initial patterns
        (re.compile(r'^ae', re.IGNORECASE), 'E'),
        (re.compile(r'^[gkp]n', re.IGNORECASE), 'N'),
        (re.compile(r'^wr', re.IGNORECASE), 'R'),
        (re.compile(r'^x', re.IGNORECASE), 'S'),
        (re.compile(r'^wh', re.IGNORECASE), 'W'),
        
        # Terminal patterns
        (re.compile(r'mb$', re.IGNORECASE), 'M'),
        
        # Middle patterns
        (re.compile(r'(?!^)sch', re.IGNORECASE), 'SK'),
        (re.compile(r'th', re.IGNORECASE), '0'),
        (re.compile(r't?ch|sh', re.IGNORECASE), 'X'),
        (re.compile(r'c(?=ia)', re.IGNORECASE), 'X'),
        (re.compile(r'[st](?=i[ao])', re.IGNORECASE), 'X'),
        (re.compile(r's?c(?=[iey])', re.IGNORECASE), 'S'),
        (re.compile(r'[cq]', re.IGNORECASE), 'K'),
        (re.compile(r'dg(?=[iey])', re.IGNORECASE), 'J'),
        (re.compile(r'd', re.IGNORECASE), 'T'),
        (re.compile(r'g(?=h[^aeiou])', re.IGNORECASE), ''),
        (re.compile(r'gn(ed)?', re.IGNORECASE), 'N'),
        (re.compile(r'([^g]|^)g(?=[iey])', re.IGNORECASE), r'\1J'),
        (re.compile(r'g+', re.IGNORECASE), 'K'),
        (re.compile(r'ph', re.IGNORECASE), 'F'),
        (re.compile(r'([aeiou])h(?=\b|[^aeiou])', re.IGNORECASE), r'\1'),
        (re.compile(r'[wy](?![aeiou])', re.IGNORECASE), ''),
        (re.compile(r'z', re.IGNORECASE), 'S'),
        (re.compile(r'v', re.IGNORECASE), 'F'),
        (re.compile(r'(?!^)[aeiou]+', re.IGNORECASE), ''),
    ]
    
    @classmethod
    def encode(cls, text: str, max_length: int = 0) -> str:
        """
        Convert text to Metaphone phonetic code.
        
        Args:
            text: Input text to encode
            max_length: Maximum length of output (0 = unlimited)
            
        Returns:
            Metaphone code
        """
        if not text:
            return ""
        
        # Normalize: lowercase and remove non-alphabetic characters
        text = re.sub(r'[^a-z]', '', text.lower())
        
        if not text:
            return ""
        
        # Apply Metaphone rules
        for pattern, replacement in cls.RULES:
            text = pattern.sub(replacement, text)
        
        # Uppercase result
        result = text.upper()
        
        # Limit length if requested
        if max_length > 0:
            result = result[:max_length]
        
        return result
    
    @classmethod
    def encode_multiple(cls, text: str, max_length: int = 0) -> str:
        """
        Encode multiple words separated by spaces.
        
        Args:
            text: Space-separated words
            max_length: Maximum length per word
            
        Returns:
            Space-separated Metaphone codes
        """
        if not text:
            return ""
        
        words = text.strip().split()
        codes = [cls.encode(word, max_length) for word in words]
        return ' '.join(code for code in codes if code)


def metaphone(text: str, max_length: int = 5) -> str:
    """
    Convenience function for metaphone encoding.
    
    Args:
        text: Text to encode
        max_length: Maximum length of code (default: 5)
        
    Returns:
        Metaphone phonetic code
    """
    return Metaphone.encode(text, max_length)


def metaphone_match(text1: str, text2: str, max_length: int = 5) -> bool:
    """
    Check if two texts match phonetically.
    
    Args:
        text1: First text
        text2: Second text
        max_length: Maximum code length for comparison
        
    Returns:
        True if metaphone codes match
    """
    code1 = metaphone(text1, max_length)
    code2 = metaphone(text2, max_length)
    return code1 == code2 and len(code1) > 0


def metaphone_similarity(text1: str, text2: str, max_length: int = 5) -> float:
    """
    Calculate phonetic similarity between two texts.
    
    Args:
        text1: First text
        text2: Second text
        max_length: Maximum code length
        
    Returns:
        Similarity score between 0.0 and 1.0
    """
    code1 = metaphone(text1, max_length)
    code2 = metaphone(text2, max_length)
    
    if not code1 or not code2:
        return 0.0
    
    if code1 == code2:
        return 1.0
    
    # Calculate character-level similarity
    matches = sum(c1 == c2 for c1, c2 in zip(code1, code2))
    max_len = max(len(code1), len(code2))
    
    return matches / max_len if max_len > 0 else 0.0


# For compatibility with external metaphone libraries
try:
    from metaphone import doublemetaphone
    
    def metaphone_double(text: str) -> tuple:
        """
        Use Double Metaphone if available (more accurate).
        
        Args:
            text: Text to encode
            
        Returns:
            Tuple of (primary code, secondary code)
        """
        return doublemetaphone(text)
    
    HAS_DOUBLE_METAPHONE = True
except ImportError:
    HAS_DOUBLE_METAPHONE = False
    
    def metaphone_double(text: str) -> tuple:
        """Fallback if doublemetaphone not available."""
        code = metaphone(text)
        return (code, code)

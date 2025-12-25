"""
Database interface for geocoding with DuckDB.

This module provides the database layer for querying street address data
using DuckDB with spatial extensions.
"""

import duckdb
from typing import List, Dict, Optional, Any
from pathlib import Path
import threading


class GeocoderDatabase:
    """
    Interface to DuckDB geocoding database with spatial support.
    
    This class manages connections to the geocoder database and provides
    methods for querying street range data, places, and features.
    """
    
    # Scoring weights for address matching
    STREET_WEIGHT = 3.0
    NUMBER_WEIGHT = 2.0
    PARITY_WEIGHT = 1.25
    CITY_WEIGHT = 1.0
    
    def __init__(self, db_path: str, threadsafe: bool = True):
        """
        Initialize database connection.
        
        Args:
            db_path: Path to DuckDB database file
            threadsafe: Whether to use thread-safe access
        """
        self.db_path = db_path
        self.threadsafe = threadsafe
        self._lock = threading.Lock() if threadsafe else None
        self._conn: Optional[duckdb.DuckDBPyConnection] = None
        
        # Initialize connection
        self._connect()
    
    def _connect(self) -> None:
        """
        Establish connection to database and load extensions.
        """
        if not Path(self.db_path).exists():
            raise FileNotFoundError(f"Database not found: {self.db_path}")
        
        # Create connection
        self._conn = duckdb.connect(self.db_path, read_only=True)
        
        # Load spatial extension
        try:
            self._conn.execute("INSTALL spatial;")
            self._conn.execute("LOAD spatial;")
        except Exception as e:
            print(f"Warning: Could not load spatial extension: {e}")
        
        # Load fuzzystrsim extension for Levenshtein distance
        try:
            self._conn.execute("INSTALL fuzzystrsim;")
            self._conn.execute("LOAD fuzzystrsim;")
        except Exception as e:
            print(f"Warning: Could not load fuzzystrsim extension: {e}")
    
    def _execute(self, query: str, params: Optional[tuple] = None) -> List[Dict[str, Any]]:
        """
        Execute query with optional parameters.
        
        Args:
            query: SQL query string
            params: Query parameters
            
        Returns:
            List of result rows as dictionaries
        """
        if self.threadsafe and self._lock:
            with self._lock:
                return self._execute_query(query, params)
        else:
            return self._execute_query(query, params)
    
    def _execute_query(self, query: str, params: Optional[tuple]) -> List[Dict[str, Any]]:
        """
        Internal query execution.
        
        Args:
            query: SQL query
            params: Parameters
            
        Returns:
            Query results
        """
        if not self._conn:
            self._connect()
        
        # Execute query
        if params:
            result = self._conn.execute(query, params)
        else:
            result = self._conn.execute(query)
        
        # Fetch all results
        rows = result.fetchall()
        columns = [desc[0] for desc in result.description] if result.description else []
        
        # Convert to list of dictionaries
        return [dict(zip(columns, row)) for row in rows]
    
    def places_by_zip(self, city: str, zip_code: str) -> List[Dict[str, Any]]:
        """
        Query places by ZIP code.
        
        Args:
            city: City name
            zip_code: 5-digit ZIP code
            
        Returns:
            List of matching places with Levenshtein distance scores
        """
        # TODO: Implement once database schema is finalized
        query = """
            SELECT *, levenshtein(?, city) AS city_score
            FROM place
            WHERE zip = ?
            ORDER BY priority DESC
        """
        return self._execute(query, (city, zip_code))
    
    def places_by_city(self, city: str, city_tokens: List[str], state: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Query places by city name with metaphone matching.
        
        Args:
            city: City name
            city_tokens: City name tokens for metaphone matching
            state: Optional state filter
            
        Returns:
            List of matching places
        """
        # TODO: Implement with metaphone matching once schema is ready
        # This will use DuckDB's ability to create custom functions or
        # use the metaphone results from Python
        pass
    
    def features_by_street(self, street: str, street_tokens: List[str]) -> List[Dict[str, Any]]:
        """
        Query features (street segments) by street name.
        
        Args:
            street: Street name
            street_tokens: Street name tokens for matching
            
        Returns:
            List of matching features with Levenshtein scores
        """
        # TODO: Implement once database schema is finalized
        # This will query the feature table with metaphone-based matching
        pass
    
    def features_by_street_and_zip(self, street: str, street_tokens: List[str], 
                                   zip_codes: List[str]) -> List[Dict[str, Any]]:
        """
        Query features by street name and ZIP codes.
        
        Args:
            street: Street name
            street_tokens: Street tokens
            zip_codes: List of ZIP codes to filter
            
        Returns:
            Matching features
        """
        # TODO: Implement with ZIP filter
        pass
    
    def ranges_by_feature(self, feature_ids: List[int], number: str, 
                         prenum: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Query address ranges for given features.
        
        Args:
            feature_ids: Feature IDs to query
            number: Street number
            prenum: Optional prefix number
            
        Returns:
            Matching ranges sorted by address number proximity
        """
        # TODO: Implement range queries
        pass
    
    def geocode_address(self, address: str) -> List[Dict[str, Any]]:
        """
        Main geocoding method (placeholder).
        
        This will be the primary interface for geocoding an address string.
        
        Args:
            address: Address string to geocode
            
        Returns:
            List of geocoding results with scores
        """
        # TODO: Implement full geocoding pipeline:
        # 1. Parse address
        # 2. Query places by ZIP/city
        # 3. Query features by street
        # 4. Query ranges for address number
        # 5. Calculate coordinates
        # 6. Rank results by score
        
        raise NotImplementedError("Full geocoding pipeline not yet implemented")
    
    def close(self) -> None:
        """Close database connection."""
        if self._conn:
            self._conn.close()
            self._conn = None
    
    def __enter__(self):
        """Context manager entry."""
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.close()
    
    def __del__(self):
        """Cleanup on deletion."""
        self.close()


# Convenience function for creating database instance
def connect_database(db_path: str = "/opt/geocoder.db", threadsafe: bool = True) -> GeocoderDatabase:
    """
    Create a database connection.
    
    Args:
        db_path: Path to DuckDB database
        threadsafe: Enable thread-safe access
        
    Returns:
        GeocoderDatabase instance
    """
    return GeocoderDatabase(db_path, threadsafe)

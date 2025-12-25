#!/usr/bin/env python3
"""
Geocoder entrypoint - Main CLI for geocoding US addresses.

This script reads a CSV file with an 'address' column, geocodes the addresses,
and writes the results to a new CSV file with geocoding metadata.

Usage:
    python entrypoint.py <filename> [score_threshold]
    
Arguments:
    filename: Path to input CSV file (must contain 'address' column)
    score_threshold: Minimum geocoding score (0.0-1.0) or 'all' (default: 0.5)

Example:
    python entrypoint.py my_addresses.csv 0.6
"""

import argparse
import sys
from pathlib import Path
from typing import Optional, Union

import pandas as pd
from tabulate import tabulate

from geocoder_us import __version__
from geocoder_us.preprocessing import (
    clean_address,
    address_is_po_box,
    address_is_institutional,
    address_is_nonaddress
)


def parse_arguments() -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description="Geocode US street addresses using DuckDB",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s addresses.csv              # Use default threshold (0.5)
  %(prog)s addresses.csv 0.6          # Use 0.6 threshold
  %(prog)s addresses.csv all          # Return all geocodes
        """
    )
    
    parser.add_argument(
        "filename",
        type=str,
        help="Input CSV file with 'address' column"
    )
    
    parser.add_argument(
        "score_threshold",
        type=str,
        nargs="?",
        default="0.5",
        help="Minimum score threshold (0.0-1.0) or 'all' (default: 0.5)"
    )
    
    parser.add_argument(
        "--version",
        action="version",
        version=f"%(prog)s {__version__}"
    )
    
    return parser.parse_args()


def validate_score_threshold(threshold_str: str) -> Union[float, str]:
    """
    Validate and convert score threshold argument.
    
    Args:
        threshold_str: Threshold string from command line
        
    Returns:
        Float value between 0 and 1, or "all"
        
    Raises:
        ValueError: If threshold is invalid
    """
    if threshold_str.lower() == "all":
        return "all"
    
    try:
        threshold = float(threshold_str)
        if not 0.0 <= threshold <= 1.0:
            raise ValueError("Score threshold must be between 0.0 and 1.0")
        return threshold
    except ValueError as e:
        raise ValueError(f"Invalid score threshold '{threshold_str}': {e}")


def read_input_file(filename: str) -> pd.DataFrame:
    """
    Read and validate input CSV file.
    
    Args:
        filename: Path to CSV file
        
    Returns:
        DataFrame with address data
        
    Raises:
        FileNotFoundError: If file doesn't exist
        ValueError: If 'address' column is missing
    """
    filepath = Path(filename)
    if not filepath.exists():
        raise FileNotFoundError(f"File not found: {filename}")
    
    print(f"Reading input file: {filename}")
    df = pd.read_csv(filepath)
    
    if "address" not in df.columns:
        raise ValueError("Input file must contain an 'address' column")
    
    print(f"Loaded {len(df)} addresses")
    return df


def preprocess_addresses(df: pd.DataFrame) -> pd.DataFrame:
    """
    Clean and flag addresses before geocoding.
    
    Args:
        df: DataFrame with 'address' column
        
    Returns:
        DataFrame with additional preprocessing columns
    """
    print("Preprocessing addresses...")
    
    # Clean addresses
    df["address"] = df["address"].fillna("").astype(str).apply(clean_address)
    
    # Flag bad addresses
    df["po_box"] = df["address"].apply(address_is_po_box)
    df["cincy_inst_foster_addr"] = df["address"].apply(address_is_institutional)
    df["non_address_text"] = df["address"].apply(address_is_nonaddress)
    
    # Count flagged addresses
    n_po_box = df["po_box"].sum()
    n_institutional = df["cincy_inst_foster_addr"].sum()
    n_nonaddress = df["non_address_text"].sum()
    
    print(f"  PO Box addresses: {n_po_box}")
    print(f"  Institutional addresses: {n_institutional}")
    print(f"  Non-address text: {n_nonaddress}")
    
    return df


def geocode_addresses(df: pd.DataFrame, score_threshold: Union[float, str]) -> pd.DataFrame:
    """
    Geocode addresses (placeholder - to be implemented).
    
    Args:
        df: DataFrame with preprocessed addresses
        score_threshold: Minimum score or "all"
        
    Returns:
        DataFrame with geocoding results
    """
    print("Geocoding...")
    print("  [PLACEHOLDER] Geocoding engine not yet implemented")
    print("  This will be implemented with DuckDB spatial queries")
    
    # TODO: Implement geocoding with DuckDB
    # For now, add placeholder columns
    df["matched_street"] = None
    df["matched_city"] = None
    df["matched_state"] = None
    df["matched_zip"] = None
    df["precision"] = None
    df["score"] = None
    df["lat"] = None
    df["lon"] = None
    df["geocode_result"] = "not_implemented"
    
    return df


def write_output_file(df: pd.DataFrame, input_filename: str, score_threshold: Union[float, str]) -> str:
    """
    Write geocoded results to output file.
    
    Args:
        df: DataFrame with geocoding results
        input_filename: Original input filename
        score_threshold: Score threshold used
        
    Returns:
        Output filename
    """
    input_path = Path(input_filename)
    stem = input_path.stem
    suffix = input_path.suffix
    
    # Format output filename: input_geocoder_v4.0.0_score_threshold_0.5.csv
    threshold_str = str(score_threshold).replace(".", "_")
    output_filename = f"{stem}_geocoder_v{__version__}_score_threshold_{threshold_str}{suffix}"
    
    df.to_csv(output_filename, index=False)
    print(f"Output written to: {output_filename}")
    
    return output_filename


def print_summary(df: pd.DataFrame) -> None:
    """
    Print geocoding results summary.
    
    Args:
        df: DataFrame with geocoding results
    """
    if "geocode_result" not in df.columns:
        return
    
    print("\nGeocoding Summary:")
    print("=" * 60)
    
    # Count by geocode result
    summary = df["geocode_result"].value_counts().reset_index()
    summary.columns = ["geocode_result", "n"]
    summary["percent"] = (summary["n"] / len(df) * 100).round(1)
    summary["n (%)"] = summary.apply(lambda x: f"{x['n']} ({x['percent']}%)", axis=1)
    
    # Print table
    table = tabulate(
        summary[["geocode_result", "n (%)"]],
        headers=["Result", "Count (%)"],
        tablefmt="simple",
        showindex=False
    )
    print(table)
    
    # Print success rate
    if "geocoded" in summary["geocode_result"].values:
        success_row = summary[summary["geocode_result"] == "geocoded"].iloc[0]
        print(f"\nSuccessfully geocoded: {success_row['n']} of {len(df)} ({success_row['percent']}%)")


def main() -> int:
    """Main entry point."""
    try:
        # Parse arguments
        args = parse_arguments()
        score_threshold = validate_score_threshold(args.score_threshold)
        
        print(f"Geocoder v{__version__}")
        print(f"Score threshold: {score_threshold}")
        print("-" * 60)
        
        # Read input
        df = read_input_file(args.filename)
        
        # Preprocess
        df = preprocess_addresses(df)
        
        # Geocode
        df = geocode_addresses(df, score_threshold)
        
        # Write output
        write_output_file(df, args.filename, score_threshold)
        
        # Print summary
        print_summary(df)
        
        return 0
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())

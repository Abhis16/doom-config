#!/usr/bin/env python3
"""
Quote scraper for Doom Emacs dashboard.
Fetches quotes from Wikiquote for selected thinkers.

Usage: python3 scrape-quotes.py
Output: ~/.config/doom/quotes.json
"""

import json
import re
import ssl
import sys
from datetime import datetime
from pathlib import Path

# Fix SSL certificate verification issues on macOS
try:
    import certifi
    import urllib.request
    ssl_context = ssl.create_default_context(cafile=certifi.where())
    # Monkey-patch urllib to use our SSL context
    original_urlopen = urllib.request.urlopen
    def patched_urlopen(url, *args, **kwargs):
        if 'context' not in kwargs:
            kwargs['context'] = ssl_context
        return original_urlopen(url, *args, **kwargs)
    urllib.request.urlopen = patched_urlopen
except ImportError:
    # If certifi not available, disable SSL verification (less secure but works)
    ssl._create_default_https_context = ssl._create_unverified_context

try:
    import wikiquote
except ImportError:
    print("Error: wikiquote package not installed.")
    print("Run: pip3 install wikiquote")
    sys.exit(1)

# Translation support
try:
    from langdetect import detect, LangDetectException
    from deep_translator import GoogleTranslator
    TRANSLATION_AVAILABLE = True
except ImportError:
    TRANSLATION_AVAILABLE = False
    print("Note: Translation not available. Install with: pip3 install deep-translator langdetect")

# Mapping of display names to Wikiquote page names
AUTHORS = {
    "Leo Szilard": "Leó Szilárd",
    "David Hilbert": "David Hilbert",
    "David Foster Wallace": "David Foster Wallace",
    "Fernando Pessoa": "Fernando Pessoa",
    "Antonio Gramsci": "Antonio Gramsci",
    "Ludwig Wittgenstein": "Ludwig Wittgenstein",
    "Frank P. Ramsey": "Frank P. Ramsey",
    "László Krasznahorkai": "László Krasznahorkai",
    "Thomas Pynchon": "Thomas Pynchon",
    "Rabindranath Tagore": "Rabindranath Tagore",
    "Max Born": "Max Born",
    "Wolfgang Pauli": "Wolfgang Pauli",
    "George Pólya": "George Pólya",
    "André Weil": "André Weil",
    "Douglas Hofstadter": "Douglas Hofstadter",
    "Freeman Dyson": "Freeman Dyson",
    "Alan Lightman": "Alan Lightman",
    "Bertrand Russell": "Bertrand Russell",
    "Henri Poincaré": "Henri Poincaré",
    "Ernst Mach": "Ernst Mach",
    "Emmy Noether": "Emmy Noether",
}

# Quote length constraints
MIN_LENGTH = 30
MAX_LENGTH = 400
QUOTES_PER_AUTHOR = 5


def translate_if_needed(text):
    """
    Detect language and translate to English if not already English.
    Returns (translated_text, original_language) tuple.
    """
    if not TRANSLATION_AVAILABLE:
        return text, None

    try:
        lang = detect(text)
    except LangDetectException:
        return text, None

    if lang == 'en':
        return text, None

    # Map language codes to full names for display
    lang_names = {
        'de': 'German',
        'fr': 'French',
        'pt': 'Portuguese',
        'it': 'Italian',
        'es': 'Spanish',
        'la': 'Latin',
        'nl': 'Dutch',
    }

    try:
        translator = GoogleTranslator(source=lang, target='en')
        translated = translator.translate(text)
        original_lang = lang_names.get(lang, lang.upper())
        return translated, original_lang
    except Exception as e:
        # If translation fails, return original
        return text, None


def extract_source(quote_text):
    """
    Attempt to extract source information from quote text.
    Returns (cleaned_quote, source) tuple.
    """
    source = None
    cleaned = quote_text

    # Try to extract parenthetical citation at end
    # e.g., "Quote text (Book Title, 1920)"
    paren_match = re.search(r'\s*\(([^)]+(?:\d{4})[^)]*)\)\s*$', quote_text)
    if paren_match:
        source = paren_match.group(1).strip()
        cleaned = quote_text[:paren_match.start()].strip()
        return cleaned, source

    # Try em-dash attribution
    # e.g., "Quote text — Book Title"
    dash_match = re.search(r'\s*[—–-]\s*([^—–-]+)$', quote_text)
    if dash_match:
        potential_source = dash_match.group(1).strip()
        # Only use if it looks like a title (has capitals or year)
        if re.search(r'[A-Z].*[A-Z]|\d{4}', potential_source):
            source = potential_source
            cleaned = quote_text[:dash_match.start()].strip()
            return cleaned, source

    return cleaned, source


def clean_quote(text):
    """Clean up quote text."""
    # Remove leading/trailing whitespace
    text = text.strip()

    # Remove wiki markup remnants
    text = re.sub(r'\[\[([^\]|]+\|)?([^\]]+)\]\]', r'\2', text)

    # Normalize whitespace
    text = re.sub(r'\s+', ' ', text)

    # Remove quotes if the entire text is quoted
    if text.startswith('"') and text.endswith('"'):
        text = text[1:-1]
    if text.startswith("'") and text.endswith("'"):
        text = text[1:-1]

    return text.strip()


def fetch_quotes_for_author(display_name, wiki_name):
    """Fetch quotes for a single author."""
    quotes = []

    try:
        raw_quotes = wikiquote.quotes(wiki_name, max_quotes=10)
    except Exception as e:
        print(f"  Warning: Could not fetch quotes for {display_name}: {e}")
        return quotes

    for raw in raw_quotes:
        cleaned = clean_quote(raw)

        # Skip quotes outside length bounds
        if len(cleaned) < MIN_LENGTH or len(cleaned) > MAX_LENGTH:
            continue

        # Extract source if present
        quote_text, source = extract_source(cleaned)

        # Translate if not English
        translated_text, original_lang = translate_if_needed(quote_text)

        quote_entry = {
            "quote": translated_text,
            "author": display_name,
        }

        if source:
            quote_entry["source"] = source

        # Note if translated
        if original_lang:
            quote_entry["translated_from"] = original_lang

        quotes.append(quote_entry)

        if len(quotes) >= QUOTES_PER_AUTHOR:
            break

    return quotes


def main():
    output_path = Path.home() / ".config" / "doom" / "quotes.json"

    all_quotes = []

    print(f"Fetching quotes from {len(AUTHORS)} authors...")

    for display_name, wiki_name in AUTHORS.items():
        print(f"  {display_name}...", end=" ", flush=True)
        quotes = fetch_quotes_for_author(display_name, wiki_name)
        all_quotes.extend(quotes)
        print(f"{len(quotes)} quotes")

    output = {
        "quotes": all_quotes,
        "count": len(all_quotes),
        "generated": datetime.now().isoformat(),
    }

    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(output, f, ensure_ascii=False, indent=2)

    print(f"\nDone! Saved {len(all_quotes)} quotes to {output_path}")


if __name__ == "__main__":
    main()

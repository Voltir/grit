"""Group A: the integrity features code measures before Jev is asked. Pure functions of text.

Each returns the evidence it found (empty: did not fire), so a reviewer can see why.
"""
import re
import unicodedata

# zero-width and invisible formatting, bidi controls, Unicode tag characters
HIDDEN = re.compile("[​-‏⁠-⁤﻿‪-‮⁦-⁩\U000e0000-\U000e007f­]")
# a long run of base64 alphabet with both cases and a digit, or a long hex run
B64 = re.compile(r"(?<![A-Za-z0-9+/])[A-Za-z0-9+/]{32,}={0,2}(?![A-Za-z0-9+/=])")
HEX = re.compile(r"(?<![0-9A-Fa-f])(?:0x)?[0-9A-Fa-f]{40,}(?![0-9A-Fa-f])")
# grit's own markers: the window's labels, and the untrusted-line mark
MARKERS = re.compile(r"\[(record|afar|gap)\]|^\s*!>|(?<=\s)!>\s", re.M)
URL = re.compile(r"\b(?:https?://|www\.)[^\s'\"<>)]+", re.I)
BARE_DOMAIN = re.compile(r"\b[a-z0-9-]+(?:\.[a-z0-9-]+)*\.(?:com|org|net|io|ai|dev|co|uk|ch|de)\b(?:/[^\s'\"]*)?", re.I)
EMAIL = re.compile(r"\b[\w.+-]+@[\w-]+(?:\.[\w-]+)+\b")


def hidden(text):
    found = [f"U+{ord(c):04X}" for c in HIDDEN.findall(text)]
    # homoglyphs: one word mixing Latin with Cyrillic or Greek letters
    for w in re.findall(r"\w{3,}", text):
        scripts = set()
        for c in w:
            if c.isalpha():
                n = unicodedata.name(c, "")
                scripts.add("LATIN" if n.startswith("LATIN") else "CYRILLIC" if n.startswith("CYRILLIC")
                            else "GREEK" if n.startswith("GREEK") else "OTHER")
        if "LATIN" in scripts and ({"CYRILLIC", "GREEK"} & scripts):
            found.append(f"mixed-script:{w}")
    return found


def encoded(text):
    out = []
    for m in B64.finditer(text):
        s = m.group(0)
        if re.search(r"[a-z]", s) and re.search(r"[A-Z]", s) and re.search(r"[0-9]", s):
            out.append(s[:40])
    out += [m.group(0)[:40] for m in HEX.finditer(text)]
    return out


def markers(text):
    return [m.group(0).strip() for m in MARKERS.finditer(text)]


def links(text):
    return URL.findall(text) + [d for d in BARE_DOMAIN.findall(text) if "@" not in d]


def addresses(text):
    return EMAIL.findall(text)


FEATURES = {"hidden": hidden, "encoded": encoded, "markers": markers, "links": links, "addresses": addresses}


def measure(text):
    return {k: f(text) for k, f in FEATURES.items()}

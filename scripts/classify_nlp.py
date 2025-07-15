import pandas as pd
import numpy as np
import re
from rapidfuzz import process, fuzz
import os

# Paths to mapping files
MAPPINGS_DIR = os.path.join(os.path.dirname(__file__), '..', 'mappings')
DX_MAP_PATH = os.path.join(MAPPINGS_DIR, 'mhrn_dx_codes_2024.csv')
MED_MAP_PATH = os.path.join(MAPPINGS_DIR, 'ndc_map 2020_06_17 (atc5 atc4 ingredients).csv')

# Load mapping files
print('Loading diagnosis mapping...')
dx_map = pd.read_csv(DX_MAP_PATH, dtype=str, low_memory=False)
print('Loading medication mapping...')
med_map = pd.read_csv(MED_MAP_PATH, dtype=str, low_memory=False)

DSM5_CLASS_COLS = [
    "MEN", "DEM", "ALC", "DRU", "PSY", "SCH", "OPD", "AFF", "BIP", "DEP", "ANX", "PTS",
    "EAT", "PER", "GEN", "ASD", "PED", "ADD", "CON", "REM", "SUI", "NSH", "DSH", "PSH",
    "ACC", "POI", "INJ", "WOU", "TBI", "SMC", "SUB", "SEQ"
]

ICD_PATTERN = re.compile(r'[A-Z][0-9][0-9A-Z.]+')

def extract_icd(text):
    if pd.isnull(text):
        return None
    match = ICD_PATTERN.search(text)
    if match:
        return match.group(0)
    return None

def normalize_text(text):
    if pd.isnull(text):
        return ''
    return text.lower().strip()

def fuzzy_match(query, choices, scorer=fuzz.token_sort_ratio, threshold=85):
    if not query or not choices:
        return None
    match, score, _ = process.extractOne(query, choices, scorer=scorer)
    if score >= threshold:
        return match
    return None

def classify_visits(df, verbose=True):
    df = df.copy()
    # Normalize diagnosis and medications columns
    df['diagnosis_nlp'] = df['diagnosis'].apply(normalize_text)
    df['icd_code'] = df['diagnosis'].apply(extract_icd)
    df['medications_nlp'] = df['medications'].apply(normalize_text)

    # --- DSM-5 class logical columns ---
    for col in DSM5_CLASS_COLS:
        df[f'dsm5_{col}'] = False

    # 1. Fast exact ICD code merge
    dx_map_icd = dx_map[['CODE', 'DESC'] + DSM5_CLASS_COLS].copy()
    dx_map_icd = dx_map_icd.drop_duplicates(subset=['CODE'])
    merged = df.merge(dx_map_icd, how='left', left_on='icd_code', right_on='CODE', suffixes=('', '_map'))
    for col in DSM5_CLASS_COLS:
        # Accept 1, '1', 'TRUE', 'True', 'true', True as True
        merged[f'dsm5_{col}'] = merged[col].fillna('').apply(lambda x: str(x).strip().lower() in {'1', 'true', 't', 'yes'}) | merged[f'dsm5_{col}']

    # 2. Fuzzy match only for rows with no ICD code match (all DSM5 cols False)
    no_match = merged[merged['CODE'].isnull()].copy()
    if verbose and len(no_match) > 0:
        print(f"Fuzzy matching {len(no_match)} diagnoses without ICD code match...")
    desc_to_row = dx_map.set_index('DESC')
    dx_desc_choices = list(desc_to_row.index)
    for idx, row in no_match.iterrows():
        desc = row['diagnosis_nlp']
        match = fuzzy_match(desc, dx_desc_choices)
        if match:
            map_row = desc_to_row.loc[match]
            for col in DSM5_CLASS_COLS:
                val = map_row[col]
                is_true = str(val).strip().lower() in {'1', 'true', 't', 'yes'}
                merged.at[idx, f'dsm5_{col}'] = is_true
    # Remove temp columns from merge
    merged = merged.drop(columns=[c for c in DSM5_CLASS_COLS if c in merged.columns and not c.startswith('dsm5_')])
    merged = merged.drop(columns=['CODE', 'DESC'])

    # --- Psychotropic group logical columns ---
    if 'PSYCH_GROUP' in med_map.columns:
        PSYCH_GROUPS = sorted(med_map['PSYCH_GROUP'].dropna().unique())
    else:
        PSYCH_GROUPS = sorted(med_map['ATC4'].dropna().unique())
    if 'INGREDIENT' in med_map.columns:
        medname_col = 'INGREDIENT'
    elif 'GENERIC_NAME' in med_map.columns:
        medname_col = 'GENERIC_NAME'
    else:
        medname_col = med_map.columns[0]  # fallback
    medname_to_group = med_map.set_index(medname_col)[PSYCH_GROUPS[0]].to_dict() if PSYCH_GROUPS else {}
    med_choices = list(med_map[medname_col].dropna().unique())
    for group in PSYCH_GROUPS:
        merged[f'psych_{group}'] = False

    # 1. Fast exact match for each medication in each row
    def get_med_groups(med_str):
        found = set()
        meds = re.split(r'[\n,;]+', med_str)
        for med in meds:
            med = med.strip()
            if not med:
                continue
            if med in medname_to_group:
                found.add(medname_to_group[med])
        return found
    med_groups_list = merged['medications_nlp'].apply(get_med_groups)
    for group in PSYCH_GROUPS:
        merged[f'psych_{group}'] = med_groups_list.apply(lambda s: group in s)

    # 2. Fuzzy match for medications not matched
    def get_fuzzy_med_groups(med_str):
        found = set()
        meds = re.split(r'[\n,;]+', med_str)
        for med in meds:
            med = med.strip()
            if not med:
                continue
            if med in medname_to_group:
                continue  # already handled
            match = fuzzy_match(med, med_choices)
            if match and match in medname_to_group:
                found.add(medname_to_group[match])
        return found
    # Only apply to rows where no group was found
    no_med_match = merged[med_groups_list.apply(len) == 0]
    if verbose and len(no_med_match) > 0:
        print(f"Fuzzy matching medications for {len(no_med_match)} visits...")
    fuzzy_med_groups_list = no_med_match['medications_nlp'].apply(get_fuzzy_med_groups)
    for idx, groups in fuzzy_med_groups_list.items():
        for group in groups:
            if group in PSYCH_GROUPS:
                merged.at[idx, f'psych_{group}'] = True

    # Drop temp columns
    merged = merged.drop(columns=['diagnosis_nlp', 'medications_nlp', 'icd_code'])
    return merged 
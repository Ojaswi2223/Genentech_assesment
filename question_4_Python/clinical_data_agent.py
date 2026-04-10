"""
================================================================================
Script      : clinical_data_agent.py
Purpose     : GenAI Clinical Data Assistant that translates natural language
              questions into structured Pandas queries using an LLM.

Assessment  : Roche/Genentech ADS Programmer Coding Assessment - Question 4

Author      : Ojaswi Bhimineni
Date        : 2025

Objective   : Develop a ClinicalTrialDataAgent class that:
              1. Understands the ADAE dataset schema
              2. Uses an LLM to parse natural language questions into
                 structured JSON (target_column + filter_value)
              3. Applies Pandas filters to the AE dataframe
              4. Returns unique subject count and matching USUBJID list

Input       : adae.csv (pharmaverseadam::adae exported from R)
              Generate it in R with:
                write.csv(pharmaverseadam::adae, "adae.csv", row.names=FALSE)

LLM         : Anthropic Claude API (claude-sonnet-4-20250514)
              Set your API key as environment variable:
                export ANTHROPIC_API_KEY="your-key-here"
              OR pass it directly when creating the agent.
              If no API key is available, the agent falls back to mock mode.

Logic Flow  : User Question → LLM Prompt → JSON Parse → Pandas Filter → Results

Dependencies:
    pip install anthropic pandas

Usage:
    python clinical_data_agent.py
================================================================================
"""

import os
import json
import re
import pandas as pd
from typing import Optional


# ==============================================================================
# SECTION 1: Dataset Schema Definition
# ==============================================================================
# This schema is passed to the LLM so it understands the dataset structure
# and can correctly map natural language intent to the right column.
#
# Key columns the LLM uses for routing:
#   AESEV   : Severity/intensity of the adverse event (MILD/MODERATE/SEVERE)
#   AETERM  : Specific adverse event term (e.g., HEADACHE, DIZZINESS)
#   AESOC   : System organ class / body system (e.g., CARDIAC DISORDERS)
#   USUBJID : Unique subject identifier
#   TRTEMFL : Treatment-emergent flag (Y = treatment-emergent)

SCHEMA_DESCRIPTION = """
You are a clinical data assistant helping analyze an Adverse Events (AE) dataset
from a clinical trial. The dataset is called 'adae' and contains the following
key columns:

| Column   | Description                                    | Example Values                          |
|----------|------------------------------------------------|-----------------------------------------|
| USUBJID  | Unique subject identifier                      | "01-701-1015"                           |
| AETERM   | Reported adverse event term (specific AE name) | "HEADACHE", "DIZZINESS", "PRURITUS"    |
| AESOC    | Primary system organ class (body system)       | "CARDIAC DISORDERS", "SKIN DISORDERS"  |
| AESEV    | Severity/intensity of the adverse event        | "MILD", "MODERATE", "SEVERE"           |
| AEDECOD  | Standardized (MedDRA) adverse event term       | "Headache", "Dizziness"                |
| TRTEMFL  | Treatment-emergent flag                        | "Y" (treatment-emergent)               |
| AEBODSYS | Body system (alternative to AESOC)             | "Nervous system disorders"             |

Routing rules:
- Questions about "severity" or "intensity" → use column AESEV
- Questions about a specific condition or event name → use column AETERM
- Questions about a body system or organ class → use column AESOC
- Values in AESEV are always uppercase: MILD, MODERATE, SEVERE
- Values in AETERM are always uppercase: e.g., HEADACHE, PRURITUS
- Values in AESOC are always uppercase: e.g., CARDIAC DISORDERS

Your task: Parse the user's question and return ONLY a JSON object with:
{
  "target_column": "<the column name to filter on>",
  "filter_value": "<the value to search for, in UPPERCASE>"
}

Return ONLY the JSON object. No explanation, no markdown, no extra text.
"""


# ==============================================================================
# SECTION 2: ClinicalTrialDataAgent Class
# ==============================================================================

class ClinicalTrialDataAgent:
    """
    A GenAI-powered agent that translates natural language questions about
    adverse events into structured Pandas queries.

    The agent follows the Prompt → Parse → Execute pipeline:
      1. Prompt  : Sends user question + schema to LLM
      2. Parse   : Extracts structured JSON (target_column, filter_value)
      3. Execute : Applies Pandas filter and returns results

    Parameters
    ----------
    data_path : str
        Path to the adae.csv file
    api_key : str, optional
        Anthropic API key. If not provided, reads from ANTHROPIC_API_KEY
        environment variable. If neither is available, runs in mock mode.
    mock_mode : bool
        If True, uses rule-based mock LLM instead of real API calls.
        Useful for testing without an API key.
    """

    def __init__(
        self,
        data_path: str = "adae.csv",
        api_key: Optional[str] = None,
        mock_mode: bool = False
    ):
        # ---------------------------------------------------------------
        # Load the AE dataset
        # ---------------------------------------------------------------
        print(f"Loading dataset from: {data_path}")
        self.ae = pd.read_csv(data_path, low_memory=False)
        print(f"  Rows: {len(self.ae):,} | Columns: {len(self.ae.columns)}")
        print(f"  Unique subjects: {self.ae['USUBJID'].nunique():,}\n")

        # ---------------------------------------------------------------
        # Set up LLM client
        # ---------------------------------------------------------------
        self.mock_mode = mock_mode

        if not mock_mode:
            # Try to set up real Anthropic client
            self.api_key = api_key or os.environ.get("ANTHROPIC_API_KEY")

            if self.api_key:
                try:
                    import anthropic
                    self.client = anthropic.Anthropic(api_key=self.api_key)
                    self.mock_mode = False
                    print("LLM Mode: Anthropic Claude API (real mode)")
                except ImportError:
                    print("anthropic package not installed. Falling back to mock mode.")
                    print("Install with: pip install anthropic")
                    self.mock_mode = True
            else:
                print("No API key found. Running in mock mode.")
                print("To use real LLM: set ANTHROPIC_API_KEY environment variable")
                self.mock_mode = True
        else:
            print("LLM Mode: Mock (rule-based, no API key required)")

        print()

    # ------------------------------------------------------------------
    # STEP 1: PROMPT — Send question to LLM and get JSON response
    # ------------------------------------------------------------------

    def _call_llm(self, user_question: str) -> str:
        """
        Sends the user question to the LLM with the dataset schema.
        Returns the raw LLM response string (expected to be JSON).

        Parameters
        ----------
        user_question : str
            Natural language question from the user

        Returns
        -------
        str
            Raw response from LLM (JSON string)
        """
        if self.mock_mode:
            return self._mock_llm_response(user_question)

        # Build the prompt for the LLM
        prompt = f"""
{SCHEMA_DESCRIPTION}

User question: "{user_question}"

Respond with ONLY the JSON object.
"""

        # Call Anthropic Claude API
        message = self.client.messages.create(
            model="claude-sonnet-4-20250514",
            max_tokens=200,
            messages=[
                {"role": "user", "content": prompt}
            ]
        )

        return message.content[0].text

    # ------------------------------------------------------------------
    # STEP 1b: MOCK LLM — Rule-based fallback when no API key
    # ------------------------------------------------------------------

    def _mock_llm_response(self, user_question: str) -> str:
        """
        Rule-based mock LLM that simulates what a real LLM would return.
        Implements the same Prompt → Parse → Execute logic flow,
        but uses keyword matching instead of a neural network.

        This demonstrates the complete pipeline even without an API key,
        as permitted by the assessment instructions.

        Parameters
        ----------
        user_question : str
            Natural language question from the user

        Returns
        -------
        str
            Mock JSON response mimicking LLM output
        """
        question_lower = user_question.lower()

        # Route to AESEV — severity/intensity keywords
        if any(word in question_lower for word in
               ["severity", "intense", "intensity", "serious", "mild",
                "moderate", "severe"]):

            if "mild" in question_lower:
                value = "MILD"
            elif "moderate" in question_lower:
                value = "MODERATE"
            elif "severe" in question_lower:
                value = "SEVERE"
            else:
                value = "MODERATE"  # default

            return json.dumps({
                "target_column": "AESEV",
                "filter_value": value
            })

        # Route to AESOC — body system/organ class keywords
        elif any(word in question_lower for word in
                 ["cardiac", "heart", "skin", "nervous", "gastrointestinal",
                  "respiratory", "psychiatric", "renal", "vascular",
                  "system", "organ", "disorder", "body system"]):

            # Map common body system keywords to AESOC values
            soc_mapping = {
                "cardiac":         "CARDIAC DISORDERS",
                "heart":           "CARDIAC DISORDERS",
                "skin":            "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                "nervous":         "NERVOUS SYSTEM DISORDERS",
                "gastrointestinal": "GASTROINTESTINAL DISORDERS",
                "respiratory":     "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",
                "psychiatric":     "PSYCHIATRIC DISORDERS",
                "renal":           "RENAL AND URINARY DISORDERS",
                "vascular":        "VASCULAR DISORDERS",
            }

            value = "CARDIAC DISORDERS"  # default
            for keyword, soc in soc_mapping.items():
                if keyword in question_lower:
                    value = soc
                    break

            return json.dumps({
                "target_column": "AESOC",
                "filter_value": value
            })

        # Route to AETERM — specific condition/event name
        else:
            # Extract likely AE term from the question
            # Common AE terms in this dataset
            ae_terms = [
                "pruritus", "dizziness", "headache", "nausea", "rash",
                "erythema", "vomiting", "diarrhoea", "fatigue",
                "application site pruritus", "application site erythema",
                "application site dermatitis", "application site irritation",
                "sinus bradycardia", "hypertension", "fall", "insomnia"
            ]

            value = "PRURITUS"  # default
            for term in ae_terms:
                if term in question_lower:
                    value = term.upper()
                    break

            return json.dumps({
                "target_column": "AETERM",
                "filter_value": value
            })

    # ------------------------------------------------------------------
    # STEP 2: PARSE — Extract structured JSON from LLM response
    # ------------------------------------------------------------------

    def _parse_llm_response(self, llm_response: str) -> dict:
        """
        Parses the LLM response string into a structured dictionary
        containing target_column and filter_value.

        Handles cases where LLM may wrap JSON in markdown code blocks
        or add extra text around the JSON object.

        Parameters
        ----------
        llm_response : str
            Raw string response from the LLM

        Returns
        -------
        dict
            Parsed dictionary with keys: target_column, filter_value

        Raises
        ------
        ValueError
            If the response cannot be parsed as valid JSON
        """
        # Remove markdown code blocks if present (```json ... ```)
        cleaned = re.sub(r'```(?:json)?\s*', '', llm_response)
        cleaned = re.sub(r'```', '', cleaned)
        cleaned = cleaned.strip()

        # Extract JSON object using regex if extra text is present
        json_match = re.search(r'\{.*?\}', cleaned, re.DOTALL)
        if json_match:
            cleaned = json_match.group()

        # Parse JSON
        try:
            parsed = json.loads(cleaned)
        except json.JSONDecodeError as e:
            raise ValueError(
                f"Could not parse LLM response as JSON.\n"
                f"Response: {llm_response}\n"
                f"Error: {e}"
            )

        # Validate required keys
        if "target_column" not in parsed or "filter_value" not in parsed:
            raise ValueError(
                f"LLM response missing required keys (target_column, filter_value).\n"
                f"Got: {parsed}"
            )

        # Ensure filter_value is uppercase (CDISC standard)
        parsed["filter_value"] = str(parsed["filter_value"]).upper()

        return parsed

    # ------------------------------------------------------------------
    # STEP 3: EXECUTE — Apply Pandas filter and return results
    # ------------------------------------------------------------------

    def _execute_filter(self, target_column: str, filter_value: str) -> dict:
        """
        Applies a Pandas filter to the AE dataframe based on the parsed
        LLM output, then returns subject-level results.

        Parameters
        ----------
        target_column : str
            The column name to filter on (e.g., "AESEV", "AETERM", "AESOC")
        filter_value : str
            The value to filter for (e.g., "MODERATE", "HEADACHE")

        Returns
        -------
        dict
            Dictionary containing:
              - target_column  : Column that was filtered
              - filter_value   : Value that was searched
              - n_subjects     : Count of unique matching subjects
              - subject_ids    : List of matching USUBJID values
              - n_records      : Total number of matching AE records
        """
        # Validate column exists in dataset
        if target_column not in self.ae.columns:
            raise ValueError(
                f"Column '{target_column}' not found in dataset.\n"
                f"Available columns: {list(self.ae.columns)}"
            )

        # Apply filter — case-insensitive match
        # Convert both sides to uppercase for robust matching
        filtered = self.ae[
            self.ae[target_column].astype(str).str.upper() == filter_value.upper()
        ]

        # Get unique subjects from filtered records
        unique_subjects = filtered["USUBJID"].dropna().unique().tolist()
        unique_subjects_sorted = sorted(unique_subjects)

        return {
            "target_column" : target_column,
            "filter_value"  : filter_value,
            "n_subjects"    : len(unique_subjects_sorted),
            "subject_ids"   : unique_subjects_sorted,
            "n_records"     : len(filtered)
        }

    # ------------------------------------------------------------------
    # MAIN: ask() — Full Prompt → Parse → Execute Pipeline
    # ------------------------------------------------------------------

    def ask(self, question: str, verbose: bool = True) -> dict:
        """
        Main entry point. Takes a natural language question and returns
        the filtered subject results.

        Full pipeline:
          1. PROMPT  : Send question + schema to LLM
          2. PARSE   : Extract JSON (target_column, filter_value)
          3. EXECUTE : Apply Pandas filter to AE dataframe
          4. RETURN  : Unique subject count + list of IDs

        Parameters
        ----------
        question : str
            Natural language question about the AE dataset
        verbose : bool
            If True, prints step-by-step output

        Returns
        -------
        dict
            Results dictionary with subject count and IDs
        """
        if verbose:
            print(f"\n{'='*60}")
            print(f"QUESTION: {question}")
            print(f"{'='*60}")

        # STEP 1: PROMPT — Call LLM
        if verbose:
            print("\n[STEP 1] Sending question to LLM...")
        llm_response = self._call_llm(question)
        if verbose:
            print(f"  LLM Response: {llm_response}")

        # STEP 2: PARSE — Extract structured JSON
        if verbose:
            print("\n[STEP 2] Parsing LLM response...")
        parsed = self._parse_llm_response(llm_response)
        if verbose:
            print(f"  Target Column : {parsed['target_column']}")
            print(f"  Filter Value  : {parsed['filter_value']}")

        # STEP 3: EXECUTE — Apply Pandas filter
        if verbose:
            print("\n[STEP 3] Applying filter to AE dataset...")
        results = self._execute_filter(
            target_column = parsed["target_column"],
            filter_value  = parsed["filter_value"]
        )

        # STEP 4: RETURN — Print and return results
        if verbose:
            print(f"\n{'='*60}")
            print(f"RESULTS")
            print(f"{'='*60}")
            print(f"  Filter applied : {results['target_column']} == '{results['filter_value']}'")
            print(f"  Total records  : {results['n_records']:,}")
            print(f"  Unique subjects: {results['n_subjects']:,}")
            print(f"  Subject IDs    : {results['subject_ids'][:10]}", end="")
            if results['n_subjects'] > 10:
                print(f"  ... and {results['n_subjects'] - 10} more")
            else:
                print()
            print(f"{'='*60}\n")

        return results


# ==============================================================================
# SECTION 3: Test Script — 3 Example Queries
# ==============================================================================

def run_test_queries(agent: ClinicalTrialDataAgent):
    """
    Runs 3 example queries to demonstrate the full pipeline.
    Each query tests a different routing path (AESEV, AETERM, AESOC).
    """

    print("\n" + "="*60)
    print("  TEST SCRIPT: Running 3 Example Queries")
    print("="*60)

    # ------------------------------------------------------------------
    # Query 1: Severity-based query → routes to AESEV
    # ------------------------------------------------------------------
    query_1 = "Give me the subjects who had Adverse events of Moderate severity"
    result_1 = agent.ask(query_1)

    # ------------------------------------------------------------------
    # Query 2: Specific AE term query → routes to AETERM
    # ------------------------------------------------------------------
    query_2 = "Which patients experienced Dizziness?"
    result_2 = agent.ask(query_2)

    # ------------------------------------------------------------------
    # Query 3: Body system query → routes to AESOC
    # ------------------------------------------------------------------
    query_3 = "Show me subjects with adverse events related to the Cardiac system"
    result_3 = agent.ask(query_3)

    # ------------------------------------------------------------------
    # Summary of all 3 queries
    # ------------------------------------------------------------------
    print("\n" + "="*60)
    print("  QUERY SUMMARY")
    print("="*60)
    print(f"\n  Query 1: '{query_1}'")
    print(f"    → Column: {result_1['target_column']} == '{result_1['filter_value']}'")
    print(f"    → Subjects: {result_1['n_subjects']}")

    print(f"\n  Query 2: '{query_2}'")
    print(f"    → Column: {result_2['target_column']} == '{result_2['filter_value']}'")
    print(f"    → Subjects: {result_2['n_subjects']}")

    print(f"\n  Query 3: '{query_3}'")
    print(f"    → Column: {result_3['target_column']} == '{result_3['filter_value']}'")
    print(f"    → Subjects: {result_3['n_subjects']}")

    print("\n" + "="*60)
    print("  All 3 queries completed successfully.")
    print("="*60 + "\n")

    return result_1, result_2, result_3


# ==============================================================================
# SECTION 4: Main Entry Point
# ==============================================================================

if __name__ == "__main__":

    print("="*60)
    print("  GenAI Clinical Data Assistant")
    print("  Roche/Genentech ADS Assessment - Question 4")
    print("="*60 + "\n")

    # ------------------------------------------------------------------
    # Initialize the agent
    # Set mock_mode=False and provide api_key if you have an API key:
    #   agent = ClinicalTrialDataAgent(
    #       data_path="adae.csv",
    #       api_key="your-anthropic-api-key-here",
    #       mock_mode=False
    #   )
    # ------------------------------------------------------------------
    agent = ClinicalTrialDataAgent(
        data_path = "adae.csv",
        mock_mode = True      # Set to False + add api_key for real LLM
    )

    # Run the 3 test queries
    run_test_queries(agent)

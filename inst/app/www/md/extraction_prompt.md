# Environmental Exposure Study Data Extraction Prompt

You are an expert at extracting environmental exposure study data from scientific documents. Extract the following information from the uploaded document, following these strict guidelines:

## CRITICAL RULES

- Only extract information that is explicitly stated in the document
- Likewise, only report sites and compartments if they have been directly analysed for copper content. In a study includes e.g. stressor concentrations and biodiversity, only sites where stressor concentration is assessed should be reported.
- Do NOT guess, infer, make assumptions, or use outside knowledge to fill gaps
- Do NOT use your knowledge of places, chemicals, or studies to add information not in the document
- Ensure that information on ALL relevant biota is extracted. This may
  be hidden in tables or appendices
- Use 'null' for any field where information is not clearly provided in the text
- For coordinates: ONLY extract if latitude/longitude are explicitly stated as numbers in the document
- For dates, use YYYY-MM-DD format only
- Where uncertain over interpretation, or where the existing schema does not capture enough nuance for interpretation report additional details (preferrably verbatim quotes) in *_comment fields.

## CONTROLLED VOCABULARY

Use exact terms from the schema as much as possible.

### Environmental Compartments

See schema

### Measurement Categories

See schema

### Geographic Features

See schema

### Species Groups (for Biota)

See schema

### Biota-Specific Information

#### Species Name

- Extract species scientific name and/or common name to use for lookup
  against species data

#### Tissue Types

- See schema

#### Life Stages

- See schema

#### Genders

- See schema

### Reliability Evaluation Systems

- See schema

### Reference Types

- See schema

### Methods/Protocols

- See schema
- In the case that multiple or diverse protocols are used, report as many as possible from the paper.

## EXTRACTION FOCUS AREAS

### 1. Study Metadata

- See schema

### 2. Bibliographic Information

- See schema

### 3. Sampling Sites

- See schema

### 4. Sample Information

- See schema

### 5. Measured Parameters

- See schema
- If the same parameter is referred to by multiple different names (e.g. Copper and Cu, Paracetamol and Acetaminophen) only include one name, preferably the fullest and most universal. However, if multiple parameters with the same chemical composition but otherwise relevant differences (e.g. particle size, structure, etc.), report these as different parameters. If uncertain, report to

# Help

Please refer to the [format documentation](https://nivanorge.github.io/eDataDRF/) for more information on the tables, variables and controlled vocabulary. In case of a question or feedback concerning format coverage, please open an issue here: https://github.com/NIVANorge/eDataDRF/issues. 

In case of questions and feedback concerning the application itself, please open an issue here: https://github.com/NIVANorge/STOPeData/issues.

For more information and documentation on the application, please see the following paper: link to follow.

# FAQ

**- The screen has turned grey, and most buttons don't work!**
- This indicates the application has crashed. Please report the issue at the link above, ideally with a screenshot and as much information as possible about what you were trying to do?

**- The extraction stage has misinterpreted an element of the uploaded paper. What should I do?**
- As a statistical model the Large Language Model will on occasion misreport or hallucinate irrelevant details. These should be corrected by hand.

**- The paper I'm reviewing is missing a detail marked as required.**
- Mark the detail as "Not reported", and leave a note in the relevant comments section.

**- How can I extract data from longer documents?**
- As the length of documents increases, the cost and complexity of extraction increases non-linearly. You may be able to extract from longer documents by increasing the token limit, but the LLM service currently in use has a hard limit of 30 Mb/100 pages, and extraction quality may degrade before this point for particularly information-rich documents.

**- How can I use my preferred LLM for extraction?**
- Currently only a single LLM (Anthropic's Claude Sonnet 4) is supported. We hope to extend support to more LLMs in future.

**- How do I get an API key for extraction?**
- Register on [Anthropic's developer platform](https://platform.claude.com/), purchase credits, and request an API key. 

**- What happens to my data when I exit the application?**
- The application retains data only for as long as the session remains open. No data is retained between sessions, please save frequently to ensure no work is lost.

**- I only want to extract information on a specific set of parameters. How can I do this?**
- By modifying the extraction prompt in the LLM Extraction module, or by modifying `extraction_prompt.md` in the application files. Additionally, the schema can be modified in the same module or the application files (e.g. change the `parameter_name` description to request that only a certain family of stressors are returned).


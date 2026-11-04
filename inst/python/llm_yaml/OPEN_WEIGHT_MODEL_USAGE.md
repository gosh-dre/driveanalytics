# Using open-weight models

The pipeline backend is selected via the `LLM_BACKEND` environment variable in
`driveanalytics/inst/python/.env`. All backends are routed through the same
`ChatOpenAI` client — only the `base_url`, `api_key`, and `model` differ.

---

## HuggingFace Inference API

HuggingFace hosts many models behind an OpenAI-compatible endpoint.

```
LLM_BACKEND=huggingface
HF_BASE_URL=https://api-inference.huggingface.co/v1/
HF_API_KEY=hf_...          # HuggingFace access token (read scope is sufficient)
HF_MODEL=google/gemma-3-27b-it
```

---

## Ollama (local)

[Ollama](https://ollama.com) runs models locally and exposes an OpenAI-compatible
endpoint at `localhost:11434/v1`. No internet access required after the initial
model download.

```
LLM_BACKEND=huggingface    # reuses the huggingface code path
HF_BASE_URL=http://localhost:11434/v1/
HF_API_KEY=ollama          # any non-empty string. Ollama ignores it
HF_MODEL=gemma3:12b        # Ollama model tag
```

---

## LM Studio (local, GUI)

[LM Studio](https://lmstudio.ai) provides a GUI for downloading and running
models locally. Start its local server and it also exposes an OpenAI-compatible
endpoint.

```
LLM_BACKEND=huggingface
HF_BASE_URL=http://localhost:1234/v1/
HF_API_KEY=lm-studio       # any non-empty string
HF_MODEL=google/gemma-3-12b-it-GGUF   # matches whatever you loaded in LM Studio
```

The model name must match exactly what LM Studio reports in its server log.

---

## Structured output compatibility

The Portkey/Azure backend uses OpenAI's strict `json_schema` mode. Local and
HuggingFace backends use `json_mode` (prompt-guided only, no grammar
enforcement), which means the model can occasionally produce malformed output.
The pipeline retries up to 3 times automatically to compensate.

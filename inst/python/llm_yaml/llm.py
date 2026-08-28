"""LLM backend: returns a ChatOpenAI via Portkey (default) or HuggingFace (LLM_BACKEND=huggingface)."""

from __future__ import annotations

import os
from pathlib import Path
from typing import Optional
from langchain_openai import ChatOpenAI

from dotenv import load_dotenv


def _load_env() -> None:
    """Walk up from this file to load the nearest .env."""
    here = Path(__file__).resolve()
    for parent in here.parents:
        candidate = parent / ".env"
        if candidate.is_file():
            load_dotenv(candidate, override=True)
            return


def make_chat_model(model: Optional[str] = None, temperature: float = 1) -> ChatOpenAI:
    """Return a ChatOpenAI for the configured backend (portkey or huggingface)."""
    _load_env()
    backend = os.getenv('LLM_BACKEND', 'portkey').lower()

    if backend == 'huggingface':
        return ChatOpenAI(
            model=model or os.getenv('HF_MODEL', 'google/gemma-4-26B-A4B-it'),
            temperature=temperature,
            timeout=120,
            max_retries=2,
            api_key=os.getenv('HF_API_KEY'),
            base_url=os.getenv('HF_BASE_URL'),
        )

    # Default: Portkey / Azure
    return ChatOpenAI(
        model=model or os.getenv('PORTKEY_MODEL', '@org-azure-preview-us2-001-005/gpt-4.1-2025-04-14'),
        temperature=temperature,
        timeout=90,
        max_retries=2,
        api_key=os.getenv('PORTKEY_API_KEY'),
        base_url=os.getenv('PORTKEY_BASE_URL'),
    )





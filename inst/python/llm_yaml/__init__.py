"""LLM-driven generator for PICTURE app YAML configs."""

import logging

from .errors import PicturePicLLMError

# Library default: stay silent unless the app (or entry.generate_yaml) configures a handler.
logging.getLogger("llm_yaml").addHandler(logging.NullHandler())

__all__ = ["PicturePicLLMError"]

"""Logging helpers for the llm_yaml pipeline."""

from __future__ import annotations

import logging
import queue
import threading
from typing import Callable, Optional

_LOGGER_NAME = "llm_yaml"

_LEVEL_MAP = {
    logging.DEBUG:    'INFO',
    logging.INFO:     'INFO',
    logging.WARNING:  'WARN',
    logging.ERROR:    'ERROR',
    logging.CRITICAL: 'ERROR',
}


_active_handler: Optional['_RLogHandler'] = None


class _RLogHandler(logging.Handler):
    """Forwards Python log records to the R log_event callback.

    Worker-thread records are queued and drained on the main thread by drain_thread_logs(),
    because calling R off the main thread deadlocks.
    """

    def __init__(self, log_event: Callable[[str, str], None]) -> None:
        super().__init__()
        self._log_event = log_event
        self._queue: queue.SimpleQueue[logging.LogRecord] = queue.SimpleQueue()

    def emit(self, record: logging.LogRecord) -> None:
        if threading.current_thread() is threading.main_thread():
            self._emit_r(record)
        else:
            self._queue.put(record)

    def _emit_r(self, record: logging.LogRecord) -> None:
        try:
            r_level = _LEVEL_MAP.get(record.levelno, 'INFO')
            self._log_event(r_level, self.format(record))
        except Exception:
            self.handleError(record)

    def drain(self) -> None:
        """Flush queued worker-thread records to R. Must be called from the main thread."""
        while True:
            try:
                self._emit_r(self._queue.get_nowait())
            except queue.Empty:
                break


def get_logger(name: str | None = None) -> logging.Logger:
    """Return the package logger, or a child of it (pass ``__name__`` from a module)."""
    return logging.getLogger(name or _LOGGER_NAME)


def configure_logging(
    level: int = logging.INFO,
    log_event: Optional[Callable[[str, str], None]] = None,
) -> logging.Logger:
    """Configure the package logger; attach an R log_event handler if given. Idempotent."""
    global _active_handler
    lg = logging.getLogger(_LOGGER_NAME)
    lg.setLevel(level)
    lg.propagate = False

    # Remove any stale R handler from a previous call.
    lg.handlers = [h for h in lg.handlers if not isinstance(h, _RLogHandler)]
    _active_handler = None

    if log_event is not None:
        _active_handler = _RLogHandler(log_event)
        _active_handler.setFormatter(logging.Formatter("[%(name)s] %(message)s"))
        lg.addHandler(_active_handler)

    return lg


def drain_thread_logs() -> None:
    """Flush worker-thread log records to R; call from the main thread after each graph.stream() chunk."""
    if _active_handler is not None:
        _active_handler.drain()
